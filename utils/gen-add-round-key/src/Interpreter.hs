module Interpreter
    ( InterpretError(..)
    , State
    , Value(..)
    , interpretErrorToText
    , runInterpreter
    , runInterpreterExpr
    , lookupAesState
    , lookupVariable
    ) where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (Except, runExcept, throwE)
import           Control.Monad.Trans.State  (StateT, evalStateT, execStateT, gets,
                                             modify')
import           Data.Bits                  (rotateL, rotateR, shiftL, shiftR, xor, (.&.))
import           Data.HashMap.Strict        (HashMap)
import           Data.Text                  (Text)
import           Data.Word                  (Word32, Word64)

import           Common                     (Address (..), AesState (..), Round (..))
import           Expr

import qualified Data.HashMap.Strict        as HashMap

newtype State = State { stVariables :: HashMap Var Value }

data Value
    = Val32 !Word32
    | Val64 !Word64

data InterpretError
    = UndefinedVariable !Var
    | WrongStateType !Int

interpretErrorToText :: InterpretError -> Text
interpretErrorToText = \case
    UndefinedVariable var -> "Undefined variable " <> (varToText var)
    WrongStateType idx    -> "Wrong type: "
                          <> varToText (VarState idx)
                          <> " must be 32-bit integer"

type InterpretM a = StateT State (Except InterpretError) a

runInterpreter :: Traversable t
               => AesState
               -> Address
               -> Round
               -> t Stmt
               -> Either InterpretError State
runInterpreter (AesState w1 w2 w3 w4) (Address addr) (Round rnd) stmts = result
  where result = runExcept (execStateT (runInterpreterM stmts) initState)
        initState = State initVariables
        initVariables = HashMap.fromList [ (VarState 0, Val32 w1)
                                         , (VarState 1, Val32 w2)
                                         , (VarState 2, Val32 w3)
                                         , (VarState 3, Val32 w4)
                                         , (VarAddress, Val64 addr)
                                         , (VarRound, Val32 rnd)
                                         ]

runInterpreterM :: Traversable t => t Stmt -> InterpretM ()
runInterpreterM = mapM_ runStmt

runInterpreterExpr :: Maybe AesState
                   -> Maybe Address
                   -> Maybe Round
                   -> Expr
                   -> Either InterpretError Value
runInterpreterExpr mAesState mAddr mRnd expr =
    runExcept (evalStateT (runExpr expr) initState)
  where initState = State initVariables
        initVariables = HashMap.fromList $  aesStateVariables
                                         ++ addrVariable
                                         ++ roundVariable
        aesStateVariables = case mAesState of
            Nothing -> []
            Just (AesState w1 w2 w3 w4) ->
                let f idx w = (VarState idx, Val32 w)
                in zipWith f [0..] [w1, w2, w3, w4]
        addrVariable = maybe [] (\(Address addr) -> [(VarAddress, Val64 addr)]) mAddr
        roundVariable = maybe [] (\(Round rnd) -> [(VarRound, Val32 rnd)]) mRnd

runStmt :: Stmt -> InterpretM ()
runStmt = \case
    Define typ var expr -> runExpr expr >>= setVariable var . castToType typ
    Assign var expr     -> do
        typ <- getVarType <$> getVariable var
        val <- castToType typ <$> runExpr expr
        setVariable var val
  where castToType :: Type -> Value -> Value
        castToType Uint32 v@(Val32 _) = v
        castToType Uint64 v@(Val64 _) = v
        castToType Uint32 (Val64 v)   = Val32 (fromIntegral v)
        castToType Uint64 (Val32 v)   = Val64 (fromIntegral v)

        getVarType :: Value -> Type
        getVarType = \case
            Val32 _ -> Uint32
            Val64 _ -> Uint64

runExpr :: Expr -> InterpretM Value
runExpr = \case
    Var var                     -> getVariable var
    Int32 i32                   -> pure (Val32 i32)
    Int64 i64                   -> pure (Val64 i64)
    Binop op e1 e2              -> runBinop op <$> runExpr e1 <*> runExpr e2
    Guard e                     -> runGuard <$> runExpr e
    AddressGuard addr begin end ->  runAddressGuard
                                <$> runExpr addr
                                <*> runExpr begin
                                <*> runExpr end
  where runBinop :: Binop -> Value -> Value -> Value
        runBinop op (Val32 v1) (Val32 v2) = Val32 (runBinop32 op v1 v2)
        runBinop op (Val64 v1) (Val64 v2) = Val64 (runBinop64 op v1 v2)
        runBinop op (Val32 v1) (Val64 v2) = Val64 (runBinop64 op (fromIntegral v1) v2)
        runBinop op (Val64 v1) (Val32 v2) = Val64 (runBinop64 op v1 (fromIntegral v2))

        runBinop32 :: Binop -> Word32 -> Word32 -> Word32
        runBinop32 op v1 v2 = case op of
            Add  -> v1 + v2
            Mul  -> v1 * v2
            Shl  -> shiftL v1 (fromIntegral v2)
            Shr  -> shiftR v1 (fromIntegral v2)
            Rotl -> rotateL v1 (fromIntegral v2)
            Rotr -> rotateR v1 (fromIntegral v2)
            Xor  -> v1 `xor` v2
            And  -> v1 .&. v2

        runBinop64 :: Binop -> Word64 -> Word64 -> Word64
        runBinop64 op v1 v2 = case op of
            Add  -> v1 + v2
            Mul  -> v1 * v2
            Shl  -> shiftL v1 (fromIntegral v2)
            Shr  -> shiftR v1 (fromIntegral v2)
            Rotl -> rotateL v1 (fromIntegral v2)
            Rotr -> rotateR v1 (fromIntegral v2)
            Xor  -> v1 `xor` v2
            And  -> v1 .&. v2

        runGuard :: Value -> Value
        runGuard (Val32 v) = Val32 (if v == 0 then 0 else 32)
        runGuard (Val64 v) = Val32 (if v == 0 then 0 else 32)

        runAddressGuard :: Value -> Value -> Value -> Value
        runAddressGuard addr begin end
          | addrGeBegin && addrLeEnd = Val32 0
          | otherwise                = Val32 33
          where addrGeBegin = case (addr, begin) of
                    (Val32 a32, Val32 b32) -> a32 >= b32
                    (Val64 a64, Val64 b64) -> a64 >= b64
                    (Val32 a32, Val64 b64) -> fromIntegral a32 >= b64
                    (Val64 a64, Val32 b32) -> a64 >= fromIntegral b32

                addrLeEnd = case (addr, end) of
                    (Val32 a32, Val32 e32) -> a32 <= e32
                    (Val64 a64, Val64 e64) -> a64 <= e64
                    (Val32 a32, Val64 e64) -> fromIntegral a32 <= e64
                    (Val64 a64, Val32 e32) -> a64 <= fromIntegral e32

lookupAesState :: State -> Either InterpretError AesState
lookupAesState (State variables) =  AesState
                                <$> lookupStateOrError 0
                                <*> lookupStateOrError 1
                                <*> lookupStateOrError 2
                                <*> lookupStateOrError 3
  where lookupStateOrError :: Int -> Either InterpretError Word32
        lookupStateOrError stateIdx =
            case HashMap.lookup var variables of
                Nothing        -> Left (UndefinedVariable var)
                Just (Val64 _) -> Left (WrongStateType stateIdx)
                Just (Val32 w) -> Right w
          where var = VarState stateIdx

lookupVariable :: Var -> State -> Either InterpretError Value
lookupVariable var (State variables) = case HashMap.lookup var variables of
    Nothing  -> Left (UndefinedVariable var)
    Just val -> Right val

getVariable :: Var -> InterpretM Value
getVariable var = gets (\State {..} -> HashMap.lookup var stVariables) >>= \case
    Nothing  -> lift $ throwE $ UndefinedVariable var
    Just val -> pure val

setVariable :: Var -> Value -> InterpretM ()
setVariable var val = modify' $
    \s@(State {..}) -> s { stVariables = HashMap.insert var val stVariables }
