module Generator
    ( FuncInfo(..)
    , Address(..)
    , Key
    , Iv
    , interpretErrorToText
    , toKey
    , toIv
    , generateAddRoundKey
    , generateGetIv
    ) where

import           Control.Monad              (forM, forM_, liftM, replicateM, when)
import           Control.Monad.ST           (ST, runST)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (Except, runExcept, throwE)
import           Control.Monad.Trans.State  (StateT, execStateT, gets, modify')
import           Data.Array                 (Array, array, (!))
import           Data.Array.ST              (STArray, newListArray, readArray, writeArray)
import           Data.Bits                  (xor)
import           Data.List                  (delete)
import           Data.STRef                 (newSTRef, readSTRef, writeSTRef)
import           Data.Sequence              (Seq, (|>))
import           Data.Word                  (Word32, Word64)
import           Prelude                    hiding (gcd, round)
import           System.Random              (Random, StdGen)

import           Common                     (Address (..), AesState (..), Iv, Key,
                                             Round (..), getIv, getKeyRoundState, toIv,
                                             toKey, xorAesStates)
import           Expr
import           Interpreter                hiding (State, lookupAesState, lookupVariable,
                                             runInterpreter)
import           Printer                    (AddRoundKeyCode (..), GetIvCode (..))

import qualified Data.Sequence              as Seq
import qualified Data.Text                  as Text
import qualified System.Random              as Random

import qualified Interpreter

data FuncInfo = FuncInfo
    { funcAddress :: !Address
    , funcSize    :: !Word32
    , funcKey     :: !Key
    , funcIv      :: !Iv
    }

type GenM a = StateT GenState (Except InterpretError) a

data GenState = GenState
    { stGen           :: !StdGen
    , stStmts         :: !(Seq Stmt)
    , stNextVar       :: !Int
    , stGuardVars     :: !(Array Int Expr)
    , stAddrGuardVars :: ![Expr]
    , stShiftVar      :: !Var
    , stSizeVar       :: !Var
    }

initAesState :: AesState
initAesState = AesState 0xa749220 0xbe93bfa6 0x852f3eee 0xde9a4ae0

rounds :: [Round]
rounds = [Round 0..Round 14]

generateAddRoundKey :: StdGen
                    -> [FuncInfo]
                    -> Either InterpretError (AddRoundKeyCode Seq, StdGen)
generateAddRoundKey = runGenerator generateAddRoundKeyM AddRoundKeyCode

generateGetIv :: StdGen -> [FuncInfo] -> Either InterpretError (GetIvCode Seq, StdGen)
generateGetIv = runGenerator generateGetIvM GetIvCode

runGenerator :: ([FuncInfo] -> GenM ())
             -> (Seq Stmt -> a)
             -> StdGen
             -> [FuncInfo]
             -> Either InterpretError (a, StdGen)
runGenerator run toRes gen funcs = case finalStateOrError of
    Left err         -> Left err
    Right finalState -> Right (toRes $ stStmts finalState, stGen finalState)
  where finalStateOrError = runExcept $ execStateT (run funcs) initState
        initState = GenState { stGen           = gen
                             , stStmts         = Seq.empty
                             , stNextVar       = 0
                             , stGuardVars     = array (0, 0) []
                             , stAddrGuardVars = []
                             , stShiftVar      = VarName "undefined"
                             , stSizeVar       = VarName "undefined"
                             }

generateAddRoundKeyM :: [FuncInfo] -> GenM ()
generateAddRoundKeyM funcs = do
    shuffledFuncs <- shuffle funcs
    defineGuardVars shuffledFuncs
    defineShiftVar
    xorInitState

    forM_ shuffledFuncs $ \func -> do
        roundsOrder <- shuffle rounds
        generateAddRoundKeyForFunction func roundsOrder
  where defineGuardVars :: [FuncInfo] -> GenM ()
        defineGuardVars shuffledFuncs = do
            guardVars <- forM rounds $ \(Round round) -> do
                guardVar <- getNextVar
                define Uint32 guardVar (Binop Xor (Var VarRound) (Int32 round))
                pure $ Var guardVar

            addrGuardVars <- defineAddrGuardVars shuffledFuncs

            let guardVarsArray = array (0, length rounds - 1) (zip [0..] guardVars)

            modify' $ \s -> s { stGuardVars     = guardVarsArray
                              , stAddrGuardVars = addrGuardVars
                              }

        defineShiftVar :: GenM ()
        defineShiftVar = do
            let shiftVar = VarName "shift"
            define Uint64 shiftVar (Int64 0)
            modify' (\s -> s { stShiftVar = shiftVar })

        xorInitState :: GenM ()
        xorInitState = forM_ [0..3] $ \stateIdx -> do
            val <- Int32 <$> randomR (0x10000000, 0xffffffff)
            assign (VarState stateIdx) (Binop Xor (Var $ VarState stateIdx) val)

-- Rounds - is rounds sequence.
-- For example [3, 9, 13, ...].
-- So, first block of code will calculate AddRoundKey for 3 round, next blocks of code
-- does not change state if round is 3, but changes it for all other rounds.
-- After block for calculation AddRoundKey for 3 round, will be blocks of code
-- that calculates AddRoundKey for 9 round, 13 round and so on.
generateAddRoundKeyForFunction :: FuncInfo -> [Round] -> GenM ()
generateAddRoundKeyForFunction func roundsOrder = do
    addrGuardExpr <- getAddrGuardExpr
    generateRounds roundsOrder addrGuardExpr
  where generateRounds :: [Round] -> Expr -> GenM ()
        generateRounds (rnd:rs) addrGuardExpr = do
            generateRound func rnd rs addrGuardExpr
            generateRounds rs addrGuardExpr
        generateRounds _ _ = pure ()

generateRound :: FuncInfo -> Round -> [Round] -> Expr -> GenM ()
generateRound FuncInfo{..} rnd nextRnds addrGuardExpr = do
    AesState c1 c2 c3 c4 <- getCurAesState
    shiftVar <- gets stShiftVar
    roundGuardExpr <- getRoundGuardExpr
    forM_ (zip3 [0..] [c1, c2, c3, c4] [d1, d2, d3, d4]) $ \(idx, c, d) -> do
        let destWord = c `xor` d
        exprForXor <- randomExprForXor rnd True destWord
        assign shiftVar exprForXor
        shiftByGuard shiftVar roundGuardExpr
        shiftByGuard shiftVar addrGuardExpr
        assign (VarState idx) (Binop Xor (Var $ VarState idx) (Var $ shiftVar))
  where AesState d1 d2 d3 d4 = initAesState `xorAesStates` getKeyRoundState funcKey rnd

        getCurAesState :: GenM AesState
        getCurAesState = runInterpreter funcAddress rnd >>= lookupAesState

        getRoundGuardExpr :: GenM Expr
        getRoundGuardExpr = do
            guardVars <- gets stGuardVars
            let roundToExpr (Round rndIdx) = guardVars ! (fromIntegral rndIdx)
            let guardExprs = map roundToExpr (rnd:nextRnds)
            pure $ Guard $ foldl1 (\e1 e2 -> Binop Mul e1 e2) guardExprs

generateGetIvM :: [FuncInfo] -> GenM ()
generateGetIvM funcs = do
    shuffledFuncs <- shuffle funcs
    setInitState
    defineShiftVar
    defineSizeVar
    addrGuardVars <- defineAddrGuardVars shuffledFuncs
    modify' (\s -> s { stAddrGuardVars = addrGuardVars })
    forM_ shuffledFuncs generateGetIvForFunction
  where setInitState :: GenM ()
        setInitState = forM_ [0..3] $ \stateIdx -> do
            val <- Int32 <$> randomR (0x10000000, 0xffffffff)
            assign (VarState stateIdx) val

        defineShiftVar :: GenM ()
        defineShiftVar = do
            let shiftVar = VarName "shift"
            define Uint64 shiftVar (Int64 0)
            modify' (\s -> s { stShiftVar = shiftVar })

        defineSizeVar :: GenM ()
        defineSizeVar = do
            val <- Int32 <$> randomR (0x10000000, 0xffffffff)
            let sizeVar = VarName "size"
            define Uint32 sizeVar val
            modify' (\s -> s { stSizeVar = sizeVar })

generateGetIvForFunction :: FuncInfo -> GenM ()
generateGetIvForFunction FuncInfo{..} = do
    shiftVar <- gets stShiftVar
    sizeVar <- gets stSizeVar
    addrGuardExpr <- getAddrGuardExpr
    (AesState c1 c2 c3 c4, size) <- getCurAesStateAndSize sizeVar
    let destWordSize = size `xor` funcSize
    exprForXorSize <- randomExprForXor rnd False destWordSize
    assign shiftVar exprForXorSize
    shiftByGuard shiftVar addrGuardExpr
    assign sizeVar (Binop Xor (Var sizeVar) (Var shiftVar))

    forM_ (zip3 [0..] [c1, c2, c3, c4] [d1, d2, d3, d4]) $ \(idx, c, d) -> do
        let destWord = c `xor` d
        exprForXor <- randomExprForXor rnd False destWord
        assign shiftVar exprForXor
        shiftByGuard shiftVar addrGuardExpr
        assign (VarState idx) (Binop Xor (Var $ VarState idx) (Var shiftVar))
  where AesState d1 d2 d3 d4 = getIv funcIv
        rnd = Round 0

        getCurAesStateAndSize :: Var -> GenM (AesState, Word32)
        getCurAesStateAndSize sizeVar = do
            state <- runInterpreter funcAddress rnd
            aesState <- lookupAesState state
            size <- lookupVariable sizeVar state >>= \case
                Val32 v32 -> pure v32
                Val64 v64 -> pure (fromIntegral v64)
            pure (aesState, size)

defineAddrGuardVars :: [FuncInfo] -> GenM [Expr]
defineAddrGuardVars funcs = forM funcs $ \FuncInfo{..} -> do
    guardVar <- getNextVar
    let Address begin = funcAddress
    let beginExpr = Int64 begin
    let end = begin + fromIntegral funcSize - 1
    let endExpr = Int64 end
    define Uint32 guardVar (AddressGuard (Var VarAddress) beginExpr endExpr)
    pure $ Var guardVar

getAddrGuardExpr :: GenM Expr
getAddrGuardExpr = do
    addrGuardVars <- gets stAddrGuardVars
    modify' (\s -> s { stAddrGuardVars = tail addrGuardVars })
    pure $ Guard $ foldl1 (\e1 e2 -> Binop Mul e1 e2) addrGuardVars

shiftByGuard :: Var -> Expr -> GenM ()
shiftByGuard shiftVar guardExpr = do
    shiftOp <- randomChoice [Shl, Shr]
    when (shiftOp == Shr) $
        assign shiftVar (Binop And (Var shiftVar) (Int64 0xffffffff))
    assign shiftVar (Binop shiftOp (Var shiftVar) guardExpr)

define :: Type -> Var -> Expr -> GenM ()
define typ var expr = modify' (\s@GenState{..} -> s { stStmts = stStmts |> stmt })
  where stmt = Define typ var expr

assign :: Var -> Expr -> GenM ()
assign var expr = modify' (\s@GenState{..} -> s { stStmts = stStmts |> stmt })
  where stmt = Assign var expr

getNextVar :: GenM Var
getNextVar = do
    idx <- gets stNextVar
    modify' (\s -> s { stNextVar = idx + 1 })
    pure (VarName $ "var" <> Text.pack (show idx))

randomR :: Random a => (a, a) -> GenM a
randomR rng = applyGen (Random.randomR rng)

random :: Random a => GenM a
random = applyGen Random.random

randomChoice :: [a] -> GenM a
randomChoice xs = randomR (0, length xs - 1) >>= \idx -> pure (xs !! idx)

shuffle :: [a] -> GenM [a]
shuffle xs = applyGen shuffle'
  where shuffle' gen = runST $ do
            g <- newSTRef gen
            let randomRST lohi = do
                    (a, s') <- liftM (Random.randomR lohi) (readSTRef g)
                    writeSTRef g s'
                    pure a
            ar <- newArray xs
            xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                pure vj
            gen' <- readSTRef g
            pure (xs', gen')
          where n = length xs
                newArray :: [a] -> ST s (STArray s Int a)
                newArray = newListArray (1, n)

-- first operation should not be add, to increase round influence.
-- last operation should not be shl, rotl or rotr, because they have not
-- reciprocal operations
randomExprForXor :: Round -> Bool -> Word32 -> GenM Expr
randomExprForXor rnd withRound destWord = do
    numOps <- randomR (2, 5)
    ops <- replicateM (numOps - 1) (randomChoice [Add, Mul, Rotl, Rotr, Xor])
    partialExpr <- randomPartialExpr ops
    partialResult <- runExpr32 rnd partialExpr
    (lastOp, lastOperand) <- randomLastOperand partialResult [Add, Mul, Xor]
    pure (Binop lastOp partialExpr lastOperand)
  where randomPartialExpr :: [Binop] -> GenM Expr
        randomPartialExpr ops@(op : _) = do
            (lOperand, isRoundUsed) <- randomOperand op (not withRound)
            randomPartialExpr' ops isRoundUsed lOperand
        randomPartialExpr _ = error "Never executed"

        randomPartialExpr' :: [Binop] -> Bool -> Expr -> GenM Expr
        randomPartialExpr' (op:ops@(_:_)) isRoundUsed lOperand = do
            (rOperand, isRoundUsed') <- randomOperand op isRoundUsed
            randomPartialExpr' ops isRoundUsed' (Binop op lOperand rOperand)
        randomPartialExpr' [op] isRoundUsed lOperand
          | isRoundUsed = Binop op lOperand . fst <$> randomOperand op isRoundUsed
          | otherwise   = pure $ Binop op lOperand (Var VarRound)
        randomPartialExpr' _ _ _ = error "Never executed"

        randomLastOperand :: Word32 -> [Binop] -> GenM (Binop, Expr)
        randomLastOperand curWord ops = randomChoice ops >>= \case
            Mul
              | Just term <- getMultTerm curWord destWord
              -> pure (Mul, Int32 term)
              | otherwise
              -> randomLastOperand curWord (delete Mul ops)
            Add -> pure (Add, Int32 (destWord - curWord))
            _   -> pure (Xor, Int32 (destWord `xor` curWord))

        randomOperand :: Binop -> Bool -> GenM (Expr, Bool)
        randomOperand op isRoundUsed = do
            useRound <- if isRoundUsed then pure False else random
            if useRound
               then pure (Var VarRound, True)
               else do
                   val <- case op of
                       Rotl -> randomR (5, 31)
                       Rotr -> randomR (5, 31)
                       _    -> randomR (0x10000000, 0xffffffff)
                   pure (Int32 val, isRoundUsed)

        -- from * getMultTerm from to = to
        getMultTerm :: Word32 -> Word32 -> Maybe Word32
        getMultTerm from to = (*to) <$> getReciprocal from

        getReciprocal :: Word32 -> Maybe Word32
        getReciprocal a
          | g /= 1    = Nothing
          | otherwise = Just $ fromIntegral $ (x `mod` m + m) `mod` m
          where (g, x, _) = gcd (fromIntegral a) m
                m = 0x100000000

        gcd :: Word64 -> Word64 -> (Word64, Word64, Word64)
        gcd a b
          | a == 0    = (b, 0, 1)
          | otherwise = (d, y - (b `div` a) * x, x)
          where (d, x, y) = gcd (b `mod` a) a

applyGen :: (StdGen -> (a, StdGen)) -> GenM a
applyGen f = do
    (res, newGen) <- f <$> gets stGen
    modify' (\s -> s { stGen = newGen })
    pure res

runInterpreter :: Address -> Round -> GenM Interpreter.State
runInterpreter addr rnd = do
    stmts <- gets stStmts
    case (Interpreter.runInterpreter initAesState addr rnd stmts) of
        Left err  -> lift $ throwE err
        Right res -> pure res

lookupAesState :: Interpreter.State -> GenM AesState
lookupAesState state = case Interpreter.lookupAesState state of
    Left err       -> lift $ throwE err
    Right aesState -> pure aesState

lookupVariable :: Var -> Interpreter.State -> GenM Value
lookupVariable var state = case Interpreter.lookupVariable var state of
    Left err  -> lift $ throwE err
    Right val -> pure val

runExpr32 :: Round -> Expr -> GenM Word32
runExpr32 rnd expr = runExpr rnd expr >>= \case
    Val32 w32 -> pure w32
    Val64 w64 -> pure (fromIntegral w64)

runExpr :: Round -> Expr -> GenM Value
runExpr rnd expr = case runInterpreterExpr rnd expr of
    Left err  -> lift $ throwE err
    Right res -> pure res
