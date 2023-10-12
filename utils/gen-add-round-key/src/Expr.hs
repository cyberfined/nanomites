module Expr
    ( Stmt(..)
    , Expr(..)
    , Binop(..)
    , Type(..)
    , Var(..)
    , varToText
    ) where

import           Data.Hashable (Hashable (..))
import           Data.Text     (Text)
import           Data.Word     (Word32, Word64)
import           System.Random (Random, random, randomR)

import qualified Data.Text     as Text

data Stmt
    = Define !Type !Var !Expr
    | Assign !Var !Expr

data Expr
    = Var !Var
    | Int32 !Word32
    | Int64 !Word64
    | Binop !Binop !Expr !Expr
    | Guard !Expr
    | AddressGuard !Expr !Expr !Expr

data Var
    = VarName !Text
    | VarState !Int
    | VarAddress
    | VarRound
    deriving stock Eq

instance Hashable Var where
    hashWithSalt s (VarName name) = hashWithSalt s name
    hashWithSalt s (VarState idx) = hashWithSalt s idx
    hashWithSalt s VarAddress     = hashWithSalt s (Text.pack "address")
    hashWithSalt s VarRound       = hashWithSalt s (Text.pack "round")

varToText :: Var -> Text
varToText = \case
    VarName name -> name
    VarState idx -> "state[" <> Text.pack (show idx) <> "]"
    VarAddress   -> "address"
    VarRound     -> "round"

data Binop
    = Add
    | Mul
    | Shl
    | Shr
    | Rotl
    | Rotr
    | Xor
    | And
    deriving stock (Eq, Bounded, Enum)

instance Random Binop where
    random g = (toEnum idx, newG)
      where (idx, newG) = randomR (fromIdx, toIdx) g
            fromIdx = fromEnum (minBound :: Binop)
            toIdx = fromEnum (maxBound :: Binop)
    randomR (from, to) g = (toEnum idx, newG)
      where (idx, newG) = randomR (fromIdx, toIdx) g
            fromIdx = fromEnum from
            toIdx = fromEnum to

data Type
    = Uint32
    | Uint64
