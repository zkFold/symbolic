{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.ArithmeticCircuit.MerkleHash where

import Crypto.Hash.SHA256 (hash)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.Function ((.))
import Data.Maybe (Maybe (..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude (Integer, error)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Binary (toByteString)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.MonadCircuit (ResidueField (..))

newtype MerkleHash (n :: Maybe Natural) = M {runHash :: ByteString}

data Prec
  = If
  | Or
  | Eq
  | Neq
  | Add
  | Mul
  | Div
  | Mod
  | Gcd
  | BezoutL
  | BezoutR
  | Exp
  | Const
  deriving (Binary, Generic)

merkleHash :: Binary a => a -> MerkleHash n
merkleHash = M . hash . toByteString

instance Binary (MerkleHash n) where
  get = error "undefined"
  put = put . runHash

instance Finite (Zp n) => Finite (MerkleHash (Just n)) where
  type Order (MerkleHash (Just n)) = n

instance {-# OVERLAPPING #-} FromConstant (MerkleHash n) (MerkleHash n)

instance {-# OVERLAPPING #-} Scale (MerkleHash n) (MerkleHash n)

instance Binary a => FromConstant a (MerkleHash n) where
  fromConstant x = merkleHash (Const, x)

instance Binary a => Scale a (MerkleHash n)

instance Exponent (MerkleHash n) Natural where
  M h ^ p = merkleHash (Exp, h, hash (toByteString p))

instance MultiplicativeSemigroup (MerkleHash n) where
  M x * M y = merkleHash (Mul, x, y)

instance MultiplicativeMonoid (MerkleHash n) where
  one = fromConstant (one :: Natural)

instance AdditiveSemigroup (MerkleHash n) where
  M x + M y = merkleHash (Add, x, y)

instance AdditiveMonoid (MerkleHash n) where
  zero = fromConstant (zero :: Natural)

instance Semiring (MerkleHash n)

instance AdditiveGroup (MerkleHash n) where
  negate (M x) = merkleHash (Add, x)

instance Ring (MerkleHash n)

instance Exponent (MerkleHash n) Integer where
  M h ^ p = merkleHash (Exp, h, hash (toByteString p))

instance Conditional (MerkleHash (Just 2)) (MerkleHash n) where
  bool (M f) (M t) (M b) = merkleHash (If, f, t, b)

instance Eq (MerkleHash n) where
  type BooleanOf (MerkleHash n) = MerkleHash (Just 2)
  M x == M y = merkleHash (Eq, x, y)
  M x /= M y = merkleHash (Neq, x, y)

instance Field (MerkleHash (Just n)) where
  finv (M x) = merkleHash (Mul, x)

instance Finite (Zp n) => ResidueField (MerkleHash (Just n)) where
  type IntegralOf (MerkleHash (Just n)) = MerkleHash Nothing
  fromIntegral = fromConstant
  toIntegral = merkleHash

instance BoolType (MerkleHash (Just 2)) where
  true = fromConstant (1 :: Natural)
  false = fromConstant (0 :: Natural)
  not = negate
  (&&) = (*)
  M x || M y = merkleHash (Or, x, y)
  xor = (+)

instance SemiEuclidean (MerkleHash Nothing) where
  div (M x) (M y) = merkleHash (Div, x, y)
  mod (M x) (M y) = merkleHash (Mod, x, y)

instance Euclidean (MerkleHash Nothing) where
  gcd (M x) (M y) = merkleHash (Gcd, x, y)
  bezoutL (M x) (M y) = merkleHash (BezoutL, x, y)
  bezoutR (M x) (M y) = merkleHash (BezoutR, x, y)
