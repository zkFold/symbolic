module ZkFold.Protocol.Plonkup.LookupConstraint where

import Control.Applicative ((<*>))
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor ((<$>))
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)

import ZkFold.Algebra.Class (Semiring)
import ZkFold.ArithmeticCircuit.Var (Var, toVar)
import ZkFold.Data.Binary (toByteString)

data LookupConstraint i a = LookupConstraint
  { lkVar1 :: Var a
  , lkVar2 :: Var a
  , lkVar3 :: Var a
  }
  deriving (Eq, Show)

instance (Arbitrary a, Binary a, Semiring a) => Arbitrary (LookupConstraint i a) where
  arbitrary =
    let var = toVar . toByteString @a <$> arbitrary
     in LookupConstraint <$> var <*> var <*> var

toLookupConstraint
  :: Semiring a => ByteString -> ByteString -> ByteString -> LookupConstraint i a
toLookupConstraint i j k = LookupConstraint (toVar i) (toVar j) (toVar k)
