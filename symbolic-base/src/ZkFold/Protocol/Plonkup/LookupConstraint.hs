{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.LookupConstraint where

import           Data.Binary                                         (Binary)
import           Data.ByteString                                     (ByteString)
import           Data.Functor.Rep                                    (Rep)
import           Prelude                                             hiding (Num (..), drop, length, sum, take, (!!),
                                                                      (/), (^))
import           Test.QuickCheck                                     (Arbitrary (..))

import           ZkFold.Data.ByteString                              (toByteString)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal

newtype LookupConstraint i a = LookupConstraint { lkVar :: SysVar i }

deriving instance Show (Rep i) => Show (LookupConstraint i a)
deriving instance Eq (Rep i) => Eq (LookupConstraint i a)

instance (Arbitrary a, Binary a) => Arbitrary (LookupConstraint i a) where
    arbitrary = LookupConstraint . NewVar . EqVar . toByteString @a <$> arbitrary

toLookupConstraint :: forall a i . ByteString -> LookupConstraint i a
toLookupConstraint = LookupConstraint . NewVar . EqVar
