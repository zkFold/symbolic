{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.LookupConstraint where

import           Data.Binary                                         (Binary)
import           Data.ByteString                                     (ByteString)
import           Data.Functor.Rep                                    (Rep)
import           Prelude                                             hiding (Num (..), drop, length, sum, take, (!!),
                                                                      (/), (^))
import           Test.QuickCheck                                     (Arbitrary (..))

import           ZkFold.Algebra.Class                                (Semiring)
import           ZkFold.Data.ByteString                              (toByteString)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal (NewVar (..), SysVar (..), Var)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var      (toVar)

data LookupConstraint i a = LookupConstraint
    { lkVar1 :: Var a i
    , lkVar2 :: Var a i
    , lkVar3 :: Var a i
    }

deriving instance (Show (Rep i), Show a) => Show (LookupConstraint i a)
deriving instance (Eq (Rep i), Eq a) => Eq (LookupConstraint i a)

instance (Arbitrary a, Binary a, Semiring a) => Arbitrary (LookupConstraint i a) where
    arbitrary =
      let var = toVar . NewVar . EqVar . toByteString @a <$> arbitrary
       in LookupConstraint <$> var <*> var <*> var

toLookupConstraint ::
    Semiring a => ByteString -> ByteString -> ByteString -> LookupConstraint i a
toLookupConstraint i j k =
    let var = toVar . NewVar . EqVar
     in LookupConstraint (var i) (var j) (var k)
