{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Hash (specHash) where

import Data.Binary (Binary)
import qualified Data.Eq as Haskell
import Data.Function (($))
import GHC.Generics (Par1 (Par1), U1 (..), type (:*:) (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)
import Text.Show (Show)

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.ArithmeticCircuit (eval)
import ZkFold.ArithmeticCircuit.Elem (Elem, compile)
import ZkFold.Data.Eq ((==))
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash, preimage)

instance Symbolic c => Hashable (FieldElement c) (FieldElement c) where
  hasher _ = zero

hashTest :: Symbolic c => FieldElement c -> Bool c
hashTest e = preimage @FieldElement (hash e) == e

specHash' :: forall a. (Arbitrary a, Arithmetic a, Binary a, Show a) => Spec
specHash' = describe "Hash spec" $ prop "Preimage works fine" $ \x ->
  eval (compile @a (:*: U1) $ hashTest @(Elem a)) (Par1 x)
    Haskell.== Par1 one

specHash :: Spec
specHash = specHash' @(Zp BLS12_381_Scalar)
