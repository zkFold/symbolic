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
import ZkFold.ArithmeticCircuit.Elem (Elem, compileV2)
import ZkFold.Data.Eq ((==))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable (..), hash, preimage)
import ZkFold.Symbolic.V2 (Symbolic)

instance
  Symbolic c
  => Hashable (CompatData FieldElement c) (CompatData FieldElement c)
  where
  hasher _ = zero

hashTest :: forall c. Symbolic c => CompatData FieldElement c -> CompatData Bool c
hashTest e = preimage @(CompatData FieldElement) (hash e) == e

specHash' :: forall a. (Arbitrary a, Arithmetic a, Binary a, Show a) => Spec
specHash' = describe "Hash spec" $ prop "Preimage works fine" $ \x ->
  eval (compileV2 @a (\i -> (i :*: U1) :*: U1) $ hashTest @(Elem a)) (Par1 x)
    Haskell.== Par1 one :*: U1

specHash :: Spec
specHash = specHash' @(Zp BLS12_381_Scalar)
