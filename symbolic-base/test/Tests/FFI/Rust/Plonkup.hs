{-# LANGUAGE TypeOperators #-}

module Tests.FFI.Rust.Plonkup where

import Data.ByteString (ByteString)
import GHC.Generics (
  Par1 (..),
 )
import Test.Hspec
import Test.QuickCheck hiding (scale, witness)
import Prelude hiding (
  Fractional (..),
  Num (..),
  length,
  (^),
 )

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Number (
  type (^),
 )
import qualified ZkFold.Algebra.Number as Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (..))
import ZkFold.FFI.Rust.Plonkup (rustPlonkupProve)
import ZkFold.Protocol.NonInteractiveProof as NP (
  NonInteractiveProof (..),
 )
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Utils (
  getParams,
  getSecretParams,
 )
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))

type TestCircuitSize = 2 ^ 6

type PlonkupTs i n t = Plonkup i Par1 n BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint t (PolyVec Fr)

specRustPlonkup :: Spec
specRustPlonkup = do
  describe "Rust Plonkup prover specification" $ do
    it "should be equal to Haskell prover" $ do
      property $
        \(x :: Fr)
         (ps :: PlonkupProverSecret BLS12_381_G1_JacobianPoint)
         (a :: ArithmeticCircuit Fr Par1 Par1) ->
            let
              (omega, k1, k2) = getParams (Number.value @TestCircuitSize)
              (gs, h1) = getSecretParams @TestCircuitSize @BLS12_381_G1_JacobianPoint @BLS12_381_G2_JacobianPoint x
              plonkup = Plonkup omega k1 k2 a h1 gs
              setupP = setupProve @(PlonkupTs Par1 TestCircuitSize ByteString) plonkup
              witness = (PlonkupWitnessInput @Par1 @BLS12_381_G1_JacobianPoint (Par1 zero), ps)
              (proofRust, _) = rustPlonkupProve setupP witness
              (_, proofHs) = prove @(PlonkupTs Par1 TestCircuitSize ByteString) setupP witness
             in
              proofHs == proofRust
