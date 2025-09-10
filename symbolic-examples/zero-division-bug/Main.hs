{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString (ByteString)
import Data.Data
import qualified Data.Vector as V
import GHC.Generics
import GHC.TypeLits (KnownNat)
import Test.QuickCheck hiding (scale)
import ZkFold.Algebra.Class hiding ((+))
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number (Natural)
import ZkFold.Algebra.Polynomial.Univariate
import ZkFold.ArithmeticCircuit
import ZkFold.Data.Eq
import ZkFold.Data.Vector
import qualified ZkFold.Data.Vector
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Protocol.Plonkup
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Utils
import ZkFold.Protocol.Plonkup.Witness
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Compiler
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Data.Vec
import ZkFold.Symbolic.Examples.Eq
import Prelude hiding (Bool, (==))

type I = (U1 :*: U1) :*: (Par1 :*: U1)

type G1 = BLS12_381_G1_Point

type G2 = BLS12_381_G2_Point

type PV = (PolyVec (ScalarFieldOf G1))

type PlonkupExample n =
  Plonkup I Par1 n G1 G2 ByteString PV

setupEqualityCheckContract :: SetupProve (PlonkupExample 16)
setupEqualityCheckContract = setupProve plonk
 where
  ac = runVec $ compile (exampleEq (fromConstant (11 :: Natural))) :: ArithmeticCircuit Fr I Par1
  (omega, k1, k2) = getParams 16
  x = fromConstant (5 :: Natural)
  (gs, h1) = getSecretParams x
  plonk = Plonkup omega k1 k2 ac h1 gs :: PlonkupExample 16

main :: IO ()
main = do
  let w = fromConstant (11 :: Natural)
  let paddedW = PlonkupWitnessInput $ (U1 :*: U1) :*: (Par1 w :*: U1)
  let witness =
        ( paddedW
        , PlonkupProverSecret $
            Vector $
              V.generate
                19
                (\i -> fromConstant (toInteger $ i + 1))
        )
  putStrLn $ "Setup: " <> show setupEqualityCheckContract
  putStrLn $ "Witness: " <> show witness

  let (input, proof) =
        prove @(PlonkupExample 16)
          setupEqualityCheckContract
          witness

  putStrLn $ "Proof: " <> show proof
  let z = zero :: Fr
  let zInv = one // z
  putStrLn $ "Zero: " <> show z <> " with inverse " <> show zInv