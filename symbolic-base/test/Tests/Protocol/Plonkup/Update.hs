{-# LANGUAGE TypeApplications #-}

module Tests.Protocol.Plonkup.Update (specPlonkupUpdate) where

import           Data.Bool                                           (Bool (..))
import           Data.ByteString                                     (ByteString)
import           Data.Foldable                                       (toList)
import           Data.Function                                       (($))
import           Data.Functor.Rep                                    (Rep, Representable)
import           Data.Ord                                            (Ord)
import           GHC.Generics                                        (Par1 (..))
import           Prelude                                             (fst)
import           Test.Hspec
import           Test.QuickCheck                                     hiding (Witness, witness)

import           ZkFold.Algebra.EllipticCurve.BLS12_381              (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Algebra.EllipticCurve.Class                  (CyclicGroup (..))
import           ZkFold.Algebra.Number                               (KnownNat)
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Data.Vector                                  (Vector)
import           ZkFold.Protocol.NonInteractiveProof                 (NonInteractiveProof (..))
import           ZkFold.Protocol.Plonkup                             (Plonkup(ac), lagrangeBasisGroupElements, PlonkupPolyExtendedLength)
import           ZkFold.Protocol.Plonkup.Update                      (updateProverSetup, updateVerifierSetup)
import           ZkFold.Protocol.Plonkup.Witness                     (witnessInput)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import Data.List (head)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSetup (..))

type P i n = Plonkup i Par1 n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point))

propUpdateSetupIsCorrect ::
    forall i n . (KnownNat n, Representable i, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n))
    => P i n -> Witness (P i n) -> Bool
propUpdateSetupIsCorrect plonkup witness =
    let pi = witnessInput $ fst witness
        par = toList $ eval (ac plonkup) pi

        setupP  = setupProve plonkup
        setupP' = updateProverSetup setupP par
        (input, proof) = prove @(P i n) setupP' witness

        h = head $ lagrangeBasisGroupElements @n @_ @(PolyVec (ScalarFieldOf BLS12_381_G1_Point)) (omega setupP) (gs setupP)
        setupV = setupVerify plonkup
        setupV' = updateVerifierSetup setupV par [h]
    in verify @(P i n) setupV' input proof

specPlonkupUpdate :: Spec
specPlonkupUpdate = do
    describe "Setup" $ do
        it "is updated correctly" $ property $ withMaxSuccess 10 $ propUpdateSetupIsCorrect @(Vector 2) @32
