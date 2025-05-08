{-# LANGUAGE TypeApplications #-}

module Tests.Protocol.Plonkup.Update (specPlonkupUpdate) where

import           Data.Bool                                           (Bool (..))
import           Data.ByteString                                     (ByteString)
import           Data.Foldable                                       (Foldable)
import           Data.Function                                       (($))
import           Data.Functor.Rep                                    (Rep, Representable)
import           Data.Ord                                            (Ord)
import           GHC.Generics                                        (Par1 (..), type (:*:) (..))
import           Test.Hspec
import           Test.QuickCheck                                     hiding (Witness, witness)

import           ZkFold.Algebra.EllipticCurve.BLS12_381              (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Algebra.EllipticCurve.Class                  (CyclicGroup (..))
import           ZkFold.Algebra.Number                               (KnownNat)
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Data.Vector                                  (Vector)
import           ZkFold.Protocol.Plonkup                             hiding (omega)
import           ZkFold.Protocol.Plonkup.Witness                     (witnessInput)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import ZkFold.Protocol.NonInteractiveProof (NonInteractiveProof(..))
import ZkFold.Protocol.Plonkup.Update (updateProverSetup, nextGroupElement, updateVerifierSetup)
import Prelude (fst)

type P i o n = Plonkup i o Par1 n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) 

propUpdateSetupIsCorrect ::
    forall i o n . (KnownNat n, Representable i, Representable o, Foldable o, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n))
    => P i o n -> Witness (P i o n) -> Bool
propUpdateSetupIsCorrect plonkup witness =
    let pi = witnessInput $ fst witness
        _ :*: par = eval (ac plonkup) pi
        
        setupP  = setupProve plonkup
        setupP' = updateProverSetup setupP par
        (input, proof) = prove @(P i o n) setupP' witness

        h = nextGroupElement setupP
        setupV = setupVerify plonkup
        setupV' = updateVerifierSetup setupV par (Par1 h)
    in verify @(P i o n) setupV' input proof

specPlonkupUpdate :: Spec
specPlonkupUpdate = do
    describe "Setup" $ do
        it "is updated correctly" $ property $ withMaxSuccess 10 $ propUpdateSetupIsCorrect @(Vector 2) @Par1 @32
