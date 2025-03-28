{-# LANGUAGE TypeOperators #-}

module Tests.Protocol.IVC (specIVC) where

import           Data.Functor.Constant                       (Constant)
import           GHC.Generics                                (U1 (..), type (:*:) (..))
import           GHC.IsList                                  (IsList (..))
import           Prelude                                     hiding (Num (..), pi, replicate, sum, (+))
import           Test.Hspec                                  (Spec, describe, it)
import           Test.QuickCheck                             (property, withMaxSuccess, (==>))

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..), one, zero)
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.Basic.Number            (Natural, type (-))
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_Scalar)
import           ZkFold.Base.Algebra.Polynomials.Univariate  (PolyVec, evalPolyVec)
import           ZkFold.Base.Data.Vector                     (Vector (..), item, singleton, unsafeToVector)
import           ZkFold.Base.Protocol.IVC.Accumulator        (Accumulator (..), AccumulatorInstance (..),
                                                              emptyAccumulator)
import           ZkFold.Base.Protocol.IVC.AccumulatorScheme  as Acc
import           ZkFold.Base.Protocol.IVC.AlgebraicMap       (algebraicMap)
import           ZkFold.Base.Protocol.IVC.CommitOpen         (commitOpen)
import           ZkFold.Base.Protocol.IVC.FiatShamir         (FiatShamir, fiatShamir)
import           ZkFold.Base.Protocol.IVC.NARK               (NARKInstanceProof (..), NARKProof (..), narkInstanceProof)
import           ZkFold.Base.Protocol.IVC.Oracle             (MiMCHash)
import           ZkFold.Base.Protocol.IVC.Predicate          (Predicate (..), predicate)
import           ZkFold.Base.Protocol.IVC.SpecialSound       (specialSoundProtocol)
import           ZkFold.Prelude                              (replicate)
import           ZkFold.Symbolic.Class                       (BaseField, Symbolic)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, acSizeN)
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement (..))

type F = Zp BLS12_381_Scalar
type C = Constant BLS12_381_G1_Point
type I = Vector 1
type P = U1
type K = 1
type AC = ArithmeticCircuit F (Vector 1 :*: U1) (Vector 1) U1
type PHI = Predicate F I P
type SPS = FiatShamir 1 I P C [F] [F] F
type D = 2
type PARDEG = 5
type PAR = PolyVec F PARDEG

testFunction :: forall ctx . (Symbolic ctx, FromConstant F (BaseField ctx))
    => PAR -> Vector 1 (FieldElement ctx) -> U1 (FieldElement ctx) -> Vector 1 (FieldElement ctx)
testFunction p x _ =
    let p' = fromList $ map fromConstant $ toList p :: PolyVec (FieldElement ctx) PARDEG
    in singleton $ evalPolyVec p' $ item x

testPredicateCircuit :: PAR -> AC
testPredicateCircuit p = predicateCircuit @F @I @P $ testPredicate p

testPredicate :: PAR -> PHI
testPredicate p = predicate $ testFunction p

testSPS :: PHI -> SPS
testSPS = fiatShamir @(MiMCHash F) . commitOpen . specialSoundProtocol @D

initAccumulator :: PHI -> Accumulator K I C F
initAccumulator = emptyAccumulator @D

initAccumulatorInstance :: PHI -> AccumulatorInstance K I C F
initAccumulatorInstance phi =
    let Accumulator ai _ = initAccumulator phi
    in ai

testPublicInput0 :: I F
testPublicInput0 = singleton $ fromConstant @Natural 42

testInstanceProofPair :: PHI -> NARKInstanceProof K I C F
testInstanceProofPair phi = narkInstanceProof (testSPS phi) testPublicInput0 U1

testMessages :: PHI -> Vector K [F]
testMessages phi =
    let NARKInstanceProof _ (NARKProof _ ms) = testInstanceProofPair phi
    in ms

testNarkProof :: PHI -> Vector K (C F)
testNarkProof phi =
    let NARKInstanceProof _ (NARKProof cs _) = testInstanceProofPair phi
    in cs

testPublicInput :: PHI -> I F
testPublicInput phi =
    let NARKInstanceProof pi _ = testInstanceProofPair phi
    in pi

testAccumulatorScheme :: PHI -> AccumulatorScheme D 1 I C F
testAccumulatorScheme = accumulatorScheme @(MiMCHash F)

testAccumulator :: PHI -> Accumulator K I C F   -- accumulator in prover output
testAccumulator phi =
    let s = testAccumulatorScheme phi
    in fst $ prover s (initAccumulator phi) $ testInstanceProofPair phi

testProverMismsatch :: PHI -> PHI -> PHI -> Accumulator K I C F   -- prover output
testProverMismsatch phi1 phi2 phi3 =
    let s = testAccumulatorScheme phi1
    in prover s (initAccumulator phi2) $ testInstanceProofPair phi3    

testAccumulatorInstance :: PHI -> AccumulatorInstance K I C F
testAccumulatorInstance phi =
    let Accumulator ai _ = testAccumulator phi
    in ai

testAccumulationProof :: PHI -> Vector (D - 1) (C F)    -- accumulation proof in prover output
testAccumulationProof phi =
    let s = testAccumulatorScheme phi
    in snd $ prover s (initAccumulator phi) $ testInstanceProofPair phi

testVerifierResult :: PHI -> AccumulatorInstance K I C F
testVerifierResult phi =
    let s = testAccumulatorScheme phi
    in verifier s (testPublicInput phi) (testNarkProof phi) (initAccumulatorInstance phi) (testAccumulationProof phi)

testDeciderResult :: PHI -> (Vector K (C F), C F)
testDeciderResult phi = decider (testAccumulator phi)
    --let s = testAccumulatorScheme phi
    --in decider (testAccumulator phi)

{-- Tentative code: I would like to itereate the prover to accumulate at each step a new proof associated with a new predicate and then output the vector of accumulator-proof pairs. Then we can call the verifier to check each iteration step and call teh decider on the last accumulator to check the final result.

iterateProver :: [PHI] -> [(Accumulator K I C F, Vector (D - 1) (C F))]
iterateProver phis =
    let s = testAccumulatorScheme (head phis)  -- Use the scheme from the first PHI
        initialAcc = initAccumulator (head phis)  -- Initialize with the first PHI
        iterateStep (acc, results) phi =
            let (newAcc, proofVec) = prover s acc (testInstanceProofPair phi)  -- Prover step
            in (newAcc, results ++ [(newAcc, proofVec)])  -- Append the new accumulator and proof
    in snd $ foldl iterateStep (initialAcc, []) phis

iterateProverVerifier :: [PHI] -> (Accumulator K I C F, [Vector (D - 1) (C F)])
iterateProverVerifier phis =
    let s = testAccumulatorScheme (head phis)  -- Use the scheme from the first PHI
        initialAcc = initAccumulator (head phis)  -- Initialize with the first PHI
        iterateStep (acc, allProofs) phi =
            let proof = testInstanceProofPair phi
                (newAcc, proofVec) = prover s acc proof  -- Prover step
                verifierAcc = verifier s (testPublicInput phi) (testNarkProof phi) (AccumulatorInstance newAcc) proofVec
            in if verifierAcc == AccumulatorInstance newAcc
               then (newAcc, allProofs ++ [proofVec])  -- Append the proofVec to the list of proofs
               else error "Verification failed: Prover and Verifier accumulators do not match"
    in foldl iterateStep (initialAcc, []) phis
--}

specAlgebraicMap :: Spec
specAlgebraicMap = do
    describe "Algebraic map specification" $ do
        describe "Algebraic map" $ do
            it "must output zeros on the public input and testMessages" $ do
               withMaxSuccess 10 $ property $
                    \p -> algebraicMap @D (testPredicate p) (testPublicInput $ testPredicate p) (testMessages $ testPredicate p) (unsafeToVector []) one
                        == replicate (acSizeN $ testPredicateCircuit p) zero

zeroVector :: (Num (c f), KnownNat k) => (Vector k (c f), c f)
zeroVector = (replicate zero, zero)

specAccumulatorScheme :: Spec
specAccumulatorScheme = do
    describe "Accumulator scheme specification" $ do
        describe "verifier" $ do
            it "must output zeros" $ do
                withMaxSuccess 10 $ property $ \p -> testVerifierResult (testPredicate p) == testAccumulatorInstance (testPredicate p)
            it "must reject on different predicates" $ do
                withMaxSuccess 10 $ property $ \p q -> p!=q ==> testVerifierResult (testPredicate q) != testAccumulatorInstance (testPredicate p)
            it "must reject on different predicates" $ do 
                withMaxSuccess 10 $ property $ \p q r -> p!=q || p!=r || q!=r ==> fst $ testProverMismsatch (testPredicate q) (testPredicate p) (testPredicate r) != testVerifierResult (testPredicate p) && fst $ testProverMismsatch (testPredicate q) (testPredicate p) (testPredicate r) != testVerifierResult (testPredicate q) && fst $ testProverMismsatch (testPredicate q) (testPredicate p) (testPredicate r) != testVerifierResult (testPredicate r)
        describe "decider" $ do
            it "must output zeros" $ do
                withMaxSuccess 10 $ property $ \p -> testDeciderResult (testPredicate p) == zeroVector (C F) C F
            it "must reject on different predicates" $ do 
                withMaxSuccess 10 $ property $ \p q r -> p!=q || p!=r || q!=r ==> fst $ decider testProverMismsatch (testPredicate q) (testPredicate p) (testPredicate r) != zeroVector (C F) C F

specIVC :: Spec
specIVC = do
    specAlgebraicMap
    specAccumulatorScheme