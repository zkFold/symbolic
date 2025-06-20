{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Protocol.IVC (specIVC) where

import Data.Bifunctor (first)
import GHC.Generics (U1 (..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property, withMaxSuccess)
import ZkFold.Algebra.Class (FromConstant (..), ToConstant (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (Natural, type (-))
import ZkFold.Algebra.Polynomial.Univariate (evalPolyVec)
import ZkFold.Algebra.Polynomial.Univariate.Simple (fromVector)
import ZkFold.Data.Package (packed, unpacked)
import ZkFold.Data.Vector (Vector (..), item, singleton)
import ZkFold.Protocol.IVC.Accumulator (
  Accumulator (..),
  AccumulatorInstance (..),
  emptyAccumulator,
 )
import ZkFold.Protocol.IVC.AccumulatorScheme as Acc
import ZkFold.Protocol.IVC.CommitOpen (commitOpen)
import ZkFold.Protocol.IVC.FiatShamir (FiatShamir, fiatShamir)
import ZkFold.Protocol.IVC.NARK (
  NARKInstanceProof (..),
  NARKProof (..),
  narkInstanceProof,
 )
import ZkFold.Protocol.IVC.Oracle (mimcHash)
import ZkFold.Protocol.IVC.Predicate (Predicate (..), predicate)
import ZkFold.Protocol.IVC.RecursiveFunction (DataSource (..))
import ZkFold.Protocol.IVC.SpecialSound (specialSoundProtocol)
import ZkFold.Symbolic.Class (BaseField, Symbolic)
import ZkFold.Symbolic.Data.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Interpreter (Interpreter)
import Prelude hiding (Num (..), pi, replicate, sum, (+), (^))

type A = Zp BLS12_381_Scalar

type F = FieldElement (Interpreter A)

type C = BLS12_381_G1_Point (Interpreter A)

type I = Vector 1

type P = U1

type K = 1

type PHI = Predicate A I P

type SPS = FiatShamir 1 I P (DataSource C) [F] [A] F

type D = 2

type PARDEG = 5

type PAR = Vector PARDEG A

testFunction
  :: forall ctx
   . (Symbolic ctx, FromConstant A (BaseField ctx))
  => PAR -> ctx (Vector 1) -> ctx U1 -> ctx (Vector 1)
testFunction p i _ =
  let p' = fromVector $ fmap fromConstant p
      x = FieldElement <$> unpacked i
      y = singleton $ evalPolyVec p' $ item x
   in packed $ fromFieldElement <$> y

-- testPredicateCircuit :: PAR -> AC
-- testPredicateCircuit p = predicateCircuit @F @I @P $ testPredicate p

testPredicate :: PAR -> PHI
testPredicate p = predicate $ testFunction p

testSPS :: PHI -> SPS
testSPS =
  fiatShamir mimcHash
    . commitOpen fromConstant toConstant
    . specialSoundProtocol @D fromConstant toConstant

initAccumulator :: PHI -> Accumulator K I (DataSource C) F
initAccumulator = emptyAccumulator @D

initAccumulatorInstance :: PHI -> AccumulatorInstance K I (DataSource C) F
initAccumulatorInstance phi =
  let Accumulator ai _ = initAccumulator phi
   in ai

testPublicInput0 :: I F
testPublicInput0 = singleton $ fromConstant @Natural 42

testInstanceProofPair :: PHI -> NARKInstanceProof K I (DataSource C) F
testInstanceProofPair phi = narkInstanceProof (testSPS phi) testPublicInput0 U1

-- testMessages :: PHI -> Vector K [F]
-- testMessages phi =
--     let NARKInstanceProof _ (NARKProof _ ms) = testInstanceProofPair phi
--     in ms

testNarkProof :: PHI -> Vector K (DataSource C)
testNarkProof phi =
  let NARKInstanceProof _ (NARKProof cs _) = testInstanceProofPair phi
   in cs

testPublicInput :: PHI -> I F
testPublicInput phi =
  let NARKInstanceProof pi _ = testInstanceProofPair phi
   in pi

testAccumulatorScheme :: PHI -> AccumulatorScheme D 1 I (DataSource C) F
testAccumulatorScheme = accumulatorScheme mimcHash

testAccumulator :: PHI -> Accumulator K I (DataSource C) F
testAccumulator phi =
  let s = testAccumulatorScheme phi
   in fst $ prover s (initAccumulator phi) $ testInstanceProofPair phi

testAccumulatorInstance :: PHI -> AccumulatorInstance K I C F
testAccumulatorInstance phi =
  let Accumulator ai _ = testAccumulator phi
   in first dataSource ai

testAccumulationProof :: PHI -> Vector (D - 1) (DataSource C)
testAccumulationProof phi =
  let s = testAccumulatorScheme phi
   in snd $ prover s (initAccumulator phi) $ testInstanceProofPair phi

-- testDeciderResult :: PHI -> (Vector K (C F), C F)
-- testDeciderResult phi =
--     let s = testAccumulatorScheme phi
--     in decider s $ testAccumulator phi

testVerifierResult :: PHI -> AccumulatorInstance K I C F
testVerifierResult phi =
  let s = testAccumulatorScheme phi
   in first dataSource $
        verifier
          s
          (testPublicInput phi)
          (testNarkProof phi)
          (initAccumulatorInstance phi)
          (testAccumulationProof phi)

specAlgebraicMap :: Spec
specAlgebraicMap = do
  describe "Algebraic map specification" (return ())

-- \$ do
-- describe "Algebraic map" $ do
--     it "must output zeros on the public input and testMessages" $ do
--        withMaxSuccess 10 $ property $
--             \p -> algebraicMap @D (testPredicate p) (testPublicInput $ testPredicate p) (testMessages $ testPredicate p) (unsafeToVector []) one
--                 == replicate (acSizeN $ testPredicateCircuit p) zero

specAccumulatorScheme :: Spec
specAccumulatorScheme = do
  describe "Accumulator scheme specification" $ do
    -- describe "decider" $ do
    --     it  "must output zeros" $ do
    --         withMaxSuccess 10 $ property $ \p -> testDeciderResult (testPredicate p) == (singleton zero, zero)
    describe "verifier" $ do
      it "must output zeros" $ do
        withMaxSuccess 10 $ property $ \p ->
          testVerifierResult (testPredicate p)
            == testAccumulatorInstance (testPredicate p)

specIVC :: Spec
specIVC = do
  specAlgebraicMap
  specAccumulatorScheme
