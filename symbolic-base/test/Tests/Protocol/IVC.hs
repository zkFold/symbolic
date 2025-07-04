module Tests.Protocol.IVC where

import Control.Lens ((^.))
import Data.Bifunctor (bimap, first)
import GHC.Generics (U1 (..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property, withMaxSuccess)
import ZkFold.Algebra.Class (FromConstant (..), ToConstant (..), one, zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (Natural)
import ZkFold.Algebra.Polynomial.Univariate (evalPolyVec)
import ZkFold.Algebra.Polynomial.Univariate.Simple (fromVector)
import ZkFold.Data.Package (packed, unpacked)
import ZkFold.Data.Vector (Vector (..), item, singleton, unsafeToVector)
import ZkFold.Protocol.IVC.Accumulator (
  Accumulator (..),
  emptyAccumulator,
  x,
 )
import ZkFold.Protocol.IVC.AccumulatorScheme as Acc
import ZkFold.Protocol.IVC.AlgebraicMap (algebraicMap)
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

-- type AC = ArithmeticCircuit A (I :*: P :*: I) U1

testFunction
  :: forall ctx
   . (Symbolic ctx, FromConstant A (BaseField ctx))
  => PAR
  -> ctx (Vector 1)
  -> ctx U1
  -> ctx (Vector 1)
testFunction p i _ =
  let p' = fromVector $ fmap fromConstant p
      z = FieldElement <$> unpacked i
      y = singleton $ evalPolyVec p' $ item z
   in packed $ fromFieldElement <$> y

-- testPredicateCircuit :: PAR -> AC
-- testPredicateCircuit p = predicateCircuit @A @I @P $ testPredicate p

specIVC :: Spec
specIVC = do
  let phi :: PAR -> PHI
      phi = predicate . testFunction

      sps :: PAR -> SPS
      sps = fiatShamir mimcHash . commitOpen fromConstant toConstant . specialSoundProtocol @D fromConstant toConstant . phi

      acc0 :: PAR -> Accumulator K I (DataSource C) F
      acc0 = emptyAccumulator @D . phi

      pi0 :: I F
      pi0 = singleton $ fromConstant @ZkFold.Algebra.Number.Natural 42

      narkIP p = narkInstanceProof (sps p) pi0 U1
      pi p = let NARKInstanceProof pi' _ = narkIP p in pi'
      cs p = let NARKInstanceProof _ (NARKProof cs' _) = narkIP p in cs'
      ms p = let NARKInstanceProof _ (NARKProof _ ms') = narkIP p in ms'

      scheme :: PAR -> AccumulatorScheme D 1 I (DataSource C) F
      scheme = accumulatorScheme mimcHash . phi

      acc p = fst $ prover (scheme p) (acc0 p) $ NARKInstanceProof (pi p) (NARKProof (cs p) (ms p))
      pf p = snd $ prover (scheme p) (acc0 p) $ NARKInstanceProof (pi p) (NARKProof (cs p) (ms p))

      verifierResult p = first dataSource $ verifier (scheme p) (pi p) (cs p) (acc0 p ^. x) (pf p)
      deciderResult p = bimap (fmap dataSource) dataSource $ decider (scheme p) $ acc p

  describe "Algebraic map specification" $ do
    describe "Algebraic map" $ do
      it "must output zeros on the public input and message" $ do
        withMaxSuccess 10 $ property $ \p ->
          any (/= zero) $ algebraicMap @D (phi p) (pi p) (ms p) (unsafeToVector []) one
  describe "Accumulator scheme specification" $ do
    describe "decider" $ do
      it "must output zeros" $ do
        withMaxSuccess 10 $ property $ \p ->
          deciderResult p == (singleton zero, zero)
    describe "verifier" $ do
      it "must output zeros" $ do
        withMaxSuccess 10 $ property $ \p ->
          verifierResult p == first dataSource (acc p ^. x)
