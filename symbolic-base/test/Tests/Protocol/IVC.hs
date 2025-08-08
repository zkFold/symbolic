{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Protocol.IVC where

import Control.Lens ((^.))
import Data.Bifunctor (bimap, first)
import Data.Foldable (toList)
import GHC.Generics (U1 (..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property, withMaxSuccess)
import Prelude hiding (Num (..), pi, replicate, sum, zip, (+), (^))

import ZkFold.Algebra.Class (FromConstant (..), zero, (+))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (Natural)
import ZkFold.Algebra.Polynomial.Univariate (evalPolyVec)
import ZkFold.Algebra.Polynomial.Univariate.Simple (fromVector)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.Package (packed, unpacked)
import ZkFold.Data.Vector (Vector (..), item, singleton, unsafeToVector, zip)
import ZkFold.Protocol.IVC.Accumulator (
  Accumulator (..),
  emptyAccumulator,
  x,
 )
import ZkFold.Protocol.IVC.AccumulatorScheme as Acc
import ZkFold.Protocol.IVC.Commit (cyclicCommit)
import ZkFold.Protocol.IVC.CommitOpen (commitOpen)
import ZkFold.Protocol.IVC.FiatShamir (FiatShamir, fiatShamir)
import qualified ZkFold.Protocol.IVC.FiatShamir as FS
import ZkFold.Protocol.IVC.NARK (
  NARKInstanceProof (..),
  NARKProof (..),
  narkInstanceProof,
 )
import ZkFold.Protocol.IVC.Oracle (mimcHash)
import ZkFold.Protocol.IVC.Predicate (Predicate (..), predicate)
import ZkFold.Protocol.IVC.RecursiveFunction (
  DataSource (..),
  RecursiveI,
  RecursiveP,
 )
import ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol, specialSoundProtocolI)
import qualified ZkFold.Protocol.IVC.SpecialSound as SPS
import ZkFold.Protocol.IVC.WeierstrassWitness (WeierstrassWitness (..))
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))

type A = Zp BLS12_381_Scalar

type F = FieldElement (Interpreter A)

type C = WeierstrassWitness (Interpreter A)

type C' = WeierstrassWitness (CircuitContext A)

type I = Vector 1

type P = U1

type K = 1

type PHI = Predicate A I P

type PHIREC = Predicate A (RecursiveI I) (RecursiveP D K I P C)

type SPS0 = SpecialSoundProtocol 1 I P F

type SPS0Rec = SpecialSoundProtocol 1 (RecursiveI I) (RecursiveP D K I P C) F

type SPS = FiatShamir 1 I P (DataSource C) F

type SPSRec = FiatShamir 1 (RecursiveI I) (RecursiveP D K I P C) (DataSource C) F

type D = 2

type PARDEG = 5

type PAR = Vector PARDEG A

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

specIVC :: Spec
specIVC = do
  let phi :: PAR -> PHI
      phi = predicate . testFunction

      -- phiRecFunc :: PAR -> RecursiveFunction D K A I P C
      -- phiRecFunc = recursiveFunction @C' mimcHash . testFunction

      -- phiRec :: PAR -> PHIREC
      -- phiRec = predicate . phiRecFunc

      sps0 :: PAR -> SPS0
      sps0 = specialSoundProtocolI @D . phi

      -- sps0Rec :: PAR -> SPS0Rec
      -- sps0Rec = specialSoundProtocolI @D . phiRec

      sps :: PAR -> SPS
      sps = fiatShamir mimcHash . commitOpen cyclicCommit . sps0

      -- spsRec :: PAR -> SPSRec
      -- spsRec = fiatShamir mimcHash . commitOpen . sps0Rec

      pi0 :: I F
      pi0 = singleton $ fromConstant @ZkFold.Algebra.Number.Natural 42

      -- pi0Rec :: RecursiveI I F
      -- pi0Rec = singleton (fromConstant @ZkFold.Algebra.Number.Natural 42) :*: Par1 zero

      pi' p = SPS.input (sps0 p) pi0 U1
      ms' p = SPS.prover (sps0 p) pi0 U1 zero 1

      -- payloadRec :: PAR -> RecursiveP D K I P C F
      -- payloadRec p =
      --   fmap fromConstant . runInterpreter . arithmetize $
      --     RecursivePayload (hpure U1) (singleton zero) (acc0Rec p ^. x) false (singleton zero)
      -- pi'Rec p = SPS.input (sps0Rec p) pi0Rec (payloadRec p)
      -- ms'Rec p = SPS.prover (sps0Rec p) pi0Rec (payloadRec p) zero 1

      narkIP p = narkInstanceProof (sps p) pi0 U1
      pi p = let NARKInstanceProof z _ = narkIP p in z
      cs p = let NARKInstanceProof _ (NARKProof z _) = narkIP p in z
      ms p = let NARKInstanceProof _ (NARKProof _ z) = narkIP p in z

      scheme :: PAR -> AccumulatorScheme D 1 I (DataSource C) F
      scheme = accumulatorScheme mimcHash cyclicCommit . phi

      acc0 :: PAR -> Accumulator K I (DataSource C) F
      acc0 = emptyAccumulator @D cyclicCommit . phi

      -- acc0Rec :: PAR -> Accumulator K (RecursiveI I) (DataSource C) F
      -- acc0Rec = emptyAccumulator @D . phiRec

      acc p = fst $ prover (scheme p) (acc0 p) $ NARKInstanceProof (pi p) (NARKProof (cs p) (ms p))
      pf p = snd $ prover (scheme p) (acc0 p) $ NARKInstanceProof (pi p) (NARKProof (cs p) (ms p))

      verifierResult p = first dataSource $ verifier (scheme p) (pi p) (cs p) (acc0 p ^. x) (pf p)
      deciderResult p = bimap (fmap dataSource) dataSource $ decider (scheme p) $ acc p

  describe "WeierstrassWitness" $ do
    it "is a homomorphic commitment" $ do
      withMaxSuccess 10 $ property $ \(fromConstant @Integer -> p) (fromConstant @Integer -> q) ->
        cyclicCommit @(WeierstrassWitness (Interpreter A)) [p + q]
        == cyclicCommit [p] + cyclicCommit [q]
  describe "Special sound protocol specification" $ do
    describe "verifier" $ do
      it "must output zeros on the public input and message" $ do
        withMaxSuccess 10 $ property $ \p ->
          all (== zero) $ SPS.verifier (sps0 p) (pi' p) (singleton $ ms' p) (unsafeToVector [])
  -- describe "Recursive special sound protocol specification" $ do
  --   describe "verifier" $ do
  --     it "must output zeros on the public input and message" $ do
  --       withMaxSuccess 10 $ property $ \p ->
  --         all (== zero) $ SPS.verifier (sps0Rec p) (pi'Rec p) (singleton $ ms'Rec p) (unsafeToVector [])
  describe "Fiat-Shamir Commit-Open protocol specification" $ do
    describe "verifier" $ do
      it "must output zeros on the public input and message" $ do
        withMaxSuccess 10 $ property $ \p ->
          (\(a, b) -> all ((== zero) . dataSource) (toList a) && all (== zero) b) $
            FS.verifier (sps p) (pi p) (zip (ms p) (cs p)) (unsafeToVector [])
  describe "Accumulator scheme specification" $ do
    describe "decider" $ do
      it "must output zeros" $ do
        withMaxSuccess 10 $ property $ \p ->
          deciderResult p == (zero, zero)
    describe "verifier" $ do
      it "must output zeros" $ do
        withMaxSuccess 10 $ property $ \p ->
          verifierResult p == first dataSource (acc p ^. x)
