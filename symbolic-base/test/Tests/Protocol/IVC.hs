{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Protocol.IVC where

import Control.Lens ((^.))
import GHC.Generics (U1 (..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property, withMaxSuccess)
import Prelude hiding (Num (..), pi, replicate, sum, zip, (+), (^))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (Natural)
import ZkFold.Algebra.Polynomial.Univariate (evalPolyVec)
import ZkFold.Algebra.Polynomial.Univariate.Simple (fromVector)
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.ArithmeticCircuit.Var (NewVar)
import ZkFold.ArithmeticCircuit.Witness (WitnessF)
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
import ZkFold.Protocol.IVC.OperationRecord (OperationRecord (opValue))
import ZkFold.Protocol.IVC.Oracle (OracleSource (..), mimcHash)
import ZkFold.Protocol.IVC.Predicate (Predicate (..), predicate)
import ZkFold.Protocol.IVC.RecursiveFunction (
  DataSource (DataSource),
  RecursiveI,
  RecursiveP,
 )
import ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol, specialSoundProtocolA)
import qualified ZkFold.Protocol.IVC.SpecialSound as SPS
import ZkFold.Protocol.IVC.WeierstrassWitness (WeierstrassWitness (..))
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))

type A = Zp BLS12_381_Scalar

type F = FieldElement (Interpreter A)

type PT = WeierstrassWitness (Interpreter A)

type PT' = WeierstrassWitness (CircuitContext A)

type C = OperationRecord A PT

type C' = OperationRecord (WitnessF A NewVar) PT'

type I = Vector 1

type P = U1

type K = 1

type PHI = Predicate A I P

type PHIREC = Predicate A (RecursiveI I) (RecursiveP D K I P PT)

type SPS0 = SpecialSoundProtocol 1 I P A

type SPS0Rec = SpecialSoundProtocol 1 (RecursiveI I) (RecursiveP D K I P PT) F

type SPS = FiatShamir 1 I P C A

type SPSRec = FiatShamir 1 (RecursiveI I) (RecursiveP D K I P PT) C F

type D = 2

type PARDEG = 5

type PAR = Vector PARDEG A

instance OracleSource A A where source = pure

instance OracleSource A C where
  source = fmap toConstant . source @F . DataSource . opValue

instance OracleSource A PT where
  source = fmap toConstant . source @F . DataSource

instance {-# OVERLAPPING #-} Scale A PT where
  scale = scale @F . fromConstant

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
      sps0 = specialSoundProtocolA @D . phi

      -- sps0Rec :: PAR -> SPS0Rec
      -- sps0Rec = specialSoundProtocolI @D . phiRec

      sps :: PAR -> SPS
      sps = fiatShamir mimcHash . commitOpen cyclicCommit . sps0

      -- spsRec :: PAR -> SPSRec
      -- spsRec = fiatShamir mimcHash . commitOpen . sps0Rec

      pi0 :: I A
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
      cs :: PAR -> Vector 1 C
      cs p =
        let NARKInstanceProof _ (NARKProof z _) = narkIP p
         in (zero .+) <$> z
      ms p = let NARKInstanceProof _ (NARKProof _ z) = narkIP p in z

      scheme :: PAR -> AccumulatorScheme D 1 I C C A
      scheme = accumulatorScheme mimcHash cyclicCommit . phi

      acc0 :: PAR -> Accumulator K I C A
      acc0 = emptyAccumulator @D cyclicCommit . phi

      -- acc0Rec :: PAR -> Accumulator K (RecursiveI I) (DataSource C) F
      -- acc0Rec = emptyAccumulator @D . phiRec

      acc p = fst $ prover (scheme p) (acc0 p) $ NARKInstanceProof (pi p) (NARKProof (cs p) (ms p))
      pf :: PAR -> Vector K C
      pf p = fmap (zero .+) $ snd $ prover (scheme p) (acc0 p) $ NARKInstanceProof (pi p) (NARKProof (cs p) (ms p))

      verifierResult p = verifier (scheme p) (pi p) (cs p) (acc0 p ^. x) (pf p)
      deciderResult p = decider (scheme p) $ acc p

  describe "WeierstrassWitness" $ do
    it "is a homomorphic commitment" $ do
      withMaxSuccess 10 $ property $ \(fromConstant @Integer -> p) (fromConstant @Integer -> q) ->
        cyclicCommit @(WeierstrassWitness (Interpreter A)) [p + q] zero
          == (cyclicCommit [p] + cyclicCommit [q]) zero
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
          (\(a, b) -> all (== zero) a && all (== zero) b) $
            FS.verifier (sps p) (pi p) (zip (ms p) (cs p)) (unsafeToVector [])
  describe "Accumulator scheme specification" $ do
    describe "decider" $ do
      it "must output zeros" $ do
        withMaxSuccess 10 $ property $ \p ->
          deciderResult p == (zero, zero)
    describe "verifier" $ do
      it "must output zeros" $ do
        withMaxSuccess 10 $ property $ \p ->
          verifierResult p == (acc p ^. x)
