{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Protocol.Plonkup (specPlonkup) where

import           Control.Monad                                       (forM_, return)
import           Data.Bool                                           (Bool)
import           Data.ByteString                                     (ByteString)
import           Data.Eq                                             (Eq (..))
import           Data.Foldable                                       (Foldable, toList)
import           Data.Function                                       (($))
import           Data.Functor.Rep                                    (Rep, Representable)
import           Data.Int                                            (Int)
import           Data.List                                           (head, sort)
import           Data.Ord                                            (Ord)
import           GHC.Generics                                        (U1 (..))
import           GHC.IsList                                          (IsList (fromList))
import           Test.Hspec
import           Test.QuickCheck

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field                     (fromZp)
import           ZkFold.Base.Algebra.Basic.Number                    (KnownNat, Natural)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381         (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Base.Algebra.EllipticCurve.Class             (CyclicGroup (..))
import           ZkFold.Base.Algebra.Polynomials.Multivariate        as PM
import           ZkFold.Base.Algebra.Polynomials.Univariate
import           ZkFold.Base.Data.Vector                             (Vector)
import           ZkFold.Base.Protocol.NonInteractiveProof            (setupProve)
import           ZkFold.Base.Protocol.Plonkup                        hiding (omega)
import           ZkFold.Base.Protocol.Plonkup.PlonkConstraint
import           ZkFold.Base.Protocol.Plonkup.Prover                 (plonkupProve)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Base.Protocol.Plonkup.Relation               (PlonkupRelation (..))
import           ZkFold.Base.Protocol.Plonkup.Testing
import           ZkFold.Base.Protocol.Plonkup.Utils                  (sortByList)
import           ZkFold.Base.Protocol.Plonkup.Witness                (PlonkupWitnessInput)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var

-- | Polynomial types and specific polynomials that were causing exceptions
problematicPolynomials :: (Ord a, FiniteField a) => [PM.Poly a (Var a (Vector 1)) Natural]
problematicPolynomials =
  [ var (ConstVar one),
    var (ConstVar zero),
    var (ConstVar $ one + one),
    let v1 = toVar (NewVar (EqVar "y\ETX^\246\226\195\154S\130M\tL\146y\248\201\162\220 \237n6p\bC\151\186\241\US\136\225\139"))
        v2 = toVar (NewVar (EqVar "~\180\185\222\SOH!\t\254\155\v\SI\187\&9\227\163|^\168Z\184Q\129\rN\218\SYN\GSp\189\139~^"))
     in polynomial [(one, M $ fromList [(v1, 1), (v2, 1)])],
    polynomial [(one, M $ fromList [(toVar (NewVar (EqVar "v1")), 1), (toVar (NewVar (EqVar "v2")), 1)])],
    polynomial [(one, M $ fromList [(toVar (NewVar (EqVar "v1")), 1), (ConstVar one, 1)])]
  ]

propPlonkConstraintConversion :: (Ord a, FiniteField a) => PlonkConstraint (Vector 1) a -> Bool
propPlonkConstraintConversion p =
  toPlonkConstraint (fromPlonkConstraint p) == p

propPlonkupRelationHolds ::
  forall i n l a.
  (Foldable l, KnownNat n, Arithmetic a) =>
  PlonkupRelation i n l a (PolyVec a) ->
  i a ->
  Bool
propPlonkupRelationHolds PlonkupRelation {..} w =
  let (w1, w2, w3) = witness w
      pub = negate $ toPolyVec $ fromList $ toList $ pubInput w
   in qL .*. w1 + qR .*. w2 + qO .*. w3 + qM .*. w1 .*. w2 + qC + pub == zero

propSortByListIsCorrect :: (Ord a) => [a] -> Bool
propSortByListIsCorrect xs = sortByList xs (sort xs) == sort xs

propPlonkPolyEquality ::
  forall i n l.
  (KnownNat n, Representable i, Representable l, Foldable l, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
  Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
  PlonkupWitnessInput i BLS12_381_G1_Point ->
  PlonkupProverSecret BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  Bool
propPlonkPolyEquality plonk witness secret pow =
  let setup = setupProve plonk
      (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)
      p = with4n6 @n $ qmX * aX * bX + qlX * aX + qrX * bX + qoX * cX + piX + qcX
   in p `evalPolyVec` (omega ^ fromZp pow) == zero

propPlonkGrandProductIsCorrect ::
  forall i n l.
  (KnownNat n, Representable i, Representable l, Foldable l, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
  Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
  PlonkupWitnessInput i BLS12_381_G1_Point ->
  PlonkupProverSecret BLS12_381_G1_Point ->
  Bool
propPlonkGrandProductIsCorrect plonk witness secret =
  let setup = setupProve plonk
      (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)
   in head (toList $ fromPolyVec grandProduct1) == one

propPlonkGrandProductEquality ::
  forall i n l.
  (KnownNat n, Representable i, Representable l, Foldable l, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
  Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
  PlonkupWitnessInput i BLS12_381_G1_Point ->
  PlonkupProverSecret BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  Bool
propPlonkGrandProductEquality plonk witness secret pow =
  let setup = setupProve plonk
      (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)

      gammaX = polyVecConstant gamma
      p =
        with4n6 @n $
          (aX + polyVecLinear beta gamma)
            * (bX + polyVecLinear (beta * k1) gamma)
            * (cX + polyVecLinear (beta * k2) gamma)
            * z1X
            .* alpha
            - (aX + (beta *. s1X) + gammaX)
            * (bX + (beta *. s2X) + gammaX)
            * (cX + (beta *. s3X) + gammaX)
            * (z1X .*. omegas')
            .* alpha
   in p `evalPolyVec` (omega ^ fromZp pow) == zero

propLookupPolyEquality ::
  forall i n l.
  (KnownNat n, Representable i, Representable l, Foldable l, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
  Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
  PlonkupWitnessInput i BLS12_381_G1_Point ->
  PlonkupProverSecret BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  Bool
propLookupPolyEquality plonk witness secret pow =
  let setup = setupProve plonk
      (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)

      p = with4n6 @n $ qkX * (aX - fX)
   in p `evalPolyVec` (omega ^ fromZp pow) == zero

propLookupGrandProductIsCorrect ::
  forall i n l.
  (KnownNat n, Representable i, Representable l, Foldable l, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
  Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
  PlonkupWitnessInput i BLS12_381_G1_Point ->
  PlonkupProverSecret BLS12_381_G1_Point ->
  Bool
propLookupGrandProductIsCorrect plonk witness secret =
  let setup = setupProve plonk
      (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)
   in z2X `evalPolyVec` omega == one

propLookupGrandProductEquality ::
  forall i n l.
  (KnownNat n, Representable i, Representable l, Foldable l, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
  Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
  PlonkupWitnessInput i BLS12_381_G1_Point ->
  PlonkupProverSecret BLS12_381_G1_Point ->
  ScalarFieldOf BLS12_381_G1_Point ->
  Bool
propLookupGrandProductEquality plonk witness secret pow =
  let setup = setupProve plonk
      (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)

      deltaX = polyVecConstant delta
      epsilonX = polyVecConstant epsilon
      p =
        with4n6 @n $
          z2X
            * (one + deltaX)
            * (epsilonX + fX)
            * ((epsilonX * (one + deltaX)) + tX + deltaX * (tX .*. omegas'))
            - (z2X .*. omegas')
            * ((epsilonX * (one + deltaX)) + h1X + deltaX * h2X)
            * ((epsilonX * (one + deltaX)) + h2X + deltaX * (h1X .*. omegas'))
   in p `evalPolyVec` (omega ^ fromZp pow) == zero

propLinearizationPolyEvaluation ::
  forall i n l.
  (KnownNat n, Representable i, Representable l, Foldable l, Ord (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
  Plonkup i n l BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
  PlonkupWitnessInput i BLS12_381_G1_Point ->
  PlonkupProverSecret BLS12_381_G1_Point ->
  Bool
propLinearizationPolyEvaluation plonk witness secret =
  let setup = setupProve plonk
      (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)
   in rX `evalPolyVec` xi == zero

instance Arbitrary (U1 a) where
  arbitrary = return U1

instance Arbitrary1 U1 where
  liftArbitrary _ = return U1

specPlonkup :: Spec
specPlonkup = do
  describe "Plonkup specification" $ do
    describe "Conversion to Plonk constraints and back" $ do
      it "produces equivalent polynomials" $ property $ propPlonkConstraintConversion @(ScalarFieldOf BLS12_381_G1_Point)
      it "handcrafted polynomials do not cause exceptions " $
        forM_ problematicPolynomials $
          \p -> fromPlonkConstraint (toPlonkConstraint @(ScalarFieldOf BLS12_381_G1_Point) p) `shouldBe` p
      it "'ConstVar a' does not cause exceptions " $
        property $
          \v -> fromPlonkConstraint (toPlonkConstraint @(ScalarFieldOf BLS12_381_G1_Point) @(Vector 1) (var $ ConstVar v)) == var (ConstVar v)
    describe "Sort by list is correct" $ do
      it "should hold" $ property $ propSortByListIsCorrect @Int
    describe "Plonkup relation" $ do
      it "should hold" $ property $ propPlonkupRelationHolds @(Vector 2) @32 @(Vector 3) @(ScalarFieldOf BLS12_381_G1_Point)
    describe "Plonk polynomials equality" $ do
      it "should hold" $ property $ propPlonkPolyEquality @(Vector 1) @32 @(Vector 2)
    describe "Plonk grand product correctness" $ do
      it "should hold" $ property $ withMaxSuccess 10 $ propPlonkGrandProductIsCorrect @(Vector 1) @32 @(Vector 2)
    describe "Plonkup grand product equality" $ do
      it "should hold" $ property $ withMaxSuccess 10 $ propPlonkGrandProductEquality @(Vector 1) @32 @(Vector 2)
    describe "Lookup polynomials equality" $ do
      it "should hold" $ property $ withMaxSuccess 10 $ propLookupPolyEquality @(Vector 1) @32 @(Vector 2)
    describe "Lookup grand product correctness" $ do
      it "should hold" $ property $ withMaxSuccess 10 $ propLookupGrandProductIsCorrect @(Vector 1) @32 @(Vector 2)
    describe "Lookup grand product equality" $ do
      it "should hold" $ property $ withMaxSuccess 10 $ propLookupGrandProductEquality @(Vector 1) @32 @(Vector 2)
    describe "Linearization polynomial in the challenge point" $ do
      it "evaluates to zero" $ property $ withMaxSuccess 10 $ propLinearizationPolyEvaluation @(Vector 1) @32 @(Vector 2)
