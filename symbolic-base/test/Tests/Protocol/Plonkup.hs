{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Protocol.Plonkup (specPlonkup) where

import           Control.Monad                                  (forM_)
import           Data.Binary                                    (Binary)
import           Data.Bool                                      (Bool, bool)
import           Data.ByteString                                (ByteString)
import           Data.Eq                                        (Eq (..))
import           Data.Foldable                                  (Foldable, toList)
import           Data.Function                                  (($))
import           Data.Functor.Rep                               (Rep, Representable)
import           Data.Int                                       (Int)
import           Data.List                                      (head, sort)
import           Data.Ord                                       (Ord)
import qualified Data.Vector                                    as V
import           GHC.IsList                                     (IsList (fromList))
import           Test.Hspec
import           Test.QuickCheck                                hiding (witness)
import           Tests.Protocol.Plonkup.Update                  (specPlonkupUpdate)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381         (BLS12_381_G1_Point, BLS12_381_G2_Point)
import           ZkFold.Algebra.EllipticCurve.Class             (CyclicGroup (..))
import           ZkFold.Algebra.Field                           (fromZp)
import           ZkFold.Algebra.Number                          (KnownNat, Natural)
import           ZkFold.Algebra.Polynomial.Multivariate         as PM
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Data.Vector                             (Vector)
import           ZkFold.Protocol.NonInteractiveProof            (setupProve)
import           ZkFold.Protocol.Plonkup                        hiding (omega)
import           ZkFold.Protocol.Plonkup.PlonkConstraint
import           ZkFold.Protocol.Plonkup.Prover                 (plonkupProve)
import           ZkFold.Protocol.Plonkup.Prover.Secret
import           ZkFold.Protocol.Plonkup.Relation               (PlonkupRelation (..))
import           ZkFold.Protocol.Plonkup.Testing
import           ZkFold.Protocol.Plonkup.Utils                  (sortByList)
import           ZkFold.Protocol.Plonkup.Witness                (PlonkupWitnessInput)
import           ZkFold.Symbolic.Class                          (Arithmetic)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var

-- | Polynomial types and specific polynomials that were causing exceptions
problematicPolynomials :: (Ord a, FiniteField a) => [PM.Poly a (Var a) Natural]
problematicPolynomials =
    [ var (ConstVar one)
    , var (ConstVar zero)
    , var (ConstVar $ one + one)
    , let v1 = toVar (EqVar "y\ETX^\246\226\195\154S\130M\tL\146y\248\201\162\220 \237n6p\bC\151\186\241\US\136\225\139")
          v2 = toVar (EqVar "~\180\185\222\SOH!\t\254\155\v\SI\187\&9\227\163|^\168Z\184Q\129\rN\218\SYN\GSp\189\139~^")
       in polynomial [(one, M $ fromList [(v1, 1), (v2, 1)])]
    , polynomial [(one, M $ fromList [(toVar (EqVar "v1"), 1), (toVar (EqVar "v2"), 1)])]
    , polynomial [(one, M $ fromList [(toVar (EqVar "v1"), 1), (ConstVar one, 1)])]
    ]

propPlonkConstraintConversion :: (Ord a, FiniteField a) => PlonkConstraint (Vector 1) a -> Bool
propPlonkConstraintConversion p =
    toPlonkConstraint (fromPlonkConstraint p) == p

propPlonkupRelationHolds :: forall i o n a .
    (KnownNat n, Arithmetic a) => PlonkupRelation i o n a (PolyVec a) ->
    i a ->
    a ->
    Property
propPlonkupRelationHolds PlonkupRelation {..} w zeta =
    let (w1, w2, w3) = witness w
        pub = negate $ toPolyVec $ fromList $ toList $ pubInput w
        !f_zeta' = w1 + zeta *. (w2 + zeta *. w3)
        !t_zeta = t1 + zeta *. (t2 + zeta *. t3)
        !f_zeta = toPolyVec $ V.zipWith3 (\lk ti ai -> bool ti ai (lk == one)) (fromPolyVec qK) (fromPolyVec t_zeta) (fromPolyVec f_zeta') :: PolyVec a n
     in (qL .*. w1 + qR .*. w2 + qO .*. w3 + qM .*. w1 .*. w2 + qC + pub == zero)
     .&&. (qK .*. (w1 + zeta *. w2 + zeta * zeta *. w3 - f_zeta) == zero)

propSortByListIsCorrect :: Ord a => [a] -> Bool
propSortByListIsCorrect xs = sortByList xs (sort xs) == sort xs

propPlonkPolyEquality ::
    forall i o n .
    (KnownNat n, Representable i, Representable o, Foldable o, Binary (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
    Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
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
    forall i o n .
    (KnownNat n, Representable i, Representable o, Foldable o, Binary (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
    Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
    PlonkupWitnessInput i BLS12_381_G1_Point ->
    PlonkupProverSecret BLS12_381_G1_Point ->
    Bool
propPlonkGrandProductIsCorrect plonk witness secret =
    let setup = setupProve plonk
        (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)
     in head (toList $ fromPolyVec grandProduct1) == one

propPlonkGrandProductEquality ::
    forall i o n .
    (KnownNat n, Representable i, Representable o, Foldable o, Binary (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
    Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
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
              * z1X .* alpha
              - (aX + (beta *. s1X) + gammaX)
              * (bX + (beta *. s2X) + gammaX)
              * (cX + (beta *. s3X) + gammaX)
              * (z1X .*. omegas')
              .* alpha
     in p `evalPolyVec` (omega ^ fromZp pow) == zero

propLookupPolyEquality ::
    forall i o n .
    (KnownNat n, Representable i, Representable o, Foldable o, Binary (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
    Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
    PlonkupWitnessInput i BLS12_381_G1_Point ->
    PlonkupProverSecret BLS12_381_G1_Point ->
    ScalarFieldOf BLS12_381_G1_Point ->
    Bool
propLookupPolyEquality plonk witness secret pow =
    let setup = setupProve plonk
        (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)

        p = with4n6 @n $ qkX * (aX + zeta *. (bX + zeta *. cX) - fX)
     in p `evalPolyVec` (omega ^ fromZp pow) == zero

propLookupGrandProductIsCorrect ::
    forall i o n .
    (KnownNat n, Representable i, Representable o, Foldable o, Binary (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
    Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
    PlonkupWitnessInput i BLS12_381_G1_Point ->
    PlonkupProverSecret BLS12_381_G1_Point ->
    Bool
propLookupGrandProductIsCorrect plonk witness secret =
    let setup = setupProve plonk
        (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)
     in head (toList $ fromPolyVec grandProduct2) == one

propLookupGrandProductEquality ::
    forall i o n .
    (KnownNat n, Representable i, Representable o, Foldable o, Binary (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
    Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
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
    forall i o n .
    (KnownNat n, Representable i, Representable o, Foldable o, Binary (Rep i), KnownNat (PlonkupPolyExtendedLength n)) =>
    Plonkup i o n BLS12_381_G1_Point BLS12_381_G2_Point ByteString (PolyVec (ScalarFieldOf BLS12_381_G1_Point)) ->
    PlonkupWitnessInput i BLS12_381_G1_Point ->
    PlonkupProverSecret BLS12_381_G1_Point ->
    Bool
propLinearizationPolyEvaluation plonk witness secret =
    let setup = setupProve plonk
        (_, _, PlonkupProverTestInfo {..}) = with4n6 @n $ plonkupProve @_ @_ @_ @_ @_ @ByteString setup (witness, secret)
     in rX `evalPolyVec` xi == zero

specPlonkup :: Spec
specPlonkup = do
    describe "Plonkup specification" $ do
        describe "Conversion to Plonk constraints and back" $ do
            it "produces equivalent polynomials" $ property $ withMaxSuccess 10 $ propPlonkConstraintConversion @(ScalarFieldOf BLS12_381_G1_Point)
            it "handcrafted polynomials do not cause exceptions " $
                forM_ problematicPolynomials $ \p ->
                    fromPlonkConstraint (toPlonkConstraint @(ScalarFieldOf BLS12_381_G1_Point) p) `shouldBe` p
            it "'ConstVar a' does not cause exceptions " $
                property $ withMaxSuccess 10 $ \v ->
                    fromPlonkConstraint (toPlonkConstraint @(ScalarFieldOf BLS12_381_G1_Point) @(Vector 1) (var $ ConstVar v)) == var (ConstVar v)
        describe "Sort by list is correct" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propSortByListIsCorrect @Int
        describe "Plonkup relation" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propPlonkupRelationHolds @(Vector 2) @(Vector 3) @32 @(ScalarFieldOf BLS12_381_G1_Point)
        describe "Plonk polynomials equality" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propPlonkPolyEquality @(Vector 1) @(Vector 2) @32
        describe "Plonk grand product correctness" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propPlonkGrandProductIsCorrect @(Vector 1) @(Vector 2) @32
        describe "Plonkup grand product equality" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propPlonkGrandProductEquality @(Vector 1) @(Vector 2) @32
        describe "Lookup polynomials equality" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propLookupPolyEquality @(Vector 1) @(Vector 2) @32
        describe "Lookup grand product correctness" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propLookupGrandProductIsCorrect @(Vector 1) @(Vector 2) @32
        describe "Lookup grand product equality" $ do
            it "should hold" $ property $ withMaxSuccess 10 $ propLookupGrandProductEquality @(Vector 1) @(Vector 2) @32
        describe "Linearization polynomial in the challenge point" $ do
            it "evaluates to zero" $ property $ withMaxSuccess 10 $ propLinearizationPolyEvaluation @(Vector 1) @(Vector 2) @32
        specPlonkupUpdate
