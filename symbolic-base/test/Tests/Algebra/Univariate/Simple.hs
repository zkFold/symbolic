{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}

module Tests.Algebra.Univariate.Simple where

import           Data.Eq                                     (Eq, (/=))
import           Data.Function                               (($))
import           Data.Functor                                (fmap)
import           Data.Semigroup                              ((<>))
import           Data.Typeable
import           Test.Hspec                                  (Spec, describe)
import qualified Test.QuickCheck                             as QC
import           Test.QuickCheck                             ((=/=), (===), (==>))
import           Tests.Common                                (it, typeAt)
import           Text.Show                                   (Show, show)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381      (Fq, Fr)
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Algebra.Polynomial.Univariate.Simple
import           ZkFold.Data.Vector                          (Vector, toV, zipWith)

specSimplePoly' ::
    forall a n .
    (QC.Arbitrary a, Eq a, Field a, Show a, Typeable a, KnownNat n) => Spec
specSimplePoly' = describe (show (typeAt @(SimplePoly a n)) <> " spec") do
    describe "vec2poly" do
        it "respects addition" \(p :: SimplePoly a n) q ->
            vec2poly (p + q) === vec2poly p + (vec2poly q :: Poly a)
        it "respects zero" $
            vec2poly (zero :: SimplePoly a n) === (zero :: Poly a)
        it "respects negation" \(p :: SimplePoly a n) ->
            vec2poly (negate p) === negate (vec2poly p :: Poly a)
        it "respects action" \(p :: SimplePoly a n) (c :: a) ->
            vec2poly (c *. p) === c *. (vec2poly p :: Poly a)
        it "completes evalPoly to evalPolyVec" \(p :: SimplePoly a n) x ->
            evalPolyVec p x === evalPoly (vec2poly p :: Poly a) x
    describe "poly2vec" do
        it "respects addition" \(p :: Poly a) q ->
            poly2vec (p + q) === poly2vec p + (poly2vec q :: SimplePoly a n)
        it "respects zero" $
            poly2vec (zero :: Poly a) === (zero :: SimplePoly a n)
        it "respects negation" \(p :: Poly a) ->
            poly2vec (negate p) === negate (poly2vec p :: SimplePoly a n)
        it "respects multiplication" \(p :: Poly a) q ->
            poly2vec (p * q) === poly2vec p * (poly2vec q :: SimplePoly a n)
        it "respects action" \(p :: Poly a) (c :: a) ->
            poly2vec (c *. p) === c *. (poly2vec p :: SimplePoly a n)
        it "is left inverse to vec2poly" \(p :: SimplePoly a n) ->
            poly2vec (vec2poly p :: Poly a) === p
        -- NOTE: correctness of group & scale instances for @SimplePoly a n@
        -- follow from the properties given above,
        -- assuming instances for @Poly a@ are correct.
    it "is quotient modulo divisor of x^n" $
        poly2vec (monomial (value @n) one :: Poly a) === (zero :: SimplePoly a n)
    it "is quotient modulo x^n" \(p :: Vector n a) ->
        p /= zero ==> toPolyVec (toV p) =/= (zero :: SimplePoly a n)
    it "implements fromVector correctly" \(p :: Vector n a) ->
        fromVector p === toPolyVec (toV p)
    it "implements hadamard correctly" \(p :: Vector n a) q ->
        fromVector p .*. fromVector q === fromVector (zipWith (*) p q)
    it "implements additive action correctly" \(p :: Vector n a) c ->
        c +. fromVector p === fromVector (fmap (c +) p)

specSimplePoly :: Spec
specSimplePoly = do
    specSimplePoly' @Fr @32
    specSimplePoly' @Fr @128
    specSimplePoly' @Fq @32
    specSimplePoly' @Fq @128
