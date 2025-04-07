{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use -"                  #-}

module Tests.Algebra.Univariate.PolyVec (specUnivariatePolyVec) where

import           Data.Bool                                   (bool)
import           Data.Data                                   (Typeable, typeOf)
import           Data.List                                   ((\\))
import qualified Data.Vector                                 as V
import qualified Data.Vector.Algorithms.Intro                as VA
import           Prelude                                     hiding (Fractional (..), Num (..), drop, length, take,
                                                              (!!), (^))
import           Prelude                                     (abs)
import           Test.Hspec
import           Test.QuickCheck

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (Fr)
import           ZkFold.Base.Algebra.Polynomials.Univariate
import           ZkFold.Prelude                              (length, take)

propToPolyVec :: forall c s .
    (Ring c, KnownNat s) =>
    [c] -> Bool
propToPolyVec cs =
    let p = toPolyVec @_ @(PolyVec c) @s $ V.fromList cs
    in length (fromPolyVec p) == value @s

propCastPolyVec :: forall c s s' .
    (Field c, KnownNat s, KnownNat s', Eq c) =>
    [c] -> Bool
propCastPolyVec cs =
    let n = min (value @s) (value @s')
        cs' = V.fromList $ bool cs (take n cs) (length cs > n)
        p' = castPolyVec @_ @(PolyVec c) @s @s' (toPolyVec cs')
    in length (fromPolyVec p') == value @s'

propPolyVecDivision
    :: forall c s .
    (Field c, KnownNat s, Eq c) =>
    PolyVec c s -> PolyVec c s -> Bool
propPolyVecDivision p q =
    let d1 = deg @c @(Poly c) $ vec2poly p
        d2 = deg @c @(Poly c) $ vec2poly q
    in (p * q) `polyVecDiv` q == p || (d1 + d2 > fromIntegral (value @s) - 1)

-- TODO: Don't use a hardcoded root of unity
propPolyVecZero
    :: forall c s d .
    (KnownNat s, KnownNat d) =>
    (Field c, Eq c) =>
    Natural -> Bool
propPolyVecZero i =
    let Just omega = rootOfUnity 5 :: Maybe c
        p = polyVecZero @_ @(PolyVec c) @d (value @s)
        x = omega^abs i
    in p `evalPolyVec` x == zero

-- TODO: Don't use a hardcoded root of unity
propPolyVecLagrange
    :: forall c s d .
    (KnownNat s, KnownNat d) =>
    (Field c, Eq c) =>
    Natural -> Bool
propPolyVecLagrange i =
    let Just omega = rootOfUnity 5 :: Maybe c
        p = polyVecLagrange @_ @(PolyVec c) @d (value @s) i omega
    in p `evalPolyVec` (omega^i) == one &&
        all ((== zero) . (p `evalPolyVec`) . (omega^)) ([1 .. value @s] \\ [i])

propPolyVecGrandProduct
    :: forall c s .
    (Field c, KnownNat s, Ord c) =>
    PolyVec c s -> c -> c -> Bool
propPolyVecGrandProduct p beta gamma =
    let p' = rewrapPolyVec (V.modify VA.sort) p in
    let zs = polyVecGrandProduct zero p p' beta gamma in
    V.last (fromPolyVec zs) * (beta * V.last (fromPolyVec p) + gamma)
        == (beta * V.last (fromPolyVec p') + gamma)

specUnivariatePolyVec' :: forall c s d .
    (KnownNat s, KnownNat d) =>
    (Arbitrary c, Show c, Typeable c, Field c, Ord c) => Spec
specUnivariatePolyVec' = do
    describe "Univariate polynomials specification" $ do
        describe ("Type: " ++ show (typeOf @(PolyVec c s) zero)) $ do
            describe "toPolyVec" $ do
                it "should return a list of the correct length" $ do
                    property $ propToPolyVec @c @s
            describe "castPolyVec" $ do
                it "should return a list of the correct length" $ do
                    property $ propCastPolyVec @c @s @s
                it "should return a list of the correct length" $ do
                    property $ propCastPolyVec @c @s @d
                it "should return a list of the correct length" $ do
                    property $ propCastPolyVec @c @d @s
            describe "Ring axioms" $ do
                it "should satisfy additive associativity" $ do
                    property $ \(a :: PolyVec c s) b c -> (a + b) + c == a + (b + c)
                it "should satisfy additive commutativity" $ do
                    property $ \(a :: PolyVec c s) b -> a + b == b + a
                it "should satisfy additive identity" $ do
                    property $ \(a :: PolyVec c s) -> a + zero == a
                it "should satisfy additive inverse" $ do
                    property $ \(a :: PolyVec c s) -> a + negate a == zero
                it "should satisfy multiplicative associativity" $ do
                    property $ \(a :: PolyVec c s) b c -> (a * b) * c == a * (b * c)
                it "should satisfy multiplicative commutativity" $ do
                    property $ \(a :: PolyVec c s) b -> a * b == b * a
                it "should satisfy multiplicative identity" $ do
                    property $ \(a :: PolyVec c s) -> a * one == a
                it "should satisfy distributivity" $ do
                    property $ \(a :: PolyVec c s) b c -> a * (b + c) == a * b + a * c
            describe "Polynomial division" $ do
                it "should satisfy the definition" $ do
                    property $ \(p :: PolyVec c s) q -> q /= zero ==> propPolyVecDivision p q
                it "should correctly divide by cm * x^m + c0" $ do
                    property $ \(p :: PolyVec c s) (m :: Integer) (cm :: c) (c0 :: c) ->
                        propPolyVecDivision p (cm *. polyVecZero ((fromIntegral $ abs m) `Prelude.mod` (value @s)) + polyVecConstant c0)
            describe "polyVecZero" $ do
                it "should satisfy the definition" $ do
                    all (propPolyVecZero @c @s @d) [0 .. value @d -! 1] `shouldBe` True
            describe "Lagrange polynomial" $ do
                it "should satisfy the definition" $ do
                    all (propPolyVecLagrange @c @s @d) [1 .. value @s] `shouldBe` True
            describe "polyVecGrandProduct" $ do
                it "should satisfy the definition" $ do
                    property $ propPolyVecGrandProduct @c @s

specUnivariatePolyVec :: Spec
specUnivariatePolyVec = do
    specUnivariatePolyVec' @Fr @32 @135
