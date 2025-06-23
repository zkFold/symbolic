module Tests.Algebra.GroebnerBasis (specGroebner) where

import Data.Map (empty, fromList)
import GHC.Natural (Natural)
import Test.Hspec
import Prelude hiding (Eq (..), Num (..), (/), (^))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Polynomial.Multivariate

testPoly :: [Poly Fr Natural Integer]
testPoly =
  [ var 1 * var 2 + var 1 * var 2 * var 3
  , var 2 + var 3
  , var 1 + var 1 * var 3
  ]

specGroebner :: Spec
specGroebner = do
  describe "Groebner basis specification" $ do
    describe "Monomial is 1 test" $ do
      it "should pass" $
        do
          oneM (monomial @Fr @Natural empty)
          `shouldBe` True
    describe "Polynomial is 0 test" $ do
      it "should pass" $ do
        zeroP @Fr @Natural @Natural
          ( polynomial
              [ (zero, monomial $ fromList [(1, 1), (2, 2), (3, 3)])
              , (zero, monomial $ fromList [(1, 1), (2, 2), (3, 3)])
              ]
          )
          `shouldBe` True
    describe "Dividable monomials" $ do
      it "should be equal" $
        do
          monomial @Natural @Natural (fromList [(1, 1), (2, 2), (3, 3)])
          `dividable` monomial (fromList [(2, 1), (3, 1), (1, 1)])
          `shouldBe` True
    describe "Monomials comparisons" $ do
      it "should be correct" $ do
        compare
          (monomial @Natural @Natural $ fromList [(1, 1), (3, 2), (2, 3)])
          (monomial $ fromList [(1, 1), (2, 2), (3, 3)])
          `shouldBe` GT
        compare
          (monomial @Natural @Natural $ fromList [(1, 1), (3, 1)])
          (monomial $ fromList [(1, 1), (2, 1), (3, 1)])
          `shouldBe` LT
        compare
          ( polynomial @Fr
              [ (1, monomial @Natural @Natural $ fromList [(1, 1), (2, 1)])
              , (1, monomial $ fromList [(1, 1), (2, 1), (3, 1)])
              ]
          )
          ( polynomial
              [ (1, monomial $ fromList [(1, 1), (2, 1), (3, 1)])
              , (1, monomial $ fromList [(1, 1), (3, 1)])
              ]
          )
          `shouldBe` GT
    describe "Polynomial multiplication test" $ do
      it "should pass" $ do
        head testPoly * (testPoly !! 1)
          `shouldBe` polynomial
            [ (1, monomial $ fromList [(1, 1), (2, 2), (3, 1)])
            , (1, monomial $ fromList [(1, 1), (2, 2)])
            , (1, monomial $ fromList [(1, 1), (3, 2), (2, 1)])
            , (1, monomial $ fromList [(1, 1), (2, 1), (3, 1)])
            ]
    describe "Leading term test" $ do
      it "should pass" $ do
        map (snd . lt . (testPoly !!)) [0 .. 2]
          `shouldBe` [ monomial $ fromList [(1, 1), (2, 1), (3, 1)]
                     , monomial $ fromList [(2, 1)]
                     , monomial $ fromList [(1, 1), (3, 1)]
                     ]
    describe "S-polynomial test" $ do
      it "should pass" $ do
        let s = makeSPoly (head testPoly) (testPoly !! 1)
        s
          `shouldBe` polynomial
            [ (negate one, monomial $ fromList [(1, 1), (2, 1)])
            , (1, monomial $ fromList [(1, 1), (3, 2)])
            ]
        s `fullReduceMany` [testPoly !! 2]
          `shouldBe` polynomial
            [ (negate one, monomial $ fromList [(1, 1), (2, 1)])
            , (1, monomial $ fromList [(1, 1)])
            ]
    describe "Groebner basis test" $ do
      it "should pass" $ do
        groebner (GroebnerParams 5 (const True)) testPoly
          `shouldBe` [var 1 * var 3 + var 1, var 2 + var 3]
