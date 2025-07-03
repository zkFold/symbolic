module Tests.Algebra.Groebner (specGroebner) where

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
    describe "mono is 1 test" $ do
      it "should pass" $
        do
          oneM (mono @Fr @Natural empty)
          `shouldBe` True
    describe "Polynomial is 0 test" $ do
      it "should pass" $ do
        zeroP @Fr @Natural @Natural
          ( poly
              [ (zero, mono $ fromList [(1, 1), (2, 2), (3, 3)])
              , (zero, mono $ fromList [(1, 1), (2, 2), (3, 3)])
              ]
          )
          `shouldBe` True
    describe "Dividable monos" $ do
      it "should be equal" $
        do
          mono @Natural @Natural (fromList [(1, 1), (2, 2), (3, 3)])
          `dividable` mono (fromList [(2, 1), (3, 1), (1, 1)])
          `shouldBe` True
    describe "monos comparisons" $ do
      it "should be correct" $ do
        compare
          (mono @Natural @Natural $ fromList [(1, 1), (3, 2), (2, 3)])
          (mono $ fromList [(1, 1), (2, 2), (3, 3)])
          `shouldBe` GT
        compare
          (mono @Natural @Natural $ fromList [(1, 1), (3, 1)])
          (mono $ fromList [(1, 1), (2, 1), (3, 1)])
          `shouldBe` LT
        compare
          ( poly @Fr
              [ (1, mono @Natural @Natural $ fromList [(1, 1), (2, 1)])
              , (1, mono $ fromList [(1, 1), (2, 1), (3, 1)])
              ]
          )
          ( poly
              [ (1, mono $ fromList [(1, 1), (2, 1), (3, 1)])
              , (1, mono $ fromList [(1, 1), (3, 1)])
              ]
          )
          `shouldBe` GT
    describe "Polynomial multiplication test" $ do
      it "should pass" $ do
        head testPoly * (testPoly !! 1)
          `shouldBe` poly
            [ (1, mono $ fromList [(1, 1), (2, 2), (3, 1)])
            , (1, mono $ fromList [(1, 1), (2, 2)])
            , (1, mono $ fromList [(1, 1), (3, 2), (2, 1)])
            , (1, mono $ fromList [(1, 1), (2, 1), (3, 1)])
            ]
    describe "Leading term test" $ do
      it "should pass" $ do
        map (snd . lt . (testPoly !!)) [0 .. 2]
          `shouldBe` [ mono $ fromList [(1, 1), (2, 1), (3, 1)]
                     , mono $ fromList [(2, 1)]
                     , mono $ fromList [(1, 1), (3, 1)]
                     ]
    describe "S-polynomial test" $ do
      it "should pass" $ do
        let s = makeSPoly (head testPoly) (testPoly !! 1)
        s
          `shouldBe` poly
            [ (negate one, mono $ fromList [(1, 1), (2, 1)])
            , (1, mono $ fromList [(1, 1), (3, 2)])
            ]
        s `fullReduceMany` [testPoly !! 2]
          `shouldBe` poly
            [ (negate one, mono $ fromList [(1, 1), (2, 1)])
            , (1, mono $ fromList [(1, 1)])
            ]
    describe "Groebner basis test" $ do
      it "should pass" $ do
        groebner (GroebnerParams 5 (const True)) testPoly
          `shouldBe` [var 1 * var 3 + var 1, var 2 + var 3]
