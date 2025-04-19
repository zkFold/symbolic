{-# LANGUAGE TypeApplications #-}

module Tests.Algebra.Permutation (specPermutation) where

import           Data.Map                     (elems)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as V
import           Prelude                      hiding (Fractional (..), Num (..), length)
import           Test.Hspec
import           Test.QuickCheck

import           ZkFold.Algebra.Permutation
import           ZkFold.Data.Vector           (fromVector)

specPermutation :: Spec
specPermutation = do
    describe "Permutations specification" $ do
        describe "Function: mkIndexPartition" $ do
            it "should preserve the total number of elements" $ property $
                \xsL -> let xs = V.fromList xsL in V.length (V.concat $ elems $ mkIndexPartition @Integer xs) `shouldBe` V.length xs
        describe "Function: fromCycles" $ do
            it "should preserve the elements" $ property $
                \v ->
                    let ts = mkIndexPartition @Integer $ V.fromList $ fromVector @100 v
                        p = fromPermutation @100 $ fromCycles ts
                    in V.modify V.sort p == V.modify V.sort (V.concat $ elems ts)
