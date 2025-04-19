{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Algebra.ReedSolomon where

import           Data.Bool                                   (bool)
import           Data.List                                   (sort)
import           Data.Typeable                               (Typeable, typeOf)
import qualified Data.Vector                                 as V
import           GHC.Natural                                 (Natural)
import           Prelude
import qualified Prelude                                     as P
import           Test.Hspec
import           Test.QuickCheck

import qualified ZkFold.Algebra.Class                        as C
import           ZkFold.Algebra.Class                        hiding ((*), (+))
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381      as BLS12_381
import qualified ZkFold.Algebra.EllipticCurve.BN254          as BN254
import qualified ZkFold.Algebra.EllipticCurve.Pasta          as Pasta
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Algorithm.ReedSolomon


data ReedSolomonExample f = ReedSolomonExample
    {
        pe  :: f,       -- primitive element
        k   :: Int,     -- message length
        r   :: Int,     -- number of errors * 2
        msg :: [f],     -- message
        err :: [f]      -- error polynomial
    } deriving (Show)

instance (Arbitrary f, FiniteField f, Eq f) => Arbitrary (ReedSolomonExample f) where
  arbitrary = do
    let pe = fromConstant (3 :: Natural)
        c = fromIntegral $ min 100 (order @f)
    t <- chooseInt (1,P.div c 3)
    let r = 2*t
        k = c P.- r P.- 1
    msg <- vector @f k
    er <- polyErr (k+t) t
    return $ ReedSolomonExample pe k r msg er

polyErr :: forall c. (Arbitrary c, Field c) => Int -> Int -> Gen [c]
polyErr kt t = do
    pos <- shuffle (replicate t (1 :: Integer) P.++ replicate kt 0)
    mapM (\p -> bool arbitrary (pure zero) (p==0) ) pos

----------------------------------------------------------------------------------------

propGenerator :: forall c. (FiniteField c, Eq c) => ReedSolomonExample c -> Bool
propGenerator ReedSolomonExample {..} =
    let vals = take r $ iterate (C.* pe) (pe :: c)
        polyGen = generator r pe
    in all (\x -> evalPoly polyGen x == zero) vals

propEncoder :: forall c. (Field c, Eq c) => ReedSolomonExample c -> Bool
propEncoder ReedSolomonExample {..} =
    let encodedMsg = encode msg pe r
        reminder = snd $ qr encodedMsg (generator r pe)
    in deg reminder == -1

propBerlekampNoError :: forall c. (FiniteField c, Eq c) => ReedSolomonExample c -> Bool
propBerlekampNoError ReedSolomonExample {..} =
    let
        vals = V.iterateN r (C.* pe) pe
        encodedMsg = encode msg pe r
        syndromes = toPoly $ V.map (evalPoly encodedMsg) vals
    in syndromes == zero && berlekamp syndromes r == (0, one)

propBerlekampWithErrors :: forall c. (FiniteField c, Ord c) => ReedSolomonExample c -> Bool
propBerlekampWithErrors ReedSolomonExample {..} =
    let
        vals = V.iterateN r (C.* pe) pe
        encoded' = encode msg pe r
        errorMsg = toPoly $ V.fromList err
        encoded = encoded' C.+ errorMsg
        syndromes = toPoly $ V.map (evalPoly encoded) vals
        bl = snd $ berlekamp syndromes r
        roots = filter (/= zero) $ map (\x -> bool zero x (evalPoly bl x == zero)) $ takeWhile (/= pe) $ iterate (C.* pe) pe

        es1 = takeWhile (/= one) $ iterate (C.* pe) one
        rightLocators = filter (/= zero) $ zipWith (\e p -> bool zero (finv p) (e /= zero)) err es1
    in sort roots == sort rightLocators

propDecodeWithoutError :: forall c. (FiniteField c, Eq c) => ReedSolomonExample c -> Bool
propDecodeWithoutError ReedSolomonExample {..} =
    let encoded = encode msg pe r
        decoded = decode encoded pe r (k+r)
    in toPoly (V.fromList msg) == decoded

propDecodeWithError :: forall c. (FiniteField c, Eq c) => ReedSolomonExample c -> Bool
propDecodeWithError ReedSolomonExample {..} =
    let encoded' = encode msg pe r
        errorMsg = toPoly $ V.fromList err
        encoded = encoded' C.+ errorMsg
        decoded = decode encoded pe r (k+r)
    in toPoly (V.fromList msg) == decoded

specReedSolomon':: forall a . (FiniteField a, Ord a, Arbitrary a, Show a, Typeable a) => Spec
specReedSolomon' = do
    describe "Reed-Solomon" $ do
        describe ("Type: " ++ show (typeOf @a zero)) $ do
            it "generator function is correct" $ do
                property $ propGenerator @a
            it "encoder function is correct" $ do
                property $ propEncoder @a
            it "berlekamp function is correct without errors" $ do
                property $ propBerlekampNoError @a
            it "berlekamp function is correct with errors" $ do
                property $ propBerlekampWithErrors @a
            it "decode function is correct without errors" $ do
                property $ propDecodeWithoutError @a
            it "decode function is correct with errors" $ do
                property $ propDecodeWithError @a

specReedSolomon :: Spec
specReedSolomon = do
    specReedSolomon' @BN254.Fr
    specReedSolomon' @BN254.Fp
    specReedSolomon' @BLS12_381.Fr
    specReedSolomon' @BLS12_381.Fq
    specReedSolomon' @Pasta.Fp
    specReedSolomon' @Pasta.Fq
