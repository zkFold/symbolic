{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Symbolic.Algorithm.SHA2 (specSHA2Natural, specSHA2) where

import           Control.Monad                               (forM_)
import           Data.Binary                                 (Binary)
import           Data.Bits                                   (shiftR)
import           Data.Function                               (($))
import           Data.Functor                                ((<$>))
import           Data.List                                   (isPrefixOf, isSuffixOf, take, (++))
import           Data.List.Split                             (splitOn)
import           Data.Proxy                                  (Proxy (..))
import           GHC.Generics                                (U1)
import           GHC.TypeLits                                (KnownSymbol, Symbol, symbolVal)
import           Prelude                                     (String, otherwise, pure, read, (<>), (==))
import qualified Prelude                                     as Haskell
import           System.Directory                            (listDirectory)
import           System.Environment                          (lookupEnv)
import           System.FilePath.Posix
import           System.IO                                   (IO)
import           Test.Hspec                                  (Spec, describe, runIO, shouldBe)
import           Test.QuickCheck                             (Gen, withMaxSuccess, (===))
import           Tests.Symbolic.ArithmeticCircuit            (it)
import           Text.Regex.TDFA

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Base.Data.Vector                     (Vector)
import           ZkFold.Prelude                              (chooseNatural)
import           ZkFold.Symbolic.Algorithms.Hash.SHA2
import           ZkFold.Symbolic.Class                       (Arithmetic)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, exec)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.VarByteString          (fromNatural)
import           ZkFold.Symbolic.Interpreter                 (Interpreter (Interpreter))

-- | These test files are provided by the Computer Security Resource Center.
-- Passing these tests is a requirement for having an implementation of a hashing function officially validated.
-- https://csrc.nist.gov/Projects/Cryptographic-Algorithm-Validation-Program/Secure-Hashing#shavs
--
dataDir :: FilePath
dataDir = "test/data/shabittestvectors/"

getTestFiles :: forall (algorithm :: Symbol) . KnownSymbol algorithm => IO [FilePath]
getTestFiles = Haskell.filter isAlgoFile <$> listDirectory dataDir
    where
        isAlgoFile :: String -> Haskell.Bool
        isAlgoFile s = (algorithm `isPrefixOf` s) && not ((algorithm <> "_") `isPrefixOf` s) && (".rsp" `isSuffixOf` s)

        algorithm :: String
        algorithm = (\c -> if c == '/' then '_' else c) <$> symbolVal (Proxy @algorithm)

readRSP :: FilePath -> IO [(Natural, Natural, Natural)]
readRSP path = do
    fullTests <- lookupEnv "FULL_SHA2"
    contents <- Haskell.readFile path
    let parts = Haskell.filter (\s -> take 3 s == "Len") $ splitOn "\r\n\r\n" contents
    case fullTests of
      Haskell.Nothing -> pure $ take 20 $ readTestCase <$> parts
      _               -> pure $ readTestCase <$> parts

readTestCase :: String -> (Natural, Natural, Natural)
readTestCase s = (numBits, msg, hash)
    where
        numBits :: Natural
        numBits = read numBitsS

        msgShift :: Haskell.Int
        msgShift
          | numBits `mod` 8 == 0 = 0
          | otherwise = 8 Haskell.- Haskell.fromIntegral (numBits `mod` 8)

        msg :: Natural
        msg = read ("0x" ++ msgS) `shiftR` msgShift

        hash :: Natural
        hash = read ("0x" ++ hashS)

        numBitsS :: String
        numBitsS = case s =~ ("Len = ([0-9]+)" :: String) of
            (_ :: String, _ :: String, _ :: String, [x]) -> x
            _                                            -> Haskell.error "unreachable"

        msgS :: String
        msgS = case s =~ ("Msg = ([a-f0-9]+)" :: String) of
            (_ :: String, _ :: String, _ :: String, [x]) -> x
            _                                            -> Haskell.error "unreachable"

        hashS :: String
        hashS = case s =~ ("MD = ([a-f0-9]+)" :: String) of
            (_ :: String, _ :: String, _ :: String, [x]) -> x
            _                                            -> Haskell.error "unreachable"


testAlgorithm
    :: forall (algorithm :: Symbol) element
    .  KnownSymbol algorithm
    => SHA2N algorithm (Interpreter element)
    => KnownNat (Log2 (ChunkSize algorithm))
    => ToConstant (ByteString (ResultSize algorithm) (Interpreter element))
    => FilePath
    -> Spec
testAlgorithm file = do
    testCases <- runIO (readRSP $ dataDir </> file)
    describe description $
        forM_ testCases $ \(bits, input, hash) -> do
            let bitMsgN = "calculates hash on a message of " <> Haskell.show bits <> " bits (input is Natural)"
            let bitMsgS = "calculates hash on a message of " <> Haskell.show bits <> " bits (input is VarByteString)"
            it bitMsgN $ toConstant (sha2Natural @algorithm @(Interpreter element) bits input) `shouldBe` hash
            it bitMsgS $ toConstant (sha2Var @algorithm @(Interpreter element) @10000 $ fromNatural bits input) `shouldBe` hash
    where
        description :: String
        description = "Testing " <> symbolVal (Proxy @algorithm) <> " on " <> file


-- | Test the implementation of a hashing algorithm with @Zp BLS12_381_Scalar@ as base field for ByteStrings.
--
specSHA2Natural'
    :: forall (algorithm :: Symbol) element
    .  KnownSymbol algorithm
    => SHA2N algorithm (Interpreter element)
    => KnownNat (Log2 (ChunkSize algorithm))
    => ToConstant (ByteString (ResultSize algorithm) (Interpreter element))
    => Spec
specSHA2Natural' = do
    testFiles <- runIO $ getTestFiles @algorithm
    forM_ testFiles $ testAlgorithm @algorithm @element

specSHA2Natural :: Spec
specSHA2Natural = do
    specSHA2Natural' @"SHA224" @(Zp BLS12_381_Scalar)
    specSHA2Natural' @"SHA256" @(Zp BLS12_381_Scalar)
    specSHA2Natural' @"SHA384" @(Zp BLS12_381_Scalar)
    specSHA2Natural' @"SHA512" @(Zp BLS12_381_Scalar)
    specSHA2Natural' @"SHA512/224" @(Zp BLS12_381_Scalar)
    specSHA2Natural' @"SHA512/256" @(Zp BLS12_381_Scalar)

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x)

eval ::
  forall a n . (Arithmetic a, Binary a) =>
  ByteString n (ArithmeticCircuit a U1 U1) -> Vector n a
eval (ByteString bits) = exec bits

specSHA2bs
    :: forall (n :: Natural) (algorithm :: Symbol)
    .  KnownSymbol algorithm
    => SHA2 algorithm (ArithmeticCircuit (Zp BLS12_381_Scalar) U1 U1) n
    => SHA2N algorithm (Interpreter (Zp BLS12_381_Scalar))
    => Spec
specSHA2bs = do
    let n = value @n
        m = 2 ^ n -! 1
    it ("calculates " <> symbolVal (Proxy @algorithm) <> " of a " <> Haskell.show n <> "-bit bytestring (SLOW)") $ withMaxSuccess 2 $ do
        x <- toss m
        let hashAC = sha2 @algorithm @(ArithmeticCircuit (Zp BLS12_381_Scalar) U1 U1) @n $ fromConstant x
            ByteString (Interpreter hashZP) = sha2Natural @algorithm @(Interpreter (Zp BLS12_381_Scalar)) n x
        pure $ eval @(Zp BLS12_381_Scalar) @(ResultSize algorithm) hashAC === hashZP


-- | Test the implementation of a hashing algorithm with @ArithmeticCircuit (Zp BLS12_381_Scalar)@ as base field for ByteStrings.
--
specSHA2'
    :: forall (algorithm :: Symbol)
    .  KnownSymbol algorithm
    => SHA2N algorithm (Interpreter (Zp BLS12_381_Scalar))
    => SHA2 algorithm (ArithmeticCircuit (Zp BLS12_381_Scalar) U1 U1) 1
    => SHA2 algorithm (ArithmeticCircuit (Zp BLS12_381_Scalar) U1 U1) 63
    => SHA2 algorithm (ArithmeticCircuit (Zp BLS12_381_Scalar) U1 U1) 64
    => SHA2 algorithm (ArithmeticCircuit (Zp BLS12_381_Scalar) U1 U1) 1900
    => Spec
specSHA2' = do
    specSHA2bs @1    @algorithm
    specSHA2bs @63   @algorithm
    specSHA2bs @64   @algorithm
    specSHA2bs @1900 @algorithm

specSHA2 :: Spec
specSHA2 = do
    specSHA2' @"SHA224"
    specSHA2' @"SHA256"
    specSHA2' @"SHA384"
    specSHA2' @"SHA512"
    specSHA2' @"SHA512/224"
    specSHA2' @"SHA512/256"
