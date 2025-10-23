{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Algorithm.SHA2 (specSHA2Natural, specSHA2) where

import Control.Monad (forM_)
import Data.Binary (Binary)
import Data.Bits (shiftR)
import Data.Constraint ((\\))
import Data.Constraint.Unsafe (unsafeAxiom)
import Data.Function (($))
import Data.Functor (fmap, (<$>))
import Data.List (isPrefixOf, isSuffixOf, take, (++))
import Data.List.Split (splitOn)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath.Posix
import System.IO (IO)
import Test.Hspec (Spec, describe, runIO, shouldBe)
import Test.QuickCheck (withMaxSuccess, (===))
import Text.Regex.TDFA
import Prelude (String, otherwise, pure, read, (<>), (==))
import qualified Prelude as Haskell

import Tests.Common (it, toss)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.ArithmeticCircuit.Elem (Elem, exec)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Algorithm.Hash.SHA2
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.VarByteString (fromNatural)

-- | These test files are provided by the Computer Security Resource Center.
-- Passing these tests is a requirement for having an implementation of a hashing function officially validated.
-- https://csrc.nist.gov/Projects/Cryptographic-Algorithm-Validation-Program/Secure-Hashing#shavs
dataDir :: FilePath
dataDir = "test/data/shabittestvectors/"

getTestFiles :: forall (algorithm :: Symbol). KnownSymbol algorithm => IO [FilePath]
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
    _ -> pure $ readTestCase <$> parts

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
    _ -> Haskell.error "unreachable"

  msgS :: String
  msgS = case s =~ ("Msg = ([a-f0-9]+)" :: String) of
    (_ :: String, _ :: String, _ :: String, [x]) -> x
    _ -> Haskell.error "unreachable"

  hashS :: String
  hashS = case s =~ ("MD = ([a-f0-9]+)" :: String) of
    (_ :: String, _ :: String, _ :: String, [x]) -> x
    _ -> Haskell.error "unreachable"

testAlgorithm
  :: forall (algorithm :: Symbol) element
   . KnownSymbol algorithm
  => Arithmetic element
  => SHA2N algorithm element
  => ToConstant (ByteString (ResultSize algorithm) element)
  => FilePath
  -> Spec
testAlgorithm file = do
  testCases <- runIO (readRSP $ dataDir </> file)
  describe description $
    forM_ testCases $ \(bits, input, hash) ->
      do
        let bitMsgN = "calculates hash on a message of " <> Haskell.show bits <> " bits (input is Natural)"
        let bitMsgS = "calculates hash on a message of " <> Haskell.show bits <> " bits (input is VarByteString)"
        it bitMsgN $ toConstant (sha2Natural @algorithm @element bits input) `shouldBe` hash
        it bitMsgS
          ((toConstant (sha2Var @algorithm $ fromNatural @10000 @element bits input)
           \\ unsafeAxiom @(1 <= PaddedLength 10000 (ChunkSize algorithm) (2 * WordSize algorithm)))
           `shouldBe` hash)
 where
  description :: String
  description = "Testing " <> symbolVal (Proxy @algorithm) <> " on " <> file

-- | Test the implementation of a hashing algorithm with @Zp BLS12_381_Scalar@ as base field for ByteStrings.
specSHA2Natural'
  :: forall (algorithm :: Symbol) element
   . KnownSymbol algorithm
  => Arithmetic element
  => SHA2N algorithm element
  => ToConstant (ByteString (ResultSize algorithm) element)
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

eval :: (Arithmetic a, Binary a) => ByteString n (Elem a) -> Vector n a
eval (exec -> ByteString bits) = fromBool <$> bits

specSHA2bs
  :: forall (n :: Natural) (algorithm :: Symbol)
   . KnownSymbol algorithm
  => SHA2 algorithm (Elem (Zp BLS12_381_Scalar)) n
  => SHA2N algorithm (Zp BLS12_381_Scalar)
  => Spec
specSHA2bs = do
  let n = value @n
      m = 2 ^ n
  it ("calculates " <> symbolVal (Proxy @algorithm) <> " of a " <> Haskell.show n <> "-bit bytestring") $ withMaxSuccess 2 $ do
    x <- toss m
    let hashAC = sha2 @algorithm @(Elem (Zp BLS12_381_Scalar)) @n $ fromConstant x
        ByteString hashZP = sha2Natural @algorithm @(Zp BLS12_381_Scalar) n x
    pure $ eval hashAC === fmap fromBool hashZP

-- | Test the implementation of a hashing algorithm with @ArithmeticCircuit (Zp BLS12_381_Scalar)@ as base field for ByteStrings.
specSHA2'
  :: forall (algorithm :: Symbol)
   . KnownSymbol algorithm
  => SHA2N algorithm (Zp BLS12_381_Scalar)
  => SHA2 algorithm (Elem (Zp BLS12_381_Scalar)) 1
  => SHA2 algorithm (Elem (Zp BLS12_381_Scalar)) 63
  => SHA2 algorithm (Elem (Zp BLS12_381_Scalar)) 64
  => SHA2 algorithm (Elem (Zp BLS12_381_Scalar)) 1900
  => Spec
specSHA2' = do
  specSHA2bs @1 @algorithm
  specSHA2bs @63 @algorithm
  specSHA2bs @64 @algorithm
  specSHA2bs @1900 @algorithm

specSHA2 :: Spec
specSHA2 = do
  specSHA2' @"SHA224"
  specSHA2' @"SHA256"
  specSHA2' @"SHA384"
  specSHA2' @"SHA512"
  specSHA2' @"SHA512/224"
  specSHA2' @"SHA512/256"
