{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Algorithm.Keccak (specKeccak) where

import Control.Monad (forM_)
import Data.Constraint
import Data.Constraint.Nat
import Data.Constraint.Unsafe
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (isPrefixOf, isSuffixOf, take, (++))
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (~))
import GHC.TypeLits (KnownSymbol, SomeNat (..), Symbol, symbolVal)
import GHC.TypeNats (someNatVal)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath.Posix
import System.IO (IO)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Text.Regex.TDFA
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Symbolic.Algorithm.Hash.Keccak (AlgorithmSetup, keccak, keccakVar)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.VarByteString
import ZkFold.Symbolic.Interpreter (Interpreter)
import Prelude (String, pure, read, (<>), (==))
import qualified Prelude as Haskell

-- | Adds following obvious constraints.
withConstraints
  :: forall n {r}
   . KnownNat n
  => ( ( Mod (n * 8) 8 ~ 0
       , KnownNat (n * 8)
       )
       => r
     )
  -> r
withConstraints =
  withDict (timesNat @n @8) $
    withDict (withConstraints' @n)

withConstraints'
  :: forall n
   . KnownNat n
    :- (Mod (n * 8) 8 ~ 0)
withConstraints' =
  Sub $
    withDict
      (unsafeAxiom @(Mod (n * 8) 8 ~ 0))
      Dict

-- | Test the implementation of a hashing algorithm with @Zp BLS12_381_Scalar@ as base field for ByteStrings.
type Element = Zp BLS12_381_Scalar

-- | Symbolic context.
type Context = Interpreter Element

-- | These test files are provided by the Computer Security Resource Center.
-- Passing these tests is a requirement for having an implementation of a hashing function officially validated.
-- https://csrc.nist.gov/Projects/Cryptographic-Algorithm-Validation-Program/Secure-Hashing#shavs
--
-- Folder also includes https://keccak.team/obsolete/KeccakKAT-3.zip, which contains test vectors for Keccak.
dataDir :: FilePath
dataDir = "test/data/sha-3bytetestvectors/"

getTestFiles :: forall (algorithm :: Symbol). KnownSymbol algorithm => IO [FilePath]
getTestFiles = Haskell.filter isAlgoFile <$> listDirectory dataDir
 where
  isAlgoFile :: String -> Haskell.Bool
  isAlgoFile s =
    if "Keccak" `isPrefixOf` algorithm
      then ("KAT_" <> Haskell.drop 6 algorithm <> ".txt") `isSuffixOf` s
      else (algorithm `isPrefixOf` s) && (".rsp" `isSuffixOf` s)

  algorithm :: String
  algorithm = (\c -> if c == '-' then '_' else c) <$> symbolVal (Proxy @algorithm)

readRSP :: forall (algorithm :: Symbol). KnownSymbol algorithm => FilePath -> IO [(Natural, Natural, Natural)]
readRSP path = do
  fullTests <- lookupEnv "FULL_SHA3"
  contents <- Haskell.readFile path
  let parts =
        Haskell.filter (\s -> take 3 s == "Len") $
          splitOn
            ( let fn = takeFileName path
               in if isPrefixOf "ShortMsgKAT" fn || isPrefixOf "LongMsgKAT" fn
                    then "\n\n"
                    else "\r\n\r\n"
            )
            contents
  case fullTests of
    Haskell.Nothing -> pure $ take 20 $ catMaybes $ readTestCase @algorithm <$> parts
    _ -> pure $ catMaybes $ readTestCase @algorithm <$> parts

readTestCase :: forall algorithm. KnownSymbol algorithm => String -> Haskell.Maybe (Natural, Natural, Natural)
readTestCase s =
  if "Keccak" `isPrefixOf` symbolVal (Proxy @algorithm)
    then
      if numBits `mod` 8 == 0 -- Keccak tests are in bits, not bytes.
        then
          Haskell.Just (numBits `div` 8, msg, hash)
        else Haskell.Nothing
    else Haskell.Just (numBits `div` 8, msg, hash)
 where
  numBits :: Natural
  numBits = read numBitsS

  msg :: Natural
  msg = read ("0x" ++ msgS)

  hash :: Natural
  hash = read ("0x" ++ hashS)

  numBitsS :: String
  numBitsS = case s =~ ("Len = ([0-9]+)" :: String) of
    (_ :: String, _ :: String, _ :: String, [x]) -> x
    _ -> Haskell.error "unreachable"

  msgS :: String
  msgS = case s =~ ("Msg = ([a-fA-F0-9]+)" :: String) of
    (_ :: String, _ :: String, _ :: String, [x]) -> x
    _ -> Haskell.error "unreachable"

  hashS :: String
  hashS = case s =~ ("MD = ([a-fA-F0-9]+)" :: String) of
    (_ :: String, _ :: String, _ :: String, [x]) -> x
    _ -> Haskell.error "unreachable"

testAlgorithm
  :: forall (algorithm :: Symbol)
   . KnownSymbol algorithm
  => AlgorithmSetup algorithm
  => FilePath
  -> Spec
testAlgorithm file = do
  testCases <- runIO (readRSP @algorithm $ dataDir </> file)
  describe description $
    forM_ testCases $ \(numBytes, input, hash) -> do
      let bitMsgN = "calculates hash on a message of " <> Haskell.show numBytes <> " bytes (input is Natural)"
      case someNatVal numBytes of
        SomeNat (_ :: Proxy bytes) ->
          it bitMsgN $
            ( withConstraints @bytes $
                let inBS = fromConstant @Natural @(ByteString (bytes * 8) Context) input
                    inBSVar :: VarByteString 500_000 Context = fromNatural (value @(bytes * 8)) input
                 in (toConstant $ keccak @algorithm @Context @(bytes * 8) inBS, toConstant $ keccakVar @algorithm @Context @500_000 inBSVar)
            )
              `shouldBe` (hash, hash)
 where
  description :: String
  description = "Testing " <> symbolVal (Proxy @algorithm) <> " on " <> file

specKeccak'
  :: forall (algorithm :: Symbol)
   . KnownSymbol algorithm
  => AlgorithmSetup algorithm
  => Spec
specKeccak' = do
  testFiles <- runIO $ getTestFiles @algorithm
  forM_ testFiles $ testAlgorithm @algorithm

specKeccak :: Spec
specKeccak = do
  describe "Keccak" $ do
    keccakSimple @"Keccak256"
    specKeccak' @"Keccak256"
    specKeccak' @"SHA3-512"
    specKeccak' @"SHA3-384"
    specKeccak' @"SHA3-256"
    specKeccak' @"SHA3-224"
