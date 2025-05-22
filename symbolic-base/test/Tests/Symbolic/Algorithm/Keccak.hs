{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Symbolic.Algorithm.Keccak (specKeccak) where

import           Control.Monad                          (forM_)
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Constraint.Unsafe
import           Data.Function                          (($))
import           Data.Functor                           ((<$>))
import           Data.List                              (isPrefixOf, isSuffixOf, take, (++))
import           Data.List.Split                        (splitOn)
import           Data.Proxy                             (Proxy (..))
import           Data.Type.Equality                     (type (~))
import           GHC.TypeLits                           (KnownSymbol, SomeNat (..), Symbol, symbolVal)
import           GHC.TypeNats                           (someNatVal, withKnownNat)
import           Prelude                                (String, pure, read, (<>), (==))
import qualified Prelude                                as Haskell
import           System.Directory                       (listDirectory)
import           System.Environment                     (lookupEnv)
import           System.FilePath.Posix
import           System.IO                              (IO)
import           Test.Hspec                             (Spec, describe, it, runIO, shouldBe)
import           Text.Regex.TDFA

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Algebra.Field                   (Zp)
import           ZkFold.Algebra.Number
import           ZkFold.Symbolic.Algorithm.Hash.Keccak  (AlgorithmSetup, keccak)
import           ZkFold.Symbolic.Class                  (Symbolic)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Interpreter            (Interpreter)

withConstraints ::
  forall n {r}.
  KnownNat n =>
  ( ( Mod (n * 8) 8 ~ 0
    , KnownNat (n * 8)
    ) =>
    r
  ) ->
  r
withConstraints = withDict (withConstraints' @n)

withConstraints' ::
  forall n.
  KnownNat n
    :- ( Mod (n * 8) 8 ~ 0
       , KnownNat (n * 8)
       )
withConstraints' =
  Sub
    $ withKnownNat @(n * 8)
      (unsafeSNat (value @n * 8))
    $ withDict
      (unsafeAxiom @(Mod (n * 8) 8 ~ 0))
      Dict

{- | These test files are provided by the Computer Security Resource Center.
Passing these tests is a requirement for having an implementation of a hashing function officially validated.
https://csrc.nist.gov/Projects/Cryptographic-Algorithm-Validation-Program/Secure-Hashing#shavs
-}
dataDir :: FilePath
dataDir = "test/data/sha-3bytetestvectors/"

getTestFiles :: forall (algorithm :: Symbol). KnownSymbol algorithm => IO [FilePath]
getTestFiles = Haskell.filter isAlgoFile <$> listDirectory dataDir
 where
  isAlgoFile :: String -> Haskell.Bool
  isAlgoFile s = (algorithm `isPrefixOf` s) && (".rsp" `isSuffixOf` s)

  algorithm :: String
  algorithm = (\c -> if c == '-' then '_' else c) <$> symbolVal (Proxy @algorithm)

readRSP :: FilePath -> IO [(Natural, Natural, Natural)]
readRSP path = do
  fullTests <- lookupEnv "FULL_SHA3"
  contents <- Haskell.readFile path
  let parts = Haskell.filter (\s -> take 3 s == "Len") $ splitOn "\r\n\r\n" contents
  case fullTests of
    Haskell.Nothing -> pure $ take 20 $ readTestCase <$> parts
    _               -> pure $ readTestCase <$> parts

readTestCase :: String -> (Natural, Natural, Natural)
readTestCase s = (numBytes, msg, hash)
 where
  numBytes :: Natural
  numBytes = read numBytesS `div` 8

  msg :: Natural
  msg = read ("0x" ++ msgS)

  hash :: Natural
  hash = read ("0x" ++ hashS)

  numBytesS :: String
  numBytesS = case s =~ ("Len = ([0-9]+)" :: String) of
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

testAlgorithm ::
  forall (algorithm :: Symbol) element.
  KnownSymbol algorithm =>
  AlgorithmSetup algorithm (Interpreter element) =>
  -- TODO: Why do I need to say symbolic constraint here? Did SHA2 also required it?
  Symbolic (Interpreter element) =>
  FilePath ->
  Spec
testAlgorithm file = do
  testCases <- runIO (readRSP $ dataDir </> file)
  describe description $
    forM_ testCases $ \(numBytes, input, hash) -> do
      let bitMsgN = "calculates hash on a message of " <> Haskell.show numBytes <> " bytes (input is Natural)"
      case someNatVal numBytes of
        SomeNat (_ :: Proxy bytes) ->
          it bitMsgN $
            ( withConstraints @bytes $
                let inBS = fromConstant @Natural @(ByteString (bytes * 8) (Interpreter element)) input
                 in toConstant $ keccak @algorithm @(Interpreter element) @(bytes * 8) inBS
            )
              `shouldBe` hash
 where
  description :: String
  description = "Testing " <> symbolVal (Proxy @algorithm) <> " on " <> file

-- | Test the implementation of a hashing algorithm with @Zp BLS12_381_Scalar@ as base field for ByteStrings.
--
specKeccak'
    :: forall (algorithm :: Symbol) element
    .  KnownSymbol algorithm
    => AlgorithmSetup algorithm (Interpreter element)
    => Symbolic (Interpreter element)
    => Spec
specKeccak' = do
    testFiles <- runIO $ getTestFiles @algorithm
    forM_ testFiles $ testAlgorithm @algorithm @element

specKeccak :: Spec
specKeccak = do
  describe "Keccak" $ do
    specKeccak' @"SHA3-512" @Element
    specKeccak' @"SHA3-384" @Element
    specKeccak' @"SHA3-256" @Element
    specKeccak' @"SHA3-224" @Element

type Element = Zp BLS12_381_Scalar
