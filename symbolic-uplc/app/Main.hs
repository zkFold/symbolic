{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Applicative            (Applicative, asum, pure, some, (<**>),
                                       (<*>))
import Control.Exception              (displayException, throwIO)
import Data.Bifunctor                 (first)
import Data.Bool                      (otherwise)
import Data.Eq                        (Eq, (==))
import Data.Foldable                  (fold, toList)
import Data.Function                  (($), (.))
import Data.Functor                   ((<$>))
import Data.List.NonEmpty             (NonEmpty (..), fromList)
import Data.Monoid                    (Last (..), Monoid, mempty)
import Data.Semigroup                 ((<>))
import Options.Applicative            qualified as O
import Prelude                        (error)
import System.IO                      (IO)
import System.IO                      qualified as IO
import System.OsPath                  qualified as OS
import System.OsPath                  (OsPath, (<.>))
import ZkFold.Symbolic.UPLC.Converter (ScriptType (..), SomeCircuit (..), convert)
import ZkFold.UPLC.Term (VersionedProgram (..), getProgram)
import qualified Codec.CBOR.Read as CBOR
import Data.Either (either)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Data.ByteString.Lazy as BS
import qualified Flat
import Data.Maybe (Maybe(..))
import qualified Data.Aeson as Aeson
import ZkFold.Protocol.NonInteractiveProof (setupProve)
import ZkFold.Protocol.Plonkup (Plonkup(..))
import ZkFold.Protocol.Plonkup.Utils (getParams, getSecretParams)
import ZkFold.Algebra.Class (zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point, BLS12_381_Scalar)
import qualified System.IO.Temp as Temp
import Control.Monad ((>>), (>>=))
import Data.Traversable (traverse)
import qualified System.Process as P
import Data.ByteString (ByteString)
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.Algebra.Field (Zp)

data InputType = UPLC | TPLC | PIR | UPLC'Flat | UPLC'CBOR deriving Eq

inputExt :: InputType -> OsPath
inputExt = OS.unsafeEncodeUtf . \case
  UPLC -> "uplc"
  TPLC -> "tplc"
  PIR -> "pir"
  UPLC'Flat -> "uplc-flat"
  UPLC'CBOR -> "uplc-cbor"

data Input = FileInput (NonEmpty OsPath) | StdIn InputType

(<.?>) :: IO.FilePath -> OS.OsString -> IO IO.FilePath
root <.?> ext = do
    pathRoot <- OS.encodeUtf root
    OS.decodeUtf (pathRoot <.> ext)

withTempFile :: IO IO.FilePath -> (IO.FilePath -> IO.Handle -> IO r) -> IO r
withTempFile template continue = do
    templateResult <- template
    Temp.withSystemTempFile templateResult continue

withBinaryInput :: InputType -> Input -> (IO.Handle -> IO r) -> IO r
withBinaryInput expected =
  let withNeededFile onPath onHandle =
        withTempFile ("preprocessed" <.?> inputExt expected)
          \neededPath neededHandle -> onPath neededPath >> onHandle neededHandle
   in \case
    StdIn actual
      | actual == expected -> ($ IO.stdin)
      | actual == UPLC -> withNeededFile \neededPath ->
        IO.getContents >>= P.readProcess "plutus" ["--stdin", "-o", neededPath]
      | otherwise -> withNeededFile \neededPath ->
        withTempFile ("input" <.?> inputExt actual) \inputPath inputHandle -> do
          BS.getContents >>= BS.hPut inputHandle
          P.callProcess "plutus" [inputPath, "-o", neededPath]
    FileInput (single :| []) | OS.takeExtension single == inputExt expected ->
      \continue -> do
        filePath <- OS.decodeUtf single
        IO.withBinaryFile filePath IO.ReadMode continue
    FileInput paths -> withNeededFile \neededPath -> do
      inputPaths <- traverse OS.decodeUtf paths
      P.callProcess "plutus" $ toList inputPaths <> ["-o", neededPath]

data OutputType = Circuit | PlutusVerifier | ProverInfo deriving Eq

at :: (Eq a, Applicative f, Monoid (f b)) => a -> b -> a -> f b
at x y z
  | x == z = pure y
  | otherwise = mempty

data Output = FileOutput (OutputType -> Last OsPath) | StdOut OutputType

withBinaryOutput :: OutputType -> Output -> (IO.Handle -> IO ()) -> IO ()
withBinaryOutput planned = \case
  StdOut actual | actual == planned -> ($ IO.stdout)
  FileOutput (($ planned) -> Last (Just path)) -> \c -> do
    filePath <- OS.decodeUtf path
    IO.withBinaryFile filePath IO.WriteMode c
  _ -> \_ -> pure ()

data Action = Act
  { actInput      :: Input
  , actScriptType :: ScriptType
  , actOutput     :: Output
  }

pathRead :: O.ReadM OsPath
pathRead = O.eitherReader (first displayException . OS.encodeUtf)

pathOption :: O.Mod O.OptionFields OsPath -> O.Parser OsPath
pathOption = O.option pathRead

main :: IO ()
main = do
  Act {..} <- O.execParser $ O.info (Act
    <$> asum [ FileInput . fromList <$> some (O.argument pathRead $ O.metavar "SCRIPT_FILE" <> O.help
                "A path to the source file(s). Input type is deduced from extension.")
             , StdIn <$> asum
                [ O.flag' TPLC (O.long "tplc" <> O.help "stdin is Typed Plutus Core with Names")
                , O.flag' PIR (O.long "pir" <> O.help "stdin is PIR with Names")
                , O.flag' UPLC'Flat (O.long "uplc-flat" <> O.help
                    "stdin is Untyped Plutus Core with NamedDeBruijn serialised in Flat")
                , O.flag' UPLC'CBOR (O.long "uplc-cbor" <> O.help
                    "stdin is Untyped Plutus Core with DeBruijn serialised in CBOR")
                , O.flag UPLC UPLC (O.long "uplc" <> O.help "stdin is Untyped Plutus Core with Names (default)")
                ]
             ]
    <*> asum [ O.flag' SpendingV1 (O.long "spending-v1" <> O.help "Input is a Spending V1 script")
             , O.flag' OtherV1 (O.long "other-v1" <> O.help "Input is a Minting, Certifying or Rewarding V1 script")
             , O.flag V3 V3 (O.long "v3" <> O.help "Input is a V3 script (default)")
             ]
    <*> asum [ FileOutput . fold <$> some (asum
                [ at Circuit <$> pathOption (O.metavar "CIRCUIT_FILE" <> O.long "circuit" <> O.short 'c'
                    <> O.help "File to write circuit to")
                , at PlutusVerifier <$> pathOption (O.metavar "VERIFIER_FILE" <> O.long "verifier" <> O.short 'v'
                    <> O.help "File to write verifier to")
                , at ProverInfo <$> pathOption (O.metavar "PROVER_INFO_FILE" <> O.long "prover-info" <> O.short 'p'
                    <> O.help "File to write prover info to")
                ])
             , StdOut <$> asum
                [ O.flag' PlutusVerifier (O.long "print-verifier" <> O.help "Write verifier to stdout")
                , O.flag' ProverInfo (O.long "print-prover-info" <> O.help "Write prover info to stdout")
                , O.flag Circuit Circuit (O.long "print-circuit" <> O.help "Write circuit to stdout (default)")
                ]
             ]
    <**> O.helper)
    (O.fullDesc <> O.progDesc "UPLC converter!")

  flat <- withBinaryInput UPLC'CBOR actInput \h -> do
    contents <- BS.hGetContents h
    ("", !flat) <- either throwIO pure $
        CBOR.deserialiseFromBytes CBOR.decodeBytes contents
    pure flat

  Program _ term <- either throwIO pure $ Flat.unflatWith getProgram flat

  case convert term actScriptType of
    SomeCircuit !circuit -> do

      withBinaryOutput Circuit actOutput \h ->
        BS.hPutStr h (Aeson.encode circuit)

      withBinaryOutput PlutusVerifier actOutput (error "TODO")

      withBinaryOutput ProverInfo actOutput \h -> do
        let (omega, k1, k2) = getParams 32
            (gs, h1) = getSecretParams zero
            !proverInfo = setupProve (Plonkup @_ @_ @32 @BLS12_381_G1_Point @BLS12_381_G2_Point @ByteString @(PolyVec (Zp BLS12_381_Scalar)) omega k1 k2 circuit h1 gs)
        BS.hPutStr h (Aeson.encode proverInfo)
