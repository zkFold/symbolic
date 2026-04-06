module Tests.Symbolic.Ledger.JSON.LedgerContractInputGolden (specLedgerContractInputJSON) where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy (toStrict)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Prelude (($), (==))
import Prelude qualified as Haskell

import Paths_symbolic_ledger (getDataFileName)
import Tests.Symbolic.Ledger.E2E.Two (batch, newState, prevState, witness)
import ZkFold.Symbolic.Ledger.Circuit.Compile (LedgerContractInput (..))

-- | Golden tests for LedgerContractInput JSON encoding.
--
-- Set the @GOLDEN_ACCEPT=1@ environment variable to overwrite the golden file
-- when the output changes, similar to @tasty --accept@.  Example:
specLedgerContractInputJSON :: Spec
specLedgerContractInputJSON = describe "LedgerContractInput JSON" $ do
  let
    lci =
      LedgerContractInput
        { lciPreviousState = prevState
        , lciTransactionBatch = batch
        , lciNewState = newState
        , lciStateWitness = witness
        }
  let encConf = defConfig {confIndent = Spaces 2}
      enc = encodePretty' encConf
      readGolden = do
        path <- getDataFileName "test/data/ledger_contract_input.json"
        bytes <- BL.readFile path
        -- Force the lazy ByteString to be fully read so the file handle is
        -- closed before we potentially write back to the same path.
        _ <- evaluate (toStrict bytes)
        Haskell.pure (path, bytes)
      accept = do
        env <- lookupEnv "GOLDEN_ACCEPT"
        Haskell.pure (env == Haskell.Just "1")
  it "encodes to JSON matching the golden file" $ do
    (goldenPath, goldenBytes) <- readGolden
    let encoded = enc lci
    if encoded == goldenBytes
      then Haskell.pure ()
      else do
        shouldAccept <- accept
        if shouldAccept
          then BL.writeFile goldenPath encoded
          else encoded `shouldBe` goldenBytes

  it "decodes from the golden JSON back to the same value (by re-encoding)" $ do
    (_, goldenBytes) <- readGolden
    case eitherDecode goldenBytes of
      Haskell.Left err -> expectationFailure err
      Haskell.Right parsed -> enc parsed `shouldBe` enc lci
