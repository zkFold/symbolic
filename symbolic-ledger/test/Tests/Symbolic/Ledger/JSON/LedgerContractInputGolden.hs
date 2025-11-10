module Tests.Symbolic.Ledger.JSON.LedgerContractInputGolden (specLedgerContractInputJSON) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Prelude (($))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Circuit.Compile (LedgerContractInput (..))

import Tests.Symbolic.Ledger.E2E.Two (batch, newState, prevState, witness)
import Data.Aeson.Encode.Pretty (defConfig, Config (..), encodePretty', Indent (..))

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
    goldenPath = "symbolic-ledger/test/data/ledger_contract_input.json"
  let encConf = defConfig { confIndent = Spaces 2 }
      enc = encodePretty' encConf
  it "encodes to JSON matching the golden file" $ do
    goldenBytes <- BL.readFile goldenPath
    enc lci `shouldBe` goldenBytes

  it "decodes from the golden JSON back to the same value (by re-encoding)" $ do
    goldenBytes <- BL.readFile goldenPath
    case eitherDecode goldenBytes of
      Haskell.Left err -> expectationFailure err
      Haskell.Right parsed -> enc parsed `shouldBe` enc lci
