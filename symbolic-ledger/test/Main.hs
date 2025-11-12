module Main where

import Test.Hspec (hspec)
import Prelude (IO, ($))

import Tests.Symbolic.Ledger.JSON.LedgerContractInputGolden (specLedgerContractInputJSON)
import Tests.Symbolic.Ledger.Update (specUpdateLedgerState)

main :: IO ()
main = hspec $ do
  specUpdateLedgerState
  specLedgerContractInputJSON
