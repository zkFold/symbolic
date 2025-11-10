module Main where

import Test.Hspec (hspec)
import Prelude (IO, ($))

import Tests.Symbolic.Ledger.Update (specUpdateLedgerState)
import Tests.Symbolic.Ledger.JSON.LedgerContractInputGolden (specLedgerContractInputJSON)

main :: IO ()
main = hspec $ do
  specUpdateLedgerState
  specLedgerContractInputJSON
