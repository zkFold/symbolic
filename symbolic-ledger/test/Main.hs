module Main where

import Test.Hspec (hspec)
import Prelude (IO)

import Tests.Symbolic.Ledger.Update (specUpdateLedgerState)

main :: IO ()
main = hspec specUpdateLedgerState
