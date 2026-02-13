module Tests.Symbolic.Ledger.E2E.Utils where

import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Prelude qualified as Haskell

-- | Measure and print the time taken to compute an action.
time :: Haskell.String -> Haskell.IO a -> Haskell.IO a
time label action = do
  start <- getCPUTime
  res <- action
  end <- getCPUTime
  let diff = Haskell.fromIntegral (end Haskell.- start) Haskell./ (10 Haskell.^ (12 :: Haskell.Integer))
  printf "Time taken to compute %s: %0.3f sec\n" label (diff :: Haskell.Double)
  Haskell.pure res
