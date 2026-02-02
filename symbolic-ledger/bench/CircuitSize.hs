module Main where

import Control.Applicative (Applicative (..))
import Data.ByteString.Lazy (ByteString)
import Data.Function (($))
import Data.Semigroup (Semigroup (..))
import Data.String (String)
import qualified Data.String as String
import System.IO (IO)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden
import Text.Show (Show (..))
import ZkFold.ArithmeticCircuit (ArithmeticCircuit)
import qualified ZkFold.ArithmeticCircuit as Circuit

import ZkFold.Symbolic.Ledger.Circuit.Compile (ledgerCircuit, LedgerCircuit)
import ZkFold.Symbolic.Ledger.Examples.One (Bi, Bo, Ud, A, Ixs, Oxs, TxCount)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)

metrics :: String -> ArithmeticCircuit a i o -> ByteString
metrics name circuit =
  String.fromString name
    <> "\nNumber of polynomial constraints: "
    <> String.fromString (show $ Circuit.acSizeN circuit)
    <> "\nNumber of variables: "
    <> String.fromString (show $ Circuit.acSizeM circuit)
    <> "\nNumber of lookup constraints: "
    <> String.fromString (show $ Circuit.acSizeL circuit)
    <> "\nNumber of lookup tables: "
    <> String.fromString (show $ Circuit.acSizeT circuit)

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Ledger circuit golden tests"
      [ Golden.goldenVsString name ("stats/" <> name) $ pure (metrics name circuit)
      | (name, circuit) <- ledgerExamples
      ]

ledgerExamples :: [(String, LedgerCircuit Bi Bo Ud A Ixs Oxs TxCount)]
ledgerExamples =
  [("Ledger.1.1.2.1.1.1.1", ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @RollupBFInterpreter)]
