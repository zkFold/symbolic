module Main where

import Control.Applicative (pure)
import Data.ByteString.Lazy (ByteString)
import Data.Function (($))
import Data.Semigroup (Semigroup (..))
import Data.String (String)
import Data.String qualified as String
import System.IO (IO)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden qualified as Golden
import Text.Show (Show (..))
import ZkFold.ArithmeticCircuit (ArithmeticCircuit)
import ZkFold.ArithmeticCircuit qualified as Circuit

import ZkFold.Symbolic.Ledger.Circuit.Compile (ledgerCircuit)
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

goldenCircuit :: String -> ArithmeticCircuit a i o -> TestTree
goldenCircuit name circuit =
  Golden.goldenVsString name ("stats/" <> name) $ pure (metrics name circuit)

-- | Ledger circuit type parameters:
-- @bi@      - number of bridge-in outputs
-- @bo@      - number of bridge-out outputs
-- @ud@      - UTXO tree depth (2^(ud-1) leaves)
-- @a@       - number of assets per output
-- @ixs@     - number of inputs per transaction
-- @oxs@     - number of outputs per transaction
-- @txCount@ - number of transactions per batch
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Ledger circuit golden tests"
      [ goldenCircuit "Ledger.1.1.2.1.1.1.1" (ledgerCircuit @1 @1 @2 @1 @1 @1 @1 @RollupBFInterpreter)
      , goldenCircuit "Ledger.1.1.2.1.1.1.2" (ledgerCircuit @1 @1 @2 @1 @1 @1 @2 @RollupBFInterpreter)
      , goldenCircuit "Ledger.1.1.4.1.1.1.2" (ledgerCircuit @1 @1 @4 @1 @1 @1 @2 @RollupBFInterpreter)
      ]
