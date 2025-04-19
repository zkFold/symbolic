module Examples.BatchTransfer (exampleBatchTransfer) where

import           ZkFold.Data.Vector                         (Vector)
import           ZkFold.Symbolic.Cardano.Contracts.BatchTransfer (Tx, TxOut, batchTransfer)
import           ZkFold.Symbolic.Cardano.Types                   (Bool, ByteString)
import           ZkFold.Symbolic.Class                           (Symbolic (..))
import           ZkFold.Symbolic.Data.Combinators                (KnownRegisters, RegisterSize (..))
import           ZkFold.Symbolic.Data.Input                      (SymbolicInput)

exampleBatchTransfer ::
    ( Symbolic c
    , SymbolicInput (TxOut c)
    , KnownRegisters c 64 'Auto
    , KnownRegisters c 256 'Auto
    )  => Tx c -> Vector 5 (TxOut c, TxOut c, ByteString 256 c) -> Bool c
exampleBatchTransfer = batchTransfer
