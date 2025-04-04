module ZkFold.Symbolic.Ledger.Types (
    module ZkFold.Symbolic.Ledger.Types.Address,
    module ZkFold.Symbolic.Ledger.Types.Hash,
    module ZkFold.Symbolic.Ledger.Types.Output,
    module ZkFold.Symbolic.Ledger.Types.Transaction,
    module ZkFold.Symbolic.Ledger.Types.Root,
    module ZkFold.Symbolic.Ledger.Types.Datum,
    module ZkFold.Symbolic.Ledger.Types.Value,
    module ZkFold.Symbolic.Ledger.Types.DataAvailability,
    module ZkFold.Symbolic.Ledger.Types.Circuit,
    Signature
) where

-- Re-exports

import           ZkFold.Symbolic.Class                         (Symbolic)
import           ZkFold.Symbolic.Data.Combinators              (KnownRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Fold                          (SymbolicFold)
import           ZkFold.Symbolic.Ledger.Types.Address
import           ZkFold.Symbolic.Ledger.Types.Circuit
import           ZkFold.Symbolic.Ledger.Types.DataAvailability
import           ZkFold.Symbolic.Ledger.Types.Datum
import           ZkFold.Symbolic.Ledger.Types.Hash
import           ZkFold.Symbolic.Ledger.Types.Output
import           ZkFold.Symbolic.Ledger.Types.Root
import           ZkFold.Symbolic.Ledger.Types.Transaction
import           ZkFold.Symbolic.Ledger.Types.Value

{-
    zkFold's ledger is a UTXO-based ledger. The architecture of the ledger is mostly similar to the Cardano ledger with some key differences:

    - Some transaction data is private and is kept off-chain by the concerned parties.

    - All UTXOs are locked by contracts.

    - Stake delegation and governance is implemented through contracts.
-}

type Signature context =
    ( KnownRegistersAssetQuantity context
    , KnownRegistersOutputIndex context
    , KnownRegisters context 11 Auto
    , Symbolic context
    , SymbolicFold context
    )
