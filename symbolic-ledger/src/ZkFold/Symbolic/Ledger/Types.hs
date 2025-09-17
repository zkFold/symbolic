{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Types (
  module ZkFold.Symbolic.Ledger.Types.Address,
  module ZkFold.Symbolic.Ledger.Types.Hash,
  module ZkFold.Symbolic.Ledger.Types.State,
  module ZkFold.Symbolic.Ledger.Types.Transaction,
  module ZkFold.Symbolic.Ledger.Types.Root,
  module ZkFold.Symbolic.Ledger.Types.Value,
  SignatureTransaction,
  SignatureTransactionBatch,
  SignatureState,
) where

-- Re-exports

import GHC.Generics ((:*:), (:.:))
import GHC.TypeLits (KnownNat)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Ledger.Types.Address
import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Symbolic.Ledger.Types.Root
import ZkFold.Symbolic.Ledger.Types.State
import ZkFold.Symbolic.Ledger.Types.Transaction
import ZkFold.Symbolic.Ledger.Types.Value

{-
    zkFold's ledger is a UTXO-based ledger. The architecture of the ledger is mostly similar to the Cardano ledger with some key differences:

    - Some transaction data is private and is kept off-chain by the concerned parties.

    - All UTXOs are locked by contracts.

    - Stake delegation and governance is implemented through contracts.
-}

type SignatureTransaction context =
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , Hashable (HashSimple context) (Transaction context)
  , forall s. Hashable (HashSimple s) (Transaction s)
  )

type SignatureTransactionBatch context t =
  ( SignatureTransaction context
  , KnownNat t
  , Hashable (HashSimple context) (TransactionBatch t context)
  , forall s. Hashable (HashSimple s) (TransactionBatch t s)
  )

type SignatureState context bi bo =
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , KnownNat bi
  , KnownNat bo
  , Hashable (HashSimple context) (State bi bo context)
  , forall s. Hashable (HashSimple s) (State bi bo s)
  , Hashable (HashSimple context) ((Vector bi :.: (Address :*: AssetValue)) context)
  , Hashable (HashSimple context) ((Vector bo :.: (Address :*: Address :*: AssetValue)) context)
  )
