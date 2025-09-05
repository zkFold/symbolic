{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Types (
  module ZkFold.Symbolic.Ledger.Types.Address,
  module ZkFold.Symbolic.Ledger.Types.Hash,
  module ZkFold.Symbolic.Ledger.Types.Transaction,
  module ZkFold.Symbolic.Ledger.Types.Root,
  module ZkFold.Symbolic.Ledger.Types.Value,
  Signature,
) where

-- Re-exports

import GHC.TypeLits (KnownNat)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Combinators (
  Ceil,
  GetRegisterSize,
  KnownRegisters,
  RegisterSize (Auto),
 )
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Data.UInt (OrdWord)
import ZkFold.Symbolic.Fold (SymbolicFold)
import ZkFold.Symbolic.Ledger.Types.Address
import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Symbolic.Ledger.Types.Root
import ZkFold.Symbolic.Ledger.Types.Transaction
import ZkFold.Symbolic.Ledger.Types.Value

{-
    zkFold's ledger is a UTXO-based ledger. The architecture of the ledger is mostly similar to the Cardano ledger with some key differences:

    - Some transaction data is private and is kept off-chain by the concerned parties.

    - All UTXOs are locked by contracts.

    - Stake delegation and governance is implemented through contracts.
-}

type Signature context t =
  ( KnownRegistersAssetQuantity context
  , KnownRegisters context 11 Auto
  , SymbolicFold context
  , KnownNat (Ceil (GetRegisterSize (BaseField context) 11 Auto) OrdWord)
  , -- TODO: Can we derive 'Hashable h' based on constituents (using generic)?
    -- TODO: Remove @ImpredicativeTypes@ extension from symbolic-ledger once above 'Hashable' issue is sorted.
    Hashable (HashSimple context) (AssetValues context)
  , Hashable (HashSimple context) (Transaction context)
  , Hashable (HashSimple context) (TransactionBatch context t)
  , forall s. Hashable (HashSimple s) (AssetValues s)
  , forall s. Hashable (HashSimple s) (Transaction s)
  , forall s. Hashable (HashSimple s) (TransactionBatch s t)
  )
