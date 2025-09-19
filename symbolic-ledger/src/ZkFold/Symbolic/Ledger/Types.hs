{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Types (
  module ZkFold.Symbolic.Ledger.Types.Address,
  module ZkFold.Symbolic.Ledger.Types.Hash,
  module ZkFold.Symbolic.Ledger.Types.State,
  module ZkFold.Symbolic.Ledger.Types.Transaction,
  module ZkFold.Symbolic.Ledger.Types.Value,
  SignatureTransaction,
  SignatureTransactionBatch,
  SignatureState,
) where

import GHC.Generics ((:.:))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Hash (Hashable)

import ZkFold.Symbolic.Ledger.Types.Address
import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Symbolic.Ledger.Types.State
import ZkFold.Symbolic.Ledger.Types.Transaction
import ZkFold.Symbolic.Ledger.Types.Value

type SignatureTransaction i o a context =
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , Hashable (HashSimple context) (Transaction i o a context)
  , forall s. Hashable (HashSimple s) (Transaction i o a s)
  )

type SignatureTransactionBatch i o a t context =
  ( SignatureTransaction i o a context
  , Hashable (HashSimple context) (TransactionBatch i o a t context)
  , forall s. Hashable (HashSimple s) (TransactionBatch i o a t s)
  )

type SignatureState bi bo ud a context =
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , Hashable (HashSimple context) (State bi bo ud a context)
  , forall s. Hashable (HashSimple s) (State bi bo ud a s)
  , Hashable (HashSimple context) ((Vector bi :.: Output a) context)
  , Hashable (HashSimple context) ((Vector bo :.: Output a) context)
  )
