{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example demonstrating that null transactions can be used as batch padding.
-- A "null transaction" has all null inputs and all null outputs. It is a pure
-- no-op on the UTxO state and passes validation.
module ZkFold.Symbolic.Ledger.Examples.Four (
  prevState,
  batch,
  witness,
  newState,
  utxoPreimage2,
  tx1,
  nullTx,
  sigs,
  bridgedIn,
  I,
  Bi,
  Bo,
  Ud,
  A,
  N,
  TxCount,
) where

import Control.Applicative (pure)
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.IsList (IsList (..))
import ZkFold.Symbolic.Data.Bool (false)

import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Ledger.Examples.One (
  A,
  Bi,
  Bo,
  I,
  N,
  S,
  Ud,
  bridgedIn,
  emptyTree,
  prevState,
  privateKey,
  publicKey,
  tx,
 )
import ZkFold.Symbolic.Ledger.Offchain.State.Update (updateLedgerState)
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

-- | Batch of 2: one real transaction followed by one null transaction used as padding.
type TxCount = 2

-- | The real transaction (same as 'tx' from Examples.One).
tx1 :: Transaction N A I
tx1 = tx

-- | A null transaction: all null inputs and all null outputs.
-- Intended as a padding transaction to fill a fixed-size batch.
nullTx :: Transaction N A I
nullTx =
  Transaction
    { inputs = Comp1 (fromList [nullOutputRef])
    , outputs = Comp1 (fromList [nullOutput @A @I :*: false])
    }

batch :: TransactionBatch N A TxCount I
batch = TransactionBatch {tbTransactions = unsafeToVector' [tx1, nullTx]}

sigs :: (Vector TxCount :.: (Vector S :.: (PublicKey :*: EdDSAPoint :*: EdDSAScalarField))) I
sigs =
  let rPoint :*: s = signTransaction tx1 privateKey
      -- Any signature works for the null transaction since its null inputs
      -- skip signature verification (allInputsNull = true).
      dummyRPoint :*: dummyS = rPoint :*: s
   in Comp1
        ( unsafeToVector'
            [ Comp1 (fromList [publicKey :*: rPoint :*: s])
            , Comp1 (fromList [publicKey :*: dummyRPoint :*: dummyS])
            ]
        )

newState :*: witness :*: _utxoTree2 :*: utxoPreimage2 =
  updateLedgerState prevState emptyTree (pure (nullUTxO @A @I)) bridgedIn batch sigs
