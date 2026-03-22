{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ZkFold.Symbolic.Ledger.Examples.Five (
  prevState,
  batch,
  batch2,
  witness,
  newState,
  utxoPreimage2,
  asset2Policy,
  asset2Name,
  address,
  address2,
  tx1,
  tx2,
  tx3,
  tx4,
  sigs,
  sigs2,
  bridgeInOutput,
  bridgedIn,
  bridgeOutOutput,
  bridgedIn2,
  newState2,
  witness2,
  utxoPreimage3,
  I,
  Bi,
  Bo,
  Ud,
  A,
  S,
  N,
  TxCount,
) where

import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.IsList (IsList (..))

import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Ledger.Offchain.State.Update (updateLedgerState)
import ZkFold.Symbolic.Ledger.Types

import ZkFold.Symbolic.Ledger.Examples.Three (
  A,
  Bi,
  Bo,
  I,
  N,
  S,
  SigEntry,
  Ud,
  address,
  address2,
  asset2Name,
  asset2Policy,
  bridgeInOutput,
  bridgeOutOutput,
  bridgedIn,
  emptyTree,
  makeBatch,
  makeSigs,
  sigEntries1,
  sigEntries2,
  tx1,
  tx2,
  tx3,
  tx4,
  utxoPreimage,
 )
import ZkFold.Symbolic.Ledger.Examples.Three qualified as Three (prevState)

type TxCount = 4

prevState :: State Ud A I
prevState = Three.prevState

batch :: TransactionBatch N A TxCount I
batch = makeBatch @TxCount [tx1, tx2]

batch2 :: TransactionBatch N A TxCount I
batch2 = makeBatch @TxCount [tx3, tx4]

sigs :: (Vector TxCount :.: (Vector S :.: (PublicKey :*: EdDSAPoint :*: EdDSAScalarField))) I
sigs = makeSigs @TxCount sigEntries1

sigs2 :: (Vector TxCount :.: (Vector S :.: (PublicKey :*: EdDSAPoint :*: EdDSAScalarField))) I
sigs2 = makeSigs @TxCount sigEntries2

newState :*: witness :*: utxoTree2 :*: utxoPreimage2 = updateLedgerState @Bi @Bo prevState emptyTree utxoPreimage bridgedIn batch sigs

bridgedIn2 :: (Vector Bi :.: Output A) I
bridgedIn2 = Comp1 (fromList [nullOutput @A @I])

newState2 :*: witness2 :*: _utxoTree3 :*: utxoPreimage3 = updateLedgerState @Bi @Bo newState utxoTree2 (unComp1 utxoPreimage2) bridgedIn2 batch2 sigs2
