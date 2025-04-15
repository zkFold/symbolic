{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData (
  TransactionBatchDataWitness (..),
  validateTransactionBatchData,
  validateTransactionBatchDataWithIx,
) where

import           Data.Function                                      ((&))
import           GHC.Generics                                       (Generic)
import           Prelude                                            (fst, snd, undefined, ($))

import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class                         (SymbolicData)
import           ZkFold.Symbolic.Data.Conditional                   (Conditional, ifThenElse)
import           ZkFold.Symbolic.Data.Eq                            (Eq, (==))
import           ZkFold.Symbolic.Data.Hash                          (Hashable (..), preimage)
import qualified ZkFold.Symbolic.Data.List                          as Symbolic.List
import           ZkFold.Symbolic.Data.List                          (List, emptyList, (.:))
import           ZkFold.Symbolic.Data.Maybe
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types
import           ZkFold.Symbolic.Ledger.Validation.Transaction.Core (UTxO, validateTransaction)

-- | Witness needed to validate a 'TransactionBatchData'.
type TransactionBatchDataWitness context = List context (Transaction context)

data TransactionBatchDataAcc context = TransactionBatchDataAcc
  { txAccIx              :: Maybe context (DAIndex context)
  -- ^ Data availability index for this batch. Must not be @Nothing@.
  , txAccBatchInterval   :: Interval context
  -- ^ Interval of the overarching transaction batch.
  , txAccIsConsistent    :: Bool context
  -- ^ Whether all relevant addresses have same data availability index.
  , txAccOnlineAddresses :: List context (Address context)
  -- ^ List of online addresses corresponding to inputs that are being "spent".
  , txAccOfflineAddrsTxs :: List context (Address context, List context (HashSimple context))
  -- ^ List of offline addresses along with their list of transaction hashes.
  , txAccOnlineAddrsTxs  :: Root (Address context, List context (HashSimple context))
  -- ^ Tree root for online addresses with their transaction hashes.
  , txAccValuesDiff      :: AssetValues context
  -- ^ Accumulated difference between outputs and inputs across all transactions.
  , txAccUtxos           :: UTxO context
  -- ^ UTxO set.
  }
  deriving stock Generic

instance Signature context => SymbolicData (TransactionBatchDataAcc context)

instance Signature context => Conditional (Bool context) (TransactionBatchDataAcc context)

instance Signature context => Eq (TransactionBatchDataAcc context)

-- | This function extracts boolean from 'validateTransactionBatchDataWithIx, see it for more details.
validateTransactionBatchData ::
  forall context.
  Signature context =>
  Interval context ->
  TransactionBatchData context ->
  TransactionBatchDataWitness context ->
  UTxO context ->
  Bool context
validateTransactionBatchData tbInterval tbd tbdw utxos = fst $ validateTransactionBatchDataWithIx tbInterval tbd tbdw utxos

{- | Validate a 'TransactionBatchData'.

To check:
  * All addresses corresponding to spent inputs have same data availability source.
  * Merkle root is computed correctly.
  * Online addresses list is computed correctly.
  * Offline txs list is computed correctly.
  * Interval of the overarching transaction batch is within the interval of individual transactions.
  * Txs are valid.
  * Batch as a whole is balanced.
-}
validateTransactionBatchDataWithIx ::
  forall context.
  Signature context =>
  Interval context ->
  TransactionBatchData context ->
  TransactionBatchDataWitness context ->
  UTxO context ->
  (Bool context, DAIndex context)
validateTransactionBatchDataWithIx tbInterval TransactionBatchData {..} tbdwTransactions utxos =
  let TransactionBatchDataAcc {..} :: TransactionBatchDataAcc context =
        Symbolic.List.foldl
          ( Morph
              \( TransactionBatchDataAcc {..} :: TransactionBatchDataAcc s
                , (tx :: Transaction s)
                ) ->
                  let txOwner' = txOwner tx
                      (_ownerAddrCir, ownerAddrIx, ownerAddrType) = txOwner' & preimage
                      jownerAddrIx = just ownerAddrIx
                      -- If we haven't yet found any index, we use the index of this owner.
                      txAccIxFinal = ifThenElse (isNothing txAccIx) jownerAddrIx txAccIx
                      txHash = hasher tx
                      -- We assume that there is at least one input in the transaction from the address of the owner for following computation. Else the transaction validity check would fail.
                      (newTxAccOfflineAddrsTxs, newTxAccOnlineAddrsTxs, newTxAccOnlineAddresses) =
                        ifThenElse
                          (isOffline ownerAddrType)
                          ( -- Add this tx hash to the list of tx hashes.
                            let updOwnerAddrTxHashes = txHash .: findAddrTxs txOwner' txAccOfflineAddrsTxs
                             in -- Update the entry of this address with given list.
                                -- TODO: We could probably do the findAddrTxs and replace in single folding. Circle back to it once symbolic list API is improved.
                                (updateAddrsTxsList txOwner' updOwnerAddrTxHashes txAccOfflineAddrsTxs, txAccOnlineAddrsTxs, txAccOnlineAddresses)
                          )
                          (txAccOfflineAddrsTxs, undefined, txOwner' .: txAccOnlineAddresses) -- TODO: Update once Merkle tree API is available.
                      (isTxValid, txValuesDiff) = validateTransaction tx txAccUtxos
                      newTxAccIsConsistent =
                        txAccIsConsistent
                          && (contains (txValidityInterval tx) txAccBatchInterval)
                          && (txAccIxFinal == jownerAddrIx)
                          && isTxValid
                   in TransactionBatchDataAcc
                        { txAccIx = txAccIxFinal
                        , txAccBatchInterval = txAccBatchInterval
                        , txAccIsConsistent = newTxAccIsConsistent
                        , txAccOnlineAddresses = newTxAccOnlineAddresses
                        , txAccOfflineAddrsTxs = newTxAccOfflineAddrsTxs
                        , txAccOnlineAddrsTxs = newTxAccOnlineAddrsTxs
                        , txAccValuesDiff = addAssetValues txAccValuesDiff txValuesDiff
                        , txAccUtxos = txAccUtxos
                        }
          )
          ( TransactionBatchDataAcc
              { txAccIx = nothing
              , txAccBatchInterval = tbInterval
              , txAccIsConsistent = true
              , txAccOnlineAddresses = emptyList
              , txAccOfflineAddrsTxs = emptyList
              , txAccOnlineAddrsTxs = empty
              , txAccValuesDiff = emptyAssetValues
              , txAccUtxos = utxos
              }
          )
          (tbdwTransactions)
   in ( isJust txAccIx
          && txAccIsConsistent
          && (tbdOnlineAddresses == removeDuplicates txAccOnlineAddresses)
          && (tbdOfflineTransactions == txAccOfflineAddrsTxs)
          && (tbdMerkleRoot == txAccOnlineAddrsTxs)
          && (txAccValuesDiff == emptyAssetValues)
      , fromJust txAccIx
      )

{- | Update the entry for the given address with given list of transaction hashes.

If the address does not exist, then it is prepended to this list.
-}
updateAddrsTxsList ::
  forall context.
  Signature context =>
  Address context ->
  List context (HashSimple context) ->
  List context (Address context, List context (HashSimple context)) ->
  List context (Address context, List context (HashSimple context))
updateAddrsTxsList addr addrTxs addrsTxs =
  let (_, _, newAddrsTxs, addrExists) =
        Symbolic.List.foldr
          ( Morph \(((iterAddr :: Address s, iterAddrTxs :: List s (HashSimple s))), (accAddrToFind :: Address s, accAddrTxs :: List s (HashSimple s), accAddrsTxs :: List s (Address s, List s (HashSimple s)), accFound :: Bool s)) ->
              let elemMatches = accAddrToFind == iterAddr
               in ( accAddrToFind
                  , accAddrTxs
                  , ifThenElse
                      elemMatches
                      ((iterAddr, accAddrTxs) .: accAddrsTxs)
                      ((iterAddr, iterAddrTxs) .: accAddrsTxs)
                  , accFound || elemMatches
                  )
          )
          (addr, addrTxs, emptyList :: List context (Address context, List context (HashSimple context)), false :: Bool context)
          addrsTxs
   in ifThenElse
        addrExists
        newAddrsTxs
        ((addr, addrTxs) .: addrsTxs)

-- TODO: Refactor following once symbolic list is able to support more generic functions.

{- | Find a transaction hash list corresponding to given address.

If the address is not found, we return an empty list.
-}
findAddrTxs ::
  forall context.
  Signature context =>
  Address context ->
  List context (Address context, List context (HashSimple context)) ->
  List context (HashSimple context)
findAddrTxs a ls =
  snd $
    Symbolic.List.foldl
      ( Morph \((accToFind :: Address s, accList :: List s (HashSimple s)), ((addr :: Address s, addrTxHashes :: List s (HashSimple s)))) ->
          ( accToFind
          , ifThenElse
              (accToFind == addr)
              addrTxHashes
              accList
          )
      )
      (a, emptyList :: List context (HashSimple context))
      ls

-- TODO: Use generic 'elem' from symbolic list module once available.

-- | Check if an item is present in the list.
elem ::
  forall context.
  Signature context =>
  Address context ->
  List context (Address context) ->
  Bool context
elem x xs =
  fst $
    Symbolic.List.foldl
      ( Morph \((acc :: Bool s, givenElement :: Address s), y :: Address s) ->
          ( acc || givenElement == y
          , givenElement
          )
      )
      (false :: Bool context, x :: Address context)
      xs

-- TODO: Use a generic function once available.

-- | Remove duplicates from list.
removeDuplicates ::
  forall context.
  Signature context =>
  List context (Address context) -> List context (Address context)
removeDuplicates ls =
  Symbolic.List.foldr
    ( Morph \(l :: Address s, acc :: List s (Address s)) ->
        ifThenElse
          (elem l acc)
          acc
          (l .: acc)
    )
    emptyList
    ls
