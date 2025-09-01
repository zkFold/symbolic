{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData (
  TransactionBatchDataWitness (..),
  validateTransactionBatchData,
  validateTransactionBatchDataWithIx,
) where

import Data.Function ((&), (.))
import GHC.Generics (Generic, Generic1, (:*:) (..))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq, (==))
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Hash (Hashable (..), preimage)
import ZkFold.Symbolic.Data.List (List, emptyList, (.:))
import qualified ZkFold.Symbolic.Data.List as Symbolic.List
import ZkFold.Symbolic.Data.Maybe
import Prelude (fst, undefined, ($))

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.Transaction.Core (validateTransactionWithAssetDiff)
import ZkFold.Data.Product (sndP, fstP)

-- | Witness needed to validate a 'TransactionBatchData'.
data TransactionBatchDataWitness context = TransactionBatchDataWitness
  { tbdwTransactions :: List Transaction context
  -- REVIEW: Add data availability index here to perhaps make slight simplification?
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance Signature context => Eq (TransactionBatchDataWitness context)

-- | This function extracts boolean from 'validateTransactionBatchDataWithIx, see it for more details.
validateTransactionBatchData
  :: forall context
   . Signature context
  => Interval context -> TransactionBatchData context -> TransactionBatchDataWitness context -> Bool context
validateTransactionBatchData tbInterval tbd tbdw = fst $ validateTransactionBatchDataWithIx tbInterval tbd tbdw

-- | Validate a 'TransactionBatchData'.
--
-- To check:
--   * All addresses corresponding to spent inputs have same data availability source.
--   * Merkle root is computed correctly.
--   * Online addresses list is computed correctly.
--   * Offline txs list is computed correctly.
--   * Interval of the overarching transaction batch is within the interval of individual transactions.
--   * Txs are valid.
--   * Batch as a whole is balanced.
validateTransactionBatchDataWithIx
  :: forall context
   . Signature context
  => Interval context
  -> TransactionBatchData context
  -> TransactionBatchDataWitness context
  -> (Bool context, DAIndex context)
validateTransactionBatchDataWithIx tbInterval TransactionBatchData {..} TransactionBatchDataWitness {..} =
  let -- Data availability index for this batch. Must not be @Nothing@.
      resTxAccIx
        :*: _resTxAccBatchInterval
        :*: -- Whether all relevant addresses have same data availability index.
            -- And whether the interval of the batch is within the interval of individual transactions.
            -- And transactions themselves are valid.
            resTxAccIsConsistent
        :*: -- List of online addresses corresponding to inputs that are being "spent". Note that this may contain duplicates which we'll need to remove later before comparing it with the field inside batch data.
            resTxAccOnlineAddresses
        :*: -- List of offline addresses along with their list of transaction hashes.
            resTxAccOfflineAddrsTxs
        :*: -- Tree root for online addresses with their transaction hashes.
            resTxAccOnlineAddrsTxs
        :*: -- Accumulated difference between outputs and inputs across all transactions.
            resTxAccValuesDiff
        =
          Symbolic.List.foldl
            (\( txAccIx
                :*: txAccBatchInterval
                :*: txAccIsConsistent
                :*: txAccOnlineAddresses
                :*: txAccOfflineAddrsTxs
                :*: txAccOnlineAddrsTxs
                :*: txAccValuesDiff
                ) tx ->
                    let txOwner' = txOwner tx
                        _ownerAddrCir :*: ownerAddrIx :*: ownerAddrType =
                          txOwner' & preimage
                        jownerAddrIx = just ownerAddrIx
                        -- If we haven't yet found any index, we use the index of this owner.
                        txAccIxFinal = ifThenElse (isNothing txAccIx) jownerAddrIx txAccIx
                        txHash = hasher tx
                        -- We assume that there is at least one input in the transaction from the address of the owner for following computation. Else the transaction validity check would fail.
                        newTxAccOfflineAddrsTxs
                          :*: newTxAccOnlineAddrsTxs
                          :*: newTxAccOnlineAddresses
                          =
                            ifThenElse
                              (isOffline ownerAddrType)
                              ( -- Add this tx hash to the list of tx hashes.
                                let updOwnerAddrTxHashes = txHash .: findAddrTxs txOwner' txAccOfflineAddrsTxs
                                 in -- Update the entry of this address with given list.
                                    -- TODO: We could probably do the findAddrTxs and replace in single folding. Circle back to it once symbolic list API is improved.
                                    updateAddrsTxsList txOwner' updOwnerAddrTxHashes txAccOfflineAddrsTxs
                                    :*: txAccOnlineAddrsTxs
                                    :*: txAccOnlineAddresses
                              )
                              (txAccOfflineAddrsTxs
                                :*: undefined -- TODO: Update once Merkle tree API is available.
                                :*: (txOwner' .: txAccOnlineAddresses))
                        (isTxValid, txValuesDiff) = validateTransactionWithAssetDiff tx
                        newTxAccIsConsistent =
                          txAccIsConsistent
                            && contains (txValidityInterval tx) txAccBatchInterval
                            && (txAccIxFinal == jownerAddrIx)
                            && isTxValid
                     in txAccIxFinal
                        :*: txAccBatchInterval
                        :*: newTxAccIsConsistent
                        :*: newTxAccOnlineAddresses
                        :*: newTxAccOfflineAddrsTxs
                        :*: newTxAccOnlineAddrsTxs
                        :*: addAssetValues txAccValuesDiff txValuesDiff
            )
            ( nothing
            :*: tbInterval
            :*: true
            :*: emptyList
            :*: emptyList
            :*: empty
            :*: emptyAssetValues
            )
            tbdwTransactions
   in ( isJust resTxAccIx
          && resTxAccIsConsistent
          && (tbdOnlineAddresses == removeDuplicates resTxAccOnlineAddresses)
          && (tbdOfflineTransactions == resTxAccOfflineAddrsTxs)
          && (tbdMerkleRoot == resTxAccOnlineAddrsTxs)
          && (resTxAccValuesDiff == emptyAssetValues)
      , fromJust resTxAccIx
      )

-- | Update the entry for the given address with given list of transaction hashes.
--
-- If the address does not exist, then it is prepended to this list.
updateAddrsTxsList
  :: forall context
   . Signature context
  => Address context
  -> List HashSimple context
  -> List (Address :*: List HashSimple) context
  -> List (Address :*: List HashSimple) context
updateAddrsTxsList addr addrTxs addrsTxs =
  let _ :*: _ :*: newAddrsTxs :*: addrExists =
        Symbolic.List.foldr
          (\ (iterAddr :*: iterAddrTxs)
             (accAddrToFind
               :*: accAddrTxs
               :*: accAddrsTxs
               :*: accFound
             ) ->
                  let elemMatches = accAddrToFind == iterAddr
                   in accAddrToFind
                      :*: accAddrTxs
                      :*: ifThenElse
                          elemMatches
                          ((iterAddr :*: accAddrTxs) .: accAddrsTxs)
                          ((iterAddr :*: iterAddrTxs) .: accAddrsTxs)
                      :*: (accFound || elemMatches)
          )
          (addr :*: addrTxs :*: emptyList :*: false)
          addrsTxs
   in ifThenElse
        addrExists
        newAddrsTxs
        ((addr :*: addrTxs) .: addrsTxs)

-- TODO: Refactor following once symbolic list is able to support more generic functions.

-- | Find a transaction hash list corresponding to given address.
--
-- If the address is not found, we return an empty list.
findAddrTxs
  :: forall context
   . Signature context
  => Address context
  -> List (Address :*: List HashSimple) context
  -> List HashSimple context
findAddrTxs a ls =
  sndP $
    Symbolic.List.foldl
      (\(accToFind :*: accList)
        (addr :*: addrTxHashes) ->
          accToFind
          :*: ifThenElse
                (accToFind == addr)
                addrTxHashes
                accList
      )
      (a :*: emptyList) ls

-- TODO: Use generic 'elem' from symbolic list module once available.

-- | Check if an item is present in the list.
elem
  :: forall context
   . Signature context
  => Address context
  -> List Address context
  -> Bool context
elem x =
  fstP . Symbolic.List.foldl
    (\(acc :*: givenElement) y -> (acc || givenElement == y) :*: givenElement)
    (false :*: x)

-- TODO: Use a generic function once available.

-- | Remove duplicates from list.
removeDuplicates
  :: forall context
   . Signature context
  => List Address context -> List Address context
removeDuplicates =
  Symbolic.List.foldr
    (\l acc -> ifThenElse (l `elem` acc) acc (l .: acc))
    emptyList
