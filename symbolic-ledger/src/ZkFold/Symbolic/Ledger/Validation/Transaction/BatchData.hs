{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData (
  TransactionBatchDataWitness (..),
  validateTransactionBatchData,
  validateTransactionBatchData',
) where

import           Data.Function                    ((&))
import           GHC.Generics                     (Generic)
import           Prelude                          (fst, snd, undefined, ($))

import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class       (SymbolicData)
import           ZkFold.Symbolic.Data.Conditional (Conditional, ifThenElse)
import           ZkFold.Symbolic.Data.Eq          (Eq, (==))
import           ZkFold.Symbolic.Data.Hash        (Hash (..), hash, preimage)
import qualified ZkFold.Symbolic.Data.List        as Symbolic.List
import           ZkFold.Symbolic.Data.List        (List, emptyList, (.:))
import           ZkFold.Symbolic.Data.Maybe       hiding (findAddrTxs)
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types

-- | Witness needed to validate a 'TransactionBatchData'.
data TransactionBatchDataWitness context = TransactionBatchDataWitness
  { tbdwTransactions :: List context (Transaction context)
  -- REVIEW: Add data availability index here to perhaps make slight simplification?
  }
  deriving stock Generic

instance Signature context => SymbolicData (TransactionBatchDataWitness context)

instance Signature context => Conditional (Bool context) (TransactionBatchDataWitness context)

instance Signature context => Eq (TransactionBatchDataWitness context)

-- | This function extracts boolean from 'validateTransactionBatchData', see it for more details.
validateTransactionBatchData :: forall context. Signature context => Interval context -> TransactionBatchData context -> TransactionBatchDataWitness context -> Bool context
validateTransactionBatchData tbInterval tbd tbdw = fst $ validateTransactionBatchData' tbInterval tbd tbdw

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
validateTransactionBatchData' :: forall context. Signature context => Interval context -> TransactionBatchData context -> TransactionBatchDataWitness context -> (Bool context, DAIndex context)
validateTransactionBatchData' tbInterval TransactionBatchData {..} TransactionBatchDataWitness {..} =
  let ( -- Data availability index for this batch. Must not be @Nothing@.
        resTxAccIx :: Maybe context (DAIndex context)
        , _resTxAccBatchInterval :: Interval context
        , -- Whether all relevant addresses have same data availability index.
          -- And whether the interval of the batch is within the intervals of individual transactions.
          resTxAccIsConsistent :: Bool context
        , -- List of online addresses corresponding to inputs that are being "spent". Note that this may contain duplicates which we'll need to remove later before comparing it with the field inside batch data.
          resTxAccOnlineAddresses :: List context (Address context)
        , -- List of offline addresses along with their list of transaction hashes.
          resTxAccOfflineAddrsTxs :: List context (Address context, List context (HashSimple context))
        , -- Tree root for online addresses with their transaction hashes.
          resTxAccOnlineAddrsTxs :: Root (Address context, List context (HashSimple context))
        ) =
          Symbolic.List.foldl
            ( Morph \((txAccIx :: Maybe s (DAIndex s), txAccBatchInterval :: Interval s, txAccIsConsistent :: Bool s, txAccOnlineAddresses :: List s (Address s), txAccOfflineAddrsTxs :: List s (Address s, List s (HashSimple s)), txAccOnlineAddrsTxs :: Root (Address s, List s (HashSimple s))), tx :: Transaction s) ->
                let txOwner' = txOwner tx
                    (_ownerAddrCir, ownerAddrIx, ownerAddrType) = txOwner' & preimage
                    -- If we haven't found any index, we use the index of this owner.
                    txAccIxFinal = ifThenElse (isNothing txAccIx) (just ownerAddrIx) txAccIx
                    txHash = hash tx & hHash
                    -- We assume that there is at least one input in the transaction from the address of the owner for following computation. Else the transaction validity check would fail.
                    (newTxAccOfflineAddrsTxs, newTxAccOnlineAddrsTxs, newTxAccOnlineAddresses) =
                      ifThenElse
                        (isOffline ownerAddrType)
                        ( -- Add this tx hash to the list of tx hashes.
                          let updOwnerAddrTxHashes = txHash .: findAddrTxs txOwner' txAccOfflineAddrsTxs
                           in -- Update the entry of this address with given list.
                              -- TODO: We could probably do the findAddrTxs and replace in single folding.
                              (updateAddrsTxsList txOwner' updOwnerAddrTxHashes txAccOfflineAddrsTxs, txAccOnlineAddrsTxs, txAccOnlineAddresses)
                        )
                        (txAccOfflineAddrsTxs, undefined, txOwner' .: txAccOnlineAddresses) -- TODO: Update once Merkle tree API is available.
                    newTxAccIsConsistent = txAccIsConsistent && (contains (txValidityInterval tx) txAccBatchInterval)
                    (resInputsAccIx, resInputsAccIsConsistent, _) =
                      Symbolic.List.foldl
                        ( Morph \((inputsAccIx :: Maybe s' (DAIndex s'), inputsAccIsConsistent :: Bool s', ownerAddr :: Address s'), input :: Input s') ->
                            let inputAddr = txoAddress (txiOutput input)
                                (_inputAddrCir, inputAddrIx, _inputAddrType) = preimage inputAddr -- If folding operation does not require context switching, we won't require fetching this pre-image here.
                                minputAddrIx = just inputAddrIx
                                newIsConsistent =
                                  ifThenElse
                                    (inputAddr == ownerAddr)
                                    -- This input is being "spent" and thus it's address is relevant.
                                    (inputsAccIsConsistent && inputsAccIx == minputAddrIx) -- Index must match.
                                    -- This input is not relevant, we skip it.
                                    (inputsAccIsConsistent)
                             in (inputsAccIx, newIsConsistent, ownerAddr)
                        )
                        (txAccIxFinal, newTxAccIsConsistent, txOwner')
                        (txInputs tx)
                 in (resInputsAccIx, txAccBatchInterval, resInputsAccIsConsistent, newTxAccOnlineAddresses, newTxAccOfflineAddrsTxs, newTxAccOnlineAddrsTxs)
            )
            (nothing :: Maybe context (DAIndex context), tbInterval, true :: Bool context, emptyList :: List context (Address context), emptyList :: List context (Address context, List context (HashSimple context)), empty :: Root (Address context, List context (HashSimple context)))
            tbdwTransactions
   in ( isJust resTxAccIx
          && resTxAccIsConsistent
          && (tbdOnlineAddresses == removeDuplicates resTxAccOnlineAddresses)
          && (tbdOfflineTransactions == resTxAccOfflineAddrsTxs)
          && (tbdMerkleRoot == resTxAccOnlineAddrsTxs)
      , fromJust resTxAccIx
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
