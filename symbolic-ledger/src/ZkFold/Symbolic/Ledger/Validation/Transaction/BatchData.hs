{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData where

import           Data.Function                    ((&))
import           Prelude                          (fst, undefined, ($))

import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Conditional (ifThenElse)
import           ZkFold.Symbolic.Data.Eq          ((==))
import           ZkFold.Symbolic.Data.Hash        (preimage)
import qualified ZkFold.Symbolic.Data.List        as Symbolic.List
import           ZkFold.Symbolic.Data.List        (List, emptyList, (++), (.:))
import           ZkFold.Symbolic.Data.Maybe
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types

data TransactionBatchDataWitness context = TransactionBatchDataWitness
  { tbdwTransactions :: List context (Transaction context)
  }

validateTransactionBatchData' :: forall context. Signature context => TransactionBatchData context -> TransactionBatchDataWitness context -> Bool context
validateTransactionBatchData' TransactionBatchData {..} TransactionBatchDataWitness {..} =
  -- To check
  -- \* All addresses corresponding to spent inputs have same data availability source.
  -- \* Merkle root is computed correctly.
  -- \* Online addresses list is computed correctly.
  -- \* Offline txs list is computed correctly.
  -- \* Txs are valid.
  -- TODO: Add more.
  let ( -- Data availability index for this batch. Must not be @Nothing@.
        resTxAccIx :: Maybe context (DAIndex context)
        , -- Wether all relevant addresses have same data availability index.
          resTxAccIsConsistent :: Bool context
        , -- List of online addresses corresponding to inputs that are being "spent". Note that this may contain duplicates which we'll need to remove later before comparing it with the field inside batch data.
          resTxAccOnlineAddresses :: List context (Address context)
        ) =
          Symbolic.List.foldl
            ( Morph \((txAccIx :: Maybe s (DAIndex s), txAccIsConsistent :: Bool s, txAccOnlineAddresses :: List s (Address s)), tx :: Transaction s) ->
                let (resInputsAccIx, resInputsAccIsConsistent, resInputsAccOnlineAddresses, _) =
                      Symbolic.List.foldl
                        ( Morph \((inputsAccIx :: Maybe s' (DAIndex s'), inputsAccIsConsistent :: Bool s', inputsAccOnlineAddresses :: List s' (Address s'), ownerAddr :: Address s'), input :: Input s') ->
                            let inputAddr = txoAddress (txiOutput input)
                                (_inputAddrCir, inputAddrIx, inputAddrType) = preimage inputAddr
                                minputAddrIx = just inputAddrIx
                                -- If index is not yet known, we update it with this input's index (provided this input is being "spent").
                                newIx = ifThenElse (isNothing inputsAccIx) minputAddrIx inputsAccIx
                                (newIsConsistent, newAddresses) =
                                  ifThenElse
                                    (inputAddr == ownerAddr)
                                    -- This input is being "spent" and thus it's address is relevant.
                                    ( inputsAccIsConsistent && newIx == minputAddrIx -- Index must match.
                                    , ifThenElse
                                        (isOnline inputAddrType)
                                        (inputAddr .: inputsAccOnlineAddresses)
                                        (inputsAccOnlineAddresses)
                                    )
                                    -- This input is not relevant, we skip it.
                                    (inputsAccIsConsistent, inputsAccOnlineAddresses)
                             in (newIx, newIsConsistent, newAddresses, ownerAddr)
                        )
                        (txAccIx, txAccIsConsistent, txAccOnlineAddresses, txOwner tx)
                        (txInputs tx)
                 in (resInputsAccIx, resInputsAccIsConsistent, resInputsAccOnlineAddresses)
            )
            (nothing :: Maybe context (DAIndex context), true :: Bool context, emptyList :: List context (Address context))
            tbdwTransactions
   in isJust resTxAccIx
        && resTxAccIsConsistent
        && (tbdOnlineAddresses == removeDuplicates resTxAccOnlineAddresses)

-- TODO: Use generic 'elem' from symbolic list module.

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

-- TODO: Use a generic function.

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
