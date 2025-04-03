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

data TransactionBatchDataWitness context = TransactionBatchDataWitness {
  tbdwTransactions :: List context (Transaction context)
}

validateTransactionBatchData' :: forall context. Signature context => TransactionBatchData context -> TransactionBatchDataWitness context -> Bool context
validateTransactionBatchData' TransactionBatchData {..} TransactionBatchDataWitness {..} =
  -- To check
  -- * All addresses corresponding to spent inputs have same data availability source.
  -- * Merkle root is computed correctly.
  -- * Online addresses list is computed correctly.
  -- * Offline txs list is computed correctly.
  -- * Txs are valid.
  -- TODO: Add more.
  let (_resTxAccIx :: Maybe context (DAIndex context), resTxAccIsConsistent :: Bool context
       , resTxAccAddresses :: List context (Address context)) =
        Symbolic.List.foldl (
        -- TODO: Remove type annotations as much as possible.
          Morph \((txAccIx :: Maybe s (DAIndex s), txAccIsConsistent :: Bool s, txAccAddresses :: List s (Address s)), tx :: Transaction s) ->
            let (resInputsAccIx, resInputsAccIsConsistent, resInputsAccAddresses, _) =
                  Symbolic.List.foldl (
                    Morph \((inputsAccIx :: Maybe s' (DAIndex s'), inputsAccIsConsistent :: Bool s', inputsAccAddresses :: List s' (Address s'), ownerAddr :: Address s'), input :: Input s') ->
                      let inputAddr = txoAddress (txiOutput input)
                          (_inputAddrCir, inputAddrIx, inputAddrType) = preimage inputAddr
                          minputAddrIx = just inputAddrIx
                          newIx = ifThenElse (isNothing inputsAccIx) minputAddrIx inputsAccIx
                          (newIsConsistent, newAddresses) =
                            ifThenElse (inputAddr == ownerAddr)
                              ( inputsAccIsConsistent && newIx == minputAddrIx
                              , ifThenElse (isOnline inputAddrType)
                                  (inputAddr .: inputsAccAddresses)
                                  (inputsAccAddresses)
                              )
                              (inputsAccIsConsistent, inputsAccAddresses)
                      in (newIx, newIsConsistent, newAddresses, ownerAddr)
                  ) (txAccIx, txAccIsConsistent, emptyList :: List s (Address s), txOwner tx) (txInputs tx)
                in (resInputsAccIx, resInputsAccIsConsistent, txAccAddresses ++ resInputsAccAddresses)
        ) (nothing :: Maybe context (DAIndex context), true :: Bool context, emptyList :: List context (Address context)) tbdwTransactions
  in
      resTxAccIsConsistent
   && (tbdAddresses == removeDuplicates resTxAccAddresses)

-- TODO: Use generic 'elem' from symbolic list module.
-- | Check if an item is present in the list.
elem ::
  forall context.
  Signature context =>
  Address context ->
  List context (Address context) -> Bool context
elem x xs =
  fst $
    Symbolic.List.foldl (
      Morph \((acc :: Bool s, givenElement :: Address s), y :: Address s) ->
        ( acc || givenElement == y
        , givenElement
        )
    ) (false :: Bool context, x :: Address context) xs

-- TODO: Use a generic function.
-- | Remove duplicates from list.
removeDuplicates ::
  forall context.
  Signature context =>
  List context (Address context) -> List context (Address context)
removeDuplicates ls =
  Symbolic.List.foldr (Morph \(l :: Address s, acc :: List s (Address s)) ->
          ifThenElse (elem l acc)
            acc
            (l .: acc)
        ) emptyList ls
