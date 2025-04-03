{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}
module ZkFold.Symbolic.Ledger.Validation.Transaction.BatchData where
import           Data.Function                    ((&))

import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Conditional (ifThenElse)
import           ZkFold.Symbolic.Data.Eq          ((==))
import           ZkFold.Symbolic.Data.Hash        (preimage)
import qualified ZkFold.Symbolic.Data.List        as Symbolic.List
import           ZkFold.Symbolic.Data.List        (List)
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
  let (_resTxIx :: Maybe context (DAIndex context), resTxAccIsConsistent :: Bool context) = 
        Symbolic.List.foldr (  -- TODO: Document why we use foldr here.
        -- TODO: Remove type annotations as much as possible.
          Morph \(tx :: Transaction s, (txAccIx :: Maybe s (DAIndex s), txAccIsConsistent :: Bool s)) ->
            let (resInputsAccIx, resInputsAccIsConsistent, _) = 
                  Symbolic.List.foldl (
                    Morph \((inputsAccIx :: Maybe s' (DAIndex s'), inputsAccIsConsistent :: Bool s', ownerAddr :: Address s'), input :: Input s') ->
                      let inputAddr = txoAddress (txiOutput input)
                          inputAddrIx = preimage inputAddr & (\(_, b, _) -> b) & just
                          newIx = ifThenElse (isNothing inputsAccIx) inputAddrIx inputsAccIx
                          newIsConsistent = ifThenElse (inputAddr == ownerAddr) (inputsAccIsConsistent && newIx == inputAddrIx) inputsAccIsConsistent
                      in (newIx, newIsConsistent, ownerAddr)
                  ) (txAccIx, txAccIsConsistent, txOwner tx) (txInputs tx) 
                in (resInputsAccIx, resInputsAccIsConsistent)
        ) (nothing :: Maybe context (DAIndex context), true :: Bool context) tbdwTransactions
  in resTxAccIsConsistent
