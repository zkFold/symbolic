{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction (
  TransactionWitness (..),
  validateTransaction,
) where

import Data.Function ((&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (AdditiveSemigroup (..), MultiplicativeMonoid (..), Zero (..))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.Vector (Vector, Zip (..))
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (hash)
import qualified ZkFold.Symbolic.Data.Hash as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry, MerkleTree)
import qualified ZkFold.Symbolic.Data.MerkleTree as MerkleTree

import ZkFold.Symbolic.Ledger.Types

data TransactionWitness ud i o a context = TransactionWitness
  { twInputs :: (Vector i :.: (MerkleEntry ud :*: UTxO a)) context
  , twOutputs :: (Vector o :.: MerkleEntry ud) context
  }

validateTransaction
  :: forall ud bo i o a context
   . SignatureTransaction ud i o a context
  => TransactionWitness ud i o a context
  -> MerkleTree ud context
  -> (Vector bo :.: Output a) context
  -> Transaction i o a context
  -> (FieldElement :*: Bool :*: MerkleTree ud) context
validateTransaction txw utxoTree bridgedOutOutputs tx =
  let
    txId' = txId tx & Base.hHash
    inputsWithWitness = zipWith (:*:) (unComp1 tx.inputs) (unComp1 txw.twInputs)
    (isInsValid :*: updatedUTxOTreeForInputs) =
      foldl'
        ( \(isInsValidAcc :*: acc) (inputRef :*: (merkleEntry :*: utxo)) ->
            let
              utxoHash :: HashSimple context = hash utxo & Base.hHash
              isValid' =
                isInsValidAcc
                  && (inputRef == utxo.uRef)
                  && (utxoHash == MerkleTree.value merkleEntry)
                  && (acc `MerkleTree.contains` merkleEntry)
                  && (utxo /= nullUTxO)
             in
              ( isValid'
                  :*: MerkleTree.replace
                    ( merkleEntry
                        { MerkleTree.value = nullUTxOHash @a @context
                        }
                    )
                    acc
              )
        )
        ((true :: Bool context) :*: utxoTree)
        inputsWithWitness
    outputsWithWitness = zipWith (:*:) (unComp1 tx.outputs) (unComp1 txw.twOutputs)
    (bouts :*: _ :*: boutsValid :*: updatedUTxOTreeForOutputs) =
      foldl'
        ( \(boutsAcc :*: outputIx :*: boutsValidAcc :*: utxoTreeAcc) ((output :*: bout) :*: merkleEntry) ->
            ifThenElse
              bout
              ( (boutsAcc + one)
                  :*: (outputIx + one)
                  :*: ( boutsValidAcc
                          && foldl' (\found boutput -> found || output == boutput) false (unComp1 bridgedOutOutputs)
                      )
                  :*: utxoTreeAcc
              )
              ( boutsAcc
                  :*: (outputIx + one)
                  :*: (boutsValidAcc && (utxoTreeAcc `MerkleTree.contains` merkleEntry) && (merkleEntry.value == nullUTxOHash @a @context))
                  :*: ( let utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = outputIx}, uOutput = output}
                         in MerkleTree.replace
                              ( merkleEntry
                                  { MerkleTree.value = hash utxo & Base.hHash
                                  }
                              )
                              utxoTreeAcc
                      )
              )
        )
        (zero :*: zero :*: (true :: Bool context) :*: updatedUTxOTreeForInputs)
        outputsWithWitness
   in
    (bouts :*: (boutsValid && isInsValid) :*: updatedUTxOTreeForOutputs)
