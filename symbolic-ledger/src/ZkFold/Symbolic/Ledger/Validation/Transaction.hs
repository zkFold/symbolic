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
import qualified Prelude as P

data TransactionWitness ud i o a context = TransactionWitness
  { twInputs :: (Vector i :.: (MerkleEntry ud :*: UTxO a)) context
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
    (bouts :*: boutsIncluded) =
      foldl'
        ( \(boutsAcc :*: boutsIncludedAcc) (output :*: bout) ->
            ifThenElse
              bout
              ( (boutsAcc + one)
                  :*: ( boutsIncludedAcc
                          && foldl' (\found (boutput) -> found || output == boutput) false (unComp1 bridgedOutOutputs)
                      )
              )
              (boutsAcc :*: boutsIncludedAcc)
        )
        (zero :*: (true :: Bool context))
        (unComp1 tx.outputs)
   in
    (bouts :*: (boutsIncluded && isInsValid) :*: utxoTree)
