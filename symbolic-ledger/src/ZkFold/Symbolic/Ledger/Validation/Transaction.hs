{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction (
  validateTransaction,
) where

import Data.Function ((&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import ZkFold.Algebra.Class (AdditiveSemigroup (..), MultiplicativeMonoid (..), Zero (..))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.Vector (Vector)
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree)
import ZkFold.Symbolic.Ledger.Types
import qualified Prelude as P

validateTransaction
  :: forall ud bo i o a context
   . SignatureTransaction i o a context
  => MerkleTree ud context
  -> (Vector bo :.: Output a) context
  -> Transaction i o a context
  -> (FieldElement :*: Bool :*: MerkleTree ud) context
validateTransaction utxoTree bridgedOutOutputs tx =
  let
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
    (bouts :*: boutsIncluded :*: utxoTree)