{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Core (
  validateTransaction,
  validateTransactionWithAssetDiff,
) where

import           Prelude                          (fst, undefined, ($), (.))

import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Conditional (ifThenElse)
import           ZkFold.Symbolic.Data.Eq          ((==))
import qualified ZkFold.Symbolic.Data.List        as Symbolic.List
import           ZkFold.Symbolic.Data.Morph
import           ZkFold.Symbolic.Ledger.Types

-- | This function extracts boolean from 'validateTransaction', see it for more details.
validateTransaction ::
  forall context.
  Signature context =>
  -- | 'Transaction' to validate.
  Transaction context ->
  Bool context
validateTransaction = fst . validateTransactionWithAssetDiff

{- | Validate a 'Transaction'.

To check:
  * TODO: write checks.
-}
validateTransactionWithAssetDiff ::
  forall context.
  Signature context =>
  -- | 'Transaction' to validate.
  Transaction context ->
  -- | Validity of transaction along with value difference between outputs and inputs.
  (Bool context, AssetValues context)
validateTransactionWithAssetDiff tx =
  let
    -- Is transaction valid?
    resTxAccValidity :: Bool context = undefined
    -- Generated value.
    resTxAccOutValues :: AssetValues context =
      fst $
        Symbolic.List.foldl
          ( Morph \((accOutValues :: AssetValues s, accTxOwner :: Address s), txOut :: Output s) ->
              ( ifThenElse
                  (txoAddress txOut == accTxOwner)
                  accOutValues
                  (addAssetValue (txoValue txOut) accOutValues)
              , accTxOwner
              )
          )
          (emptyAssetValues :: AssetValues context, txOwner tx)
          (txOutputs tx)
    -- Consumed value.
    resTxAccInValues :: AssetValues context =
      fst $
        Symbolic.List.foldl
          ( Morph \((accInValues :: AssetValues s, accTxOwner :: Address s), txInput :: Input s) ->
              let out = txiOutput txInput
               in ( ifThenElse
                      (txoAddress out == accTxOwner)
                      (accInValues)
                      (addAssetValue (txoValue $ txiOutput txInput) accInValues)
                  , accTxOwner
                  )
          )
          (emptyAssetValues :: AssetValues context, txOwner tx)
          (txInputs tx)
   in
    ( resTxAccValidity
    , addAssetValues resTxAccOutValues (negateAssetValues resTxAccInValues)
    )
