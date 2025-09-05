{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction.Core (
  validateTransaction,
  validateTransactionWithAssetDiff,
) where

import GHC.Generics ((:*:) (..))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Product (fstP)
import ZkFold.Symbolic.Data.Bool
import qualified ZkFold.Symbolic.Data.List as Symbolic.List
import Prelude (fst, undefined, ($), (.))

import ZkFold.Symbolic.Ledger.Types

-- | This function extracts boolean from 'validateTransaction', see it for more details.
validateTransaction
  :: forall context
   . Signature context
  => Transaction context
  -- ^ 'Transaction' to validate.
  -> Bool context
validateTransaction = fst . validateTransactionWithAssetDiff

-- | Validate a 'Transaction'.
--
-- To check:
--   * TODO: write checks.
validateTransactionWithAssetDiff
  :: forall context
   . Signature context
  => Transaction context
  -- ^ 'Transaction' to validate.
  -> (Bool context, AssetValues context)
  -- ^ Validity of transaction along with value difference between outputs and inputs.
validateTransactionWithAssetDiff tx =
  let
    -- Is transaction valid?
    resTxAccValidity :: Bool context = undefined
    -- Generated value.
    resTxAccOutValues :: AssetValues context =
      fstP $
        Symbolic.List.foldl
          ( \(accOutValues :*: accTxOwner) txOut ->
              ifThenElse
                (txoAddress txOut == accTxOwner)
                accOutValues
                (addAssetValue (txoValue txOut) accOutValues)
                :*: accTxOwner
          )
          (emptyAssetValues :*: txOwner tx)
          (txOutputs tx)
    -- Consumed value.
    resTxAccInValues :: AssetValues context =
      fstP $
        Symbolic.List.foldl
          ( \(accInValues :*: accTxOwner) txInput ->
              let out = txiOutput txInput
               in ifThenElse
                    (txoAddress out == accTxOwner)
                    accInValues
                    (addAssetValue (txoValue $ txiOutput txInput) accInValues)
                    :*: accTxOwner
          )
          (emptyAssetValues :*: txOwner tx)
          (txInputs tx)
   in
    ( resTxAccValidity
    , addAssetValues resTxAccOutValues (negateAssetValues resTxAccInValues)
    )
