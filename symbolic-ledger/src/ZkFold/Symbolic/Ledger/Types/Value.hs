{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Ledger.Types.Value where

import           Prelude                               hiding (Bool, Eq, all, length, null, splitAt, (&&), (*), (+),
                                                        (==))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Symbolic.Class                 (Symbolic)
import           ZkFold.Symbolic.Data.Bool             (Bool, (&&))
import           ZkFold.Symbolic.Data.Class            (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Data.Combinators      (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional      (Conditional, bool)
import           ZkFold.Symbolic.Data.Eq               (Eq (BooleanOf, (==)), SymbolicEq)
import           ZkFold.Symbolic.Data.List             (List, emptyList, null, singleton, uncons, (.:))
import           ZkFold.Symbolic.Data.UInt             (UInt)
import           ZkFold.Symbolic.Ledger.Types.Contract (Contract, ContractId)

-- | Input to the minting contract. Usually a token name.
data Token context

-- | A minting contract is a contract that guards the minting and burning of tokens.
-- In order to mint or burn tokens, the transaction must satisfy the minting contract.
type MintingContract tx w context = Contract tx (Token context) w context

-- | A currency symbol is a hash of the minting contract that mints the tokens.
type CurrencySymbol context = ContractId context

-- | A value represents the amount of tokens that is contained in a transaction output.
-- The `ContractId` corresponds to the contract that minted the tokens with the `Token` containing the input data.
-- The `UInt64` contains the amount of tokens.
data Value context = Value
  { mintingPolicy :: CurrencySymbol context
  , tokenInstance :: Token context
  , tokenQuantity :: UInt 64 Auto context
  }

newtype MultiAssetValue context = UnsafeMultiAssetValue (List context (Value context))

emptyMultiAssetValue ::
     SymbolicData (Value context)
  => Context (Value context) ~ context
  => MultiAssetValue context
emptyMultiAssetValue = UnsafeMultiAssetValue emptyList

-- Add a single value to a multi-asset value
addValue ::
     forall context. Conditional (Bool context) (MultiAssetValue context)
  => BooleanOf (Token context) ~ Bool context
  => Eq (Token context)
  => Symbolic context
  => SymbolicOutput (Value context)
  => Context (Value context) ~ context
  => SymbolicEq (CurrencySymbol context)
  => Context (CurrencySymbol context) ~ context
  => Value context
  -> MultiAssetValue context
  -> MultiAssetValue context
addValue val (UnsafeMultiAssetValue valList) =
  let oneVal = UnsafeMultiAssetValue (singleton val)
      (valHead, valTail) = uncons valList
      valHeadAdded = valHead {tokenQuantity = tokenQuantity valHead + tokenQuantity val}
      UnsafeMultiAssetValue valTailAdded = addValue val (UnsafeMultiAssetValue valTail)
      multiVal = bool @(Bool context)
        (UnsafeMultiAssetValue (valHead .: valTailAdded))
        (UnsafeMultiAssetValue (valHeadAdded .: valTail))
        (mintingPolicy val == mintingPolicy valHead && tokenInstance val == tokenInstance valHead)
  in bool multiVal oneVal (null valList)

-- Safe constructor for a multi-asset value
multiValueAsset ::
     Symbolic context
  => SymbolicOutput (Value context)
  => Context (Value context) ~ context
  => Conditional (Bool context) (MultiAssetValue context)
  => BooleanOf (Token context) ~ Bool context
  => Eq (Token context)
  => SymbolicEq (CurrencySymbol context)
  => Context (CurrencySymbol context) ~ context
  => Foldable (List context)
  => List context (Value context)
  -> MultiAssetValue context
multiValueAsset = foldr addValue emptyMultiAssetValue

-- | Unsafe constructor for a multi-asset value. Mainly to be used for testing.
unsafeMultiAssetValue :: List context (Value context) -> MultiAssetValue context
unsafeMultiAssetValue = UnsafeMultiAssetValue
