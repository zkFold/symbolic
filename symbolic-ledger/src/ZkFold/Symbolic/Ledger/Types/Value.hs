{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}

module ZkFold.Symbolic.Ledger.Types.Value (
  Token,
  MintingContract,
  CurrencySymbol,
  Amount,
  Value (..),
  MultiAssetValue,
  multiAssetValueToList,
  unsafeMultiAssetValueFromList,
  emptyMultiAssetValue,
  addValue,
  multiAssetValue,
) where

import           Data.Coerce                           (coerce)
import           Data.Data                             (Proxy)
import           Prelude                               hiding ((||), Bool, Eq, all, length, null, splitAt, (&&), (*), (+),
                                                        (==))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Symbolic.Class                 (Symbolic)
import           ZkFold.Symbolic.Data.Bool             (Bool, BoolType (..))
import           ZkFold.Symbolic.Data.Class            (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Data.Combinators      (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional      (Conditional, ifThenElse)
import qualified ZkFold.Symbolic.Data.List as Symbolic.List
import           ZkFold.Symbolic.Data.Eq               (Eq (BooleanOf, (==)), SymbolicEq)
import           ZkFold.Symbolic.Data.List             (List, emptyList, null, singleton, uncons, (.:))
import           ZkFold.Symbolic.Data.UInt             (UInt)
import           ZkFold.Symbolic.Ledger.Types.Contract (Contract, ContractId)
import ZkFold.Symbolic.Data.Switch (Switch (..))
import           ZkFold.Symbolic.Data.Morph        (MorphFrom, MorphTo (..), (@))
import ZkFold.Symbolic.Fold (SymbolicFold)

-- | Input to the minting contract. Usually a token name.
data Token context

-- | A minting contract is a contract that guards the minting and burning of tokens.
-- In order to mint or burn tokens, the transaction must satisfy the minting contract.
type MintingContract tx w context = Contract tx (Token context) w context

-- | A currency symbol is a hash of the minting contract that mints the tokens.
type CurrencySymbol context = ContractId context

-- | Amount of tokens.
type Amount context = UInt 64 Auto context

-- | A value represents the amount of tokens that is contained in a transaction output.
-- The `ContractId` corresponds to the contract that minted the tokens with the `Token` containing the input data.
-- The `Amount` contains the amount of tokens.
data Value context = Value
  { mintingPolicy :: CurrencySymbol context
  , tokenInstance :: Token context
  , tokenQuantity :: Amount context
  }

-- | Denotes multiple values.
newtype MultiAssetValue context = UnsafeMultiAssetValue (List context (CurrencySymbol context, List context (Token context, Amount context)))

-- | Convert a multi-asset value to a list.
multiAssetValueToList :: MultiAssetValue context -> List context (CurrencySymbol context, List context (Token context, Amount context))
multiAssetValueToList = coerce

-- | Unsafe constructor for a multi-asset value. Mainly to be used for testing.
unsafeMultiAssetValueFromList :: List context (CurrencySymbol context, List context (Token context, UInt 64 Auto context)) -> MultiAssetValue context
unsafeMultiAssetValueFromList = UnsafeMultiAssetValue

-- | Construct an empty multi-asset value.
emptyMultiAssetValue ::
       SymbolicData (CurrencySymbol context)
    => Context (CurrencySymbol context) ~ context
    => Support (CurrencySymbol context) ~ Proxy context
    => SymbolicData ((Token context, Amount context))
    => Context ((Token context, Amount context)) ~ context
    => MultiAssetValue context
emptyMultiAssetValue = UnsafeMultiAssetValue emptyList

-- FIXME: delete me
y ::
     forall context.
     SymbolicOutput (Token context)
  => Eq (Token context)
  => Context (Token context) ~ context
  => SymbolicFold context
  => List context (Token context)
y = addToken (Morph \(a :: (Token s), b) -> a == b) undefined emptyList


-- FIXME: delete me.
addToken ::
     forall context.
     SymbolicOutput (Token context)
  => Context (Token context) ~ context
  => SymbolicFold context
  => MorphFrom context (Token context, Token context) (Bool context)
  -> Token context
  -> List context (Token context) -> List context (Token context)
addToken eq givenToken ls =
  let (exists, _, r) =
        Symbolic.List.foldr (Morph \(y :: Switch s (Token context), (found :: Bool s, givenToken' :: Switch s (Token context), ys)) ->
          let isSame = eq @ (y, givenToken') in
          (found || isSame, givenToken', y .: ys)
              )
              (false :: Bool context, givenToken, emptyList)
              ls
  in r

-- | Add a single value to a multi-asset value.
addValue ::
     forall context. Conditional (Bool context) (MultiAssetValue context)
  => BooleanOf (Token context) ~ Bool context
  => Eq (Token context)
  => Symbolic context
  => SymbolicOutput (Value context)
  => Context (Value context) ~ context
  => SymbolicEq (CurrencySymbol context)
  => Context (CurrencySymbol context) ~ context
  => SymbolicData ((Token context, Amount context))
  => Context ((Token context, Amount context)) ~ context
  => Support (Token context) ~ Proxy context
  => Value context
  -> MultiAssetValue context
  -> MultiAssetValue context
addValue val@Value {..} (UnsafeMultiAssetValue valList) =
  let (valHead@(valHeadCurrencySymbol, valHeadTokenList), valTail) = uncons valList
      valHeadTokenListAdded = addTokenAmount valHeadTokenList
      UnsafeMultiAssetValue valTailAdded = addValue val (UnsafeMultiAssetValue valTail)
  in ifThenElse (null valList)
       (UnsafeMultiAssetValue (singleton (mintingPolicy, singleton (tokenInstance, tokenQuantity))))
       (
        ifThenElse (mintingPolicy == valHeadCurrencySymbol)
          (UnsafeMultiAssetValue ((valHeadCurrencySymbol, valHeadTokenListAdded) .: valTail))
          (UnsafeMultiAssetValue (valHead .: valTailAdded))

       )
  where
    addTokenAmount tokenAmountList =
      let (tokenHead@(tokenHeadToken, tokenHeadAmount), tokenTail) = uncons tokenAmountList
          tokenAmountAdded =
            ifThenElse (tokenHeadToken == tokenInstance)
              ((tokenHeadToken, tokenHeadAmount + tokenQuantity) .: tokenTail)
              (tokenHead .: addTokenAmount tokenTail)
      in ifThenElse (null tokenAmountList)
           (singleton (tokenInstance, tokenQuantity))
           tokenAmountAdded

-- | Safe constructor for a multi-asset value.
multiAssetValue ::
     Symbolic context
  => SymbolicOutput (Value context)
  => Context (Value context) ~ context
  => Conditional (Bool context) (MultiAssetValue context)
  => BooleanOf (Token context) ~ Bool context
  => Eq (Token context)
  => SymbolicEq (CurrencySymbol context)
  => Context (CurrencySymbol context) ~ context
  => Foldable (List context)
  => SymbolicData ((Token context, Amount context))
  => Context ((Token context, Amount context)) ~ context
  => Support (Token context) ~ Proxy context
  => List context (Value context)
  -> MultiAssetValue context
multiAssetValue = foldr addValue emptyMultiAssetValue

