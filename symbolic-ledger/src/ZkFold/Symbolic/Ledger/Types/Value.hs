{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           Prelude                               hiding (Bool, Eq, all, length, null, splitAt, (&&), (*), (+),
                                                        (==), (||))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Symbolic.Class                 (Symbolic)
import           ZkFold.Symbolic.Data.Bool             (Bool, BoolType (..))
import           ZkFold.Symbolic.Data.ByteString       (ByteString)
import           ZkFold.Symbolic.Data.Class            (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Data.Combinators      (KnownRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional      (Conditional, ifThenElse)
import           ZkFold.Symbolic.Data.Eq               (Eq (BooleanOf, (==)), SymbolicEq)
import qualified ZkFold.Symbolic.Data.List             as Symbolic.List
import           ZkFold.Symbolic.Data.List             (List, emptyList, singleton, (.:))
import           ZkFold.Symbolic.Data.Morph            (MorphTo (..))
import           ZkFold.Symbolic.Data.UInt             (UInt)
import           ZkFold.Symbolic.Fold                  (SymbolicFold)
import           ZkFold.Symbolic.Ledger.Types.Contract (Contract, ContractId)

-- TODO: Make it a field element.
-- | Input to the minting contract. Usually a token name.
newtype Token context = Token (ByteString 256 context)

deriving newtype instance (Symbolic context) => SymbolicData (Token context)
deriving newtype instance (Symbolic context) => Conditional (Bool context) (Token context)
deriving newtype instance (Symbolic context) => Eq (Token context)

-- | A minting contract is a contract that guards the minting and burning of tokens.
-- In order to mint or burn tokens, the transaction must satisfy the minting contract.
type MintingContract tx w context = Contract tx (Token context) w context

-- | A currency symbol is a hash of the minting contract that mints the tokens.
type CurrencySymbol context = Token context -- ContractId context

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
    => MultiAssetValue context
emptyMultiAssetValue = UnsafeMultiAssetValue emptyList

-- | Add a given token with it's amount to a list. If the token already exists, the amount is added to the existing amount.
--
-- We assume that all the tokens in the list are unique.
addTokenAmount ::
     forall context.
     SymbolicFold context
  => KnownRegisters context 64 Auto
  => Token context
  -> Amount context
  -> List context (Token context, Amount context) -> List context (Token context, Amount context)
addTokenAmount givenToken givenAmount ls =
  let (tokenExisted, _, _, r) =
        Symbolic.List.foldr (
          Morph
            \((yt :: Token s, ya :: Amount s),
              (found :: Bool s, givenToken' :: Token s, givenAmount' :: Amount s, ys)) ->
                let isSame :: Bool s = givenToken' == yt
                in (
                    found || isSame,
                    givenToken',
                    givenAmount',
                    ifThenElse isSame
                      ((yt, ya + givenAmount') .: ys)
                      ((yt, ya) .: ys)
                  )
        )
          (false :: Bool context, givenToken, givenAmount, emptyList)
          ls
  in ifThenElse tokenExisted
       r
       ((givenToken, givenAmount) .: ls)

-- | Add a single value to a multi-asset value.
addValue ::
     forall context. Conditional (Bool context) (MultiAssetValue context)
  => BooleanOf (Token context) ~ Bool context
  => Eq (Token context)
  => SymbolicOutput (Value context)
  => Context (Value context) ~ context
  => SymbolicEq (CurrencySymbol context)
  => Context (CurrencySymbol context) ~ context
  => SymbolicFold context
  => SymbolicOutput (Token context)
  => SymbolicOutput (Amount context)
  => SymbolicData ((Token context, Amount context))
  => Context ((Token context, Amount context)) ~ context
  => Support (Token context) ~ Proxy context
  => KnownRegisters context 64 Auto
  => Value context
  -> MultiAssetValue context
  -> MultiAssetValue context
addValue Value {..} (UnsafeMultiAssetValue valList) =
  let (policyExisted, _, _, _, r) =
        Symbolic.List.foldr (Morph \((yp :: CurrencySymbol s, yas :: List s ((Token s, Amount s))), (found :: Bool s, mintingPolicy' :: CurrencySymbol s, tokenInstance' :: (Token s), tokenQuantity' :: (Amount s), ys)) ->
          let isSame :: Bool s = undefined  -- mintingPolicy' == yp
              tokenAmountAdded = addTokenAmount tokenInstance' tokenQuantity' yas
          in (
               found || isSame,
               mintingPolicy',
               tokenInstance',
               tokenQuantity',
               ifThenElse isSame
                 ((yp, tokenAmountAdded) .: ys)
                 ((yp, yas) .: ys))
          )
              (false :: Bool context, mintingPolicy, tokenInstance, tokenQuantity, emptyList)
              valList
  in ifThenElse policyExisted
       (UnsafeMultiAssetValue r)
       (UnsafeMultiAssetValue $ (mintingPolicy, singleton (tokenInstance, tokenQuantity)) .: valList)

-- | Safe constructor for a multi-asset value.
multiAssetValue ::
     SymbolicOutput (Value context)
  => SymbolicFold context
  => SymbolicOutput (Token context)
  => SymbolicOutput (Amount context)
  => Context (Value context) ~ context
  => Conditional (Bool context) (MultiAssetValue context)
  => BooleanOf (Token context) ~ Bool context
  => Eq (Token context)
  => SymbolicEq (CurrencySymbol context)
  => Context (CurrencySymbol context) ~ context
  => Foldable (List context)
  => SymbolicData ((Token context, Amount context))
  => KnownRegisters context 64 Auto
  => Context ((Token context, Amount context)) ~ context
  => Support (Token context) ~ Proxy context
  => List context (Value context)
  -> MultiAssetValue context
multiAssetValue = foldr addValue emptyMultiAssetValue

