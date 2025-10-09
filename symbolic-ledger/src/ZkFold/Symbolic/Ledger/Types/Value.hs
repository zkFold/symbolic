{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Value (
  AssetPolicy,
  adaPolicy,
  AssetName,
  adaName,
  AssetQuantity,
  AssetValue (..),
  nullAssetValue,
  AssetValues,
  KnownRegistersAssetQuantity,

  -- * Construction
  assetValuesToList,
  unsafeAssetValuesFromList,
  emptyAssetValues,
  assetValuesFromList,

  -- * Arithmetic
  addAssetValue,
  negateAssetValues,
  addAssetValues,
) where

import Data.Coerce (coerce)
import Data.Function ((&))
import GHC.Generics (Generic, Generic1, type (:*:) (..))
import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.List (List, emptyList, (.:))
import qualified ZkFold.Symbolic.Data.List as Symbolic.List
import ZkFold.Symbolic.Fold (SymbolicFold)
import Prelude hiding (
  Bool,
  Eq,
  Int,
  all,
  foldr,
  length,
  negate,
  null,
  splitAt,
  (&&),
  (*),
  (+),
  (==),
  (||),
 )

-- | Asset policy.
type AssetPolicy context = FieldElement context

-- | Name of the asset.
type AssetName context = FieldElement context

-- | Quantity of an asset.
type AssetQuantity context = Int 128 Auto context

type KnownRegistersAssetQuantity context = KnownRegisters context 128 Auto

-- TODO: Replace with actual value, once we finalize how policy names are represented.
adaPolicy :: AssetPolicy context
adaPolicy = undefined

-- TODO: Replace with actual value, once we finalize how asset names are represented.
adaName :: AssetName context
adaName = undefined

-- | A value represents the details of an asset that is contained in a transaction output.
data AssetValue context = AssetValue
  { assetPolicy :: AssetPolicy context
  , assetName :: AssetName context
  , assetQuantity :: AssetQuantity context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance (KnownRegistersAssetQuantity context, Symbolic context) => Eq (AssetValue context)

-- | Null asset value.
nullAssetValue :: Symbolic context => AssetValue context
nullAssetValue = AssetValue {assetPolicy = zero, assetName = zero, assetQuantity = zero}

instance Symbolic context => Zero (AssetValue context) where
  zero = nullAssetValue

-- | Denotes multiple assets.
newtype AssetValues context = UnsafeAssetValues (List AssetValue context)
  deriving newtype SymbolicData

deriving newtype instance (KnownRegistersAssetQuantity context, Symbolic context) => Eq (AssetValues context)

-- | Convert a 'AssetValues' to a list.
assetValuesToList :: AssetValues context -> List AssetValue context
assetValuesToList = coerce

-- | Unsafe constructor for 'AssetValues'. Mainly to be used for testing.
unsafeAssetValuesFromList :: List AssetValue context -> AssetValues context
unsafeAssetValuesFromList = UnsafeAssetValues

-- | Construct an empty 'AssetValues'.
emptyAssetValues
  :: KnownRegistersAssetQuantity context
  => Symbolic context
  => AssetValues context
emptyAssetValues = UnsafeAssetValues emptyList

-- | Safe constructor for 'AssetValues'.
assetValuesFromList
  :: SymbolicFold context
  => KnownRegistersAssetQuantity context
  => List AssetValue context
  -> AssetValues context
assetValuesFromList = Symbolic.List.foldr addAssetValue emptyAssetValues

-- | Add an 'AssetValue' to 'AssetValues'.
--
-- If the asset already exists in the list, the quantities are added. Else the asset is added to the list.
addAssetValue
  :: forall context
   . SymbolicFold context
  => KnownRegistersAssetQuantity context
  => AssetValue context
  -> AssetValues context
  -> AssetValues context
addAssetValue givenAssetVal (UnsafeAssetValues assetValList) =
  let assetExisted :*: _ :*: r =
        Symbolic.List.foldr
          ( \y (found :*: givenAssetVal' :*: ys) ->
              let isSame = givenAssetVal' == y
               in (found || isSame)
                    :*: givenAssetVal'
                    :*: ifThenElse
                      isSame
                      ( AssetValue
                          { assetPolicy = assetPolicy y
                          , assetName = assetName y
                          , assetQuantity = assetQuantity y + assetQuantity givenAssetVal'
                          }
                          .: ys
                      )
                      (y .: ys)
          )
          ((false :: Bool context) :*: givenAssetVal :*: emptyList)
          assetValList
   in ifThenElse
        assetExisted
        (UnsafeAssetValues r)
        (UnsafeAssetValues $ givenAssetVal .: assetValList)

-- | Negate quantities present inside 'AssetValues'.
negateAssetValues
  :: forall context
   . SymbolicFold context
  => KnownRegistersAssetQuantity context
  => AssetValues context
  -> AssetValues context
negateAssetValues (UnsafeAssetValues ls) =
  UnsafeAssetValues $
    Symbolic.List.foldr
      ( \av acc ->
          (av {assetQuantity = assetQuantity av & negate}) .: acc
      )
      (emptyList :: List AssetValue context)
      ls

-- | Add two 'AssetValues'.
addAssetValues
  :: forall context
   . SymbolicFold context
  => KnownRegistersAssetQuantity context
  => AssetValues context
  -> AssetValues context
  -> AssetValues context
addAssetValues as (UnsafeAssetValues bs) =
  Symbolic.List.foldl (flip addAssetValue) as bs
