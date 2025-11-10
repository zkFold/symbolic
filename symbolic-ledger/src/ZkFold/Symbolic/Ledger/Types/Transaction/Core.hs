{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Ledger.Types.Transaction.Core (
  OutputRef (..),
  nullOutputRef,
  Output (..),
  nullOutput,
  UTxO (..),
  nullUTxO,
  nullUTxOHash,
  Transaction (..),
  TransactionId,
  txId,
  EdDSABaseField,
  EdDSAScalarField,
  EdDSAPoint,
  PrivateKey,
  PublicKey,
  signTransaction,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import GHC.Generics (Generic, Generic1, (:*:), (:.:) (..))
import GHC.TypeNats (KnownNat)
import ZkFold.Algebra.Class (Zero (..))
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.HFunctor.Classes (HEq, HShow)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaSign)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.Hash (Hashable, hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.UInt (UInt)
import Prelude hiding (Bool, Eq, Maybe, length, splitAt, (*), (+), (==), (||))
import Prelude qualified as Haskell hiding ((||))

import ZkFold.Symbolic.Ledger.Types.Address (Address, nullAddress)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple, hashFn)
import ZkFold.Symbolic.Ledger.Types.Orphans ()
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue, KnownRegistersAssetQuantity)

-- | An output's reference.
data OutputRef context = OutputRef
  { orTxId :: HashSimple context
  -- ^ Transaction ID which created this output.
  , orIndex :: UInt 32 Auto context
  -- ^ Index of the output in the transaction.
  -- TODO: Restrict to represent 'o' outputs instead of 2^32?
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

instance
  Symbolic context
  => Eq (OutputRef context)

deriving stock instance HEq context => Haskell.Eq (OutputRef context)

deriving stock instance HShow context => Haskell.Show (OutputRef context)

deriving anyclass instance ToJSON (OutputRef RollupBFInterpreter)

deriving anyclass instance FromJSON (OutputRef RollupBFInterpreter)

-- | Null output reference.
nullOutputRef :: Symbolic context => OutputRef context
nullOutputRef = OutputRef {orTxId = zero, orIndex = zero}

-- | An output of a transaction.
data Output a context = Output
  { oAddress :: Address context
  -- ^ Address of the output.
  , oAssets :: (Vector a :.: AssetValue) context
  -- ^ Assets of the output.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

instance
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  )
  => Eq (Output a context)

deriving stock instance HEq context => Haskell.Eq (Output a context)

deriving stock instance HShow context => Haskell.Show (Output a context)

instance Symbolic context => Hashable (HashSimple context) (Output a context) where
  hasher = hashFn

deriving anyclass instance ToJSON (Output a RollupBFInterpreter)

deriving anyclass instance FromJSON (Output a RollupBFInterpreter)

-- | Null output.
nullOutput :: forall a context. (Symbolic context, KnownNat a) => Output a context
nullOutput = Output {oAddress = nullAddress, oAssets = Comp1 zero}

-- | A UTxO.
data UTxO a context = UTxO
  { uRef :: OutputRef context
  , uOutput :: Output a context
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

instance
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  )
  => Eq (UTxO a context)

deriving stock instance HEq context => Haskell.Eq (UTxO a context)

deriving stock instance HShow context => Haskell.Show (UTxO a context)

instance Symbolic context => Hashable (HashSimple context) (UTxO a context) where
  hasher = hashFn

deriving anyclass instance ToJSON (UTxO a RollupBFInterpreter)

-- | Null UTxO.
nullUTxO :: forall a context. (Symbolic context, KnownNat a) => UTxO a context
nullUTxO = UTxO {uRef = nullOutputRef, uOutput = nullOutput}

-- | Null UTxO's hash.
nullUTxOHash
  :: forall a context. (Symbolic context, KnownNat a) => HashSimple context
nullUTxOHash = hash (nullUTxO @a @context) & Base.hHash

-- | Transaction in our symbolic ledger.
data Transaction i o a context = Transaction
  { inputs :: (Vector i :.: OutputRef) context
  -- ^ Inputs.
  , outputs :: (Vector o :.: (Output a :*: Bool)) context
  -- ^ Outputs. Boolean denotes whether the output is a bridge out output, in which case `oAddress` denotes Cardano address.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

instance
  forall i o a context
   . ( Symbolic context
     , KnownRegistersAssetQuantity context
     )
  => Eq (Transaction i o a context)

-- | Transaction hash.
type TransactionId i o a = Hash (Transaction i o a)

instance Symbolic context => Hashable (HashSimple context) (Transaction i o a context) where
  hasher = hashFn

-- | Obtain transaction hash.
txId
  :: forall i o a context
   . Symbolic context
  => Transaction i o a context
  -> TransactionId i o a context
txId = hash

type EdDSABaseField = FFA Jubjub_Base 'Auto

type EdDSAScalarField = FFA Jubjub_Scalar 'Auto

type EdDSAPoint = Jubjub_Point

type PrivateKey = EdDSAScalarField

type PublicKey = EdDSAPoint

signTransaction
  :: forall i o a context
   . Symbolic context
  => KnownFFA Jubjub_Scalar 'Auto context
  => KnownFFA Jubjub_Base 'Auto context
  => Transaction i o a context
  -> PrivateKey context
  -> (EdDSAPoint :*: EdDSAScalarField) context
signTransaction tx privateKey = eddsaSign hashFn privateKey (txId tx & Base.hHash)
