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
import Data.OpenApi (ToSchema (..))
import GHC.Generics (Generic, Generic1, (:*:), (:.:) (..))
import GHC.TypeNats (KnownNat)
import ZkFold.Algebra.Class (NumberOfBits, Zero (..))
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaSign)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.Hash (Hashable, hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.UInt (KnownUInt, UInt)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import Prelude hiding (Bool, Eq, Maybe, length, splitAt, (*), (+), (==), (||))
import Prelude qualified as Haskell hiding ((||))

import ZkFold.Symbolic.Ledger.Types.Address (Address, nullAddress)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Symbolic.Ledger.Types.Hash (Hash, HashSimple, hashFn)
import ZkFold.Symbolic.Ledger.Types.Orphans ()
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue)

-- | An output's reference.
data OutputRef context = OutputRef
  { orTxId :: HashSimple context
  -- ^ Transaction ID which created this output.
  , orIndex :: UInt 32 context
  -- ^ Index of the output in the transaction.
  -- TODO: Restrict to represent 'o' outputs instead of 2^32?
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (SymbolicData, SymbolicInput)

instance Symbolic context => Eq (OutputRef context)

instance Symbolic c => Collect (ConstrainedDatum c) (OutputRef c)

deriving stock instance Haskell.Show context => Haskell.Show (OutputRef context)

deriving anyclass instance ToJSON (OutputRef RollupBF)

deriving anyclass instance FromJSON (OutputRef RollupBF)

deriving anyclass instance ToSchema (OutputRef RollupBF)

-- | Null output reference.
nullOutputRef :: (Symbolic context, KnownUInt 32 context) => OutputRef context
nullOutputRef = OutputRef {orTxId = zero, orIndex = zero}

-- | An output of a transaction.
data Output a context = Output
  { oAddress :: Address context
  -- ^ Address of the output.
  , oAssets :: (Vector a :.: AssetValue) context
  -- ^ Assets of the output.
  }
  deriving stock (Generic, Generic1, Show)
  deriving anyclass (Eq, SymbolicData, SymbolicInput)

instance Symbolic c => Collect (ConstrainedDatum c) (Output a c)

instance Symbolic context => Hashable (HashSimple context) (Output a context) where
  hasher = hashFn

deriving anyclass instance ToJSON (Output a RollupBF)

deriving anyclass instance FromJSON (Output a RollupBF)

deriving anyclass instance forall a. KnownNat a => ToSchema (Output a RollupBF)

-- | Null output.
nullOutput :: forall a c. (Symbolic c, KnownNat a, KnownUInt 64 c) => Output a c
nullOutput = Output {oAddress = nullAddress, oAssets = Comp1 zero}

-- | A UTxO.
data UTxO a context = UTxO
  { uRef :: OutputRef context
  , uOutput :: Output a context
  }
  deriving stock (Generic, Generic1, Show)
  deriving anyclass (Eq, SymbolicData, SymbolicInput)

instance Symbolic c => Collect (ConstrainedDatum c) (UTxO a c)

instance Symbolic context => Hashable (HashSimple context) (UTxO a context) where
  hasher = hashFn

deriving anyclass instance ToJSON (UTxO a RollupBF)

deriving anyclass instance FromJSON (UTxO a RollupBF)

deriving anyclass instance forall a. KnownNat a => ToSchema (UTxO a RollupBF)

-- | Null UTxO.
nullUTxO
  :: forall a c
   . (Symbolic c, KnownNat a, KnownUInt 64 c, KnownUInt 32 c) => UTxO a c
nullUTxO = UTxO {uRef = nullOutputRef, uOutput = nullOutput}

-- | Null UTxO's hash.
nullUTxOHash
  :: forall a c
   . (Symbolic c, KnownNat a, KnownUInt 64 c, KnownUInt 32 c) => HashSimple c
nullUTxOHash = hash (nullUTxO @a @c) & Base.hHash

-- | Transaction in our symbolic ledger.
data Transaction i o a context = Transaction
  { inputs :: (Vector i :.: OutputRef) context
  -- ^ Inputs.
  , outputs :: (Vector o :.: (Output a :*: Bool)) context
  -- ^ Outputs. Boolean denotes whether the output is a bridge out output, in which case `oAddress` denotes Cardano address.
  }
  deriving stock (Generic, Generic1)
  deriving anyclass (Eq, SymbolicData, SymbolicInput)

deriving anyclass instance ToJSON (Transaction i o a RollupBF)

deriving anyclass instance FromJSON (Transaction i o a RollupBF)

instance Symbolic c => Collect (ConstrainedDatum c) (Transaction i o a c)

deriving anyclass instance
  forall i o a. (KnownNat i, KnownNat o, KnownNat a) => ToSchema (Transaction i o a RollupBF)

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

type EdDSABaseField = FFA Jubjub_Base

type EdDSAScalarField = FFA Jubjub_Scalar

type EdDSAPoint = Jubjub_Point

type PrivateKey = EdDSAScalarField

type PublicKey = EdDSAPoint

signTransaction
  :: forall i o a context
   . Symbolic context
  => KnownFFA Jubjub_Scalar context
  => KnownFFA Jubjub_Base context
  => KnownUInt (NumberOfBits context) context
  => Transaction i o a context
  -> PrivateKey context
  -> (EdDSAPoint :*: EdDSAScalarField) context
signTransaction tx privateKey =
  eddsaSign hashFn privateKey (txId tx & Base.hHash)
