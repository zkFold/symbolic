{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Ledger.Types.Orphans (

) where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1, (:.:) (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Ctx, Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Data.Hash qualified as Base

import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry)
import Data.Aeson (FromJSON (..), ToJSON (..), withBool)
import ZkFold.Algebra.Class (FromConstant (..), MultiplicativeMonoid (..), ToConstant (..))
import qualified ZkFold.Symbolic.Data.Bool as SBool
import Prelude (Integer, (.))
import qualified Prelude as Haskell
import ZkFold.Symbolic.Data.Bool (fromBool)
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.Combinators (KnownRegisterSize)
import GHC.TypeNats (KnownNat)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter, RollupBF)

newtype VectorTakingCtx n (a :: Ctx -> Type) c = VectorTakingCtx ((Vector n :.: a) c)
  deriving stock (Generic, Generic1)
  deriving anyclass SymbolicData

instance (Symbolic c, SymbolicData a) => Hashable (HashSimple c) (VectorTakingCtx n a c) where
  hasher = hashFn

deriving via
  (VectorTakingCtx n a c)
  instance
    (Symbolic c, SymbolicData a) => Hashable (HashSimple c) ((Vector n :.: a) c)

instance Symbolic context => Hashable (HashSimple context) (FieldElement context) where
  hasher = hashFn

instance FromJSON (FieldElement RollupBFInterpreter) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

instance FromJSON (SBool.Bool RollupBFInterpreter) where
  parseJSON = withBool "Bool" $ \b -> pure $ fromConstant b

instance ToJSON (FieldElement RollupBFInterpreter) where
  toJSON v = toJSON (toConstant v :: RollupBF)


instance ToJSON (SBool.Bool RollupBFInterpreter) where
  toJSON b = toJSON (fromBool b Haskell.== one)

instance forall n a. FromJSON (a RollupBFInterpreter) => FromJSON ((:.:) (Vector n) a RollupBFInterpreter) where
  parseJSON v = Comp1 <$> parseJSON v

instance forall n a. ToJSON (a RollupBFInterpreter) => ToJSON ((:.:) (Vector n) a RollupBFInterpreter) where
  toJSON (Comp1 x) = toJSON x

deriving anyclass instance forall ud. FromJSON (MerkleEntry ud RollupBFInterpreter)
deriving anyclass instance forall ud. ToJSON   (MerkleEntry ud RollupBFInterpreter)

instance forall n r. (KnownRegisterSize r, KnownNat n) => ToJSON (Int n r RollupBFInterpreter) where
  toJSON = toJSON . toConstant

instance forall n r. (KnownRegisterSize r, KnownNat n) => FromJSON (Int n r RollupBFInterpreter) where
  parseJSON v = fromConstant @Integer <$> parseJSON v