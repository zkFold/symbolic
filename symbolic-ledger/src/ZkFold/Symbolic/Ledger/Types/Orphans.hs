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
import GHC.Generics (Generic, Generic1, Par1 (..), (:.:) (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Ctx, Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Data.Hash qualified as Base

import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry)
import Data.Aeson (FromJSON (..), ToJSON (..), withBool)
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Algebra.Class (FromConstant (..), MultiplicativeMonoid (..), ToConstant (..))
import qualified ZkFold.Symbolic.Data.Bool as SBool
import Prelude (Integer)
import qualified Prelude as Haskell
import ZkFold.Symbolic.Data.Bool (fromBool)

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

instance FromJSON (FieldElement (Interpreter Fq)) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

instance FromJSON (SBool.Bool (Interpreter Fq)) where
  parseJSON = withBool "Bool" $ \b -> pure $ fromConstant b

instance ToJSON (FieldElement (Interpreter Fq)) where
  toJSON v = toJSON (toConstant v :: Fq)


instance ToJSON (SBool.Bool (Interpreter Fq)) where
  toJSON b = toJSON (fromBool b Haskell.== one)

instance forall n. FromJSON (SBool.Bool (Interpreter Fq)) => FromJSON ((:.:) (Vector n) SBool.Bool (Interpreter Fq)) where
  parseJSON v = Comp1 <$> parseJSON v

instance forall n. ToJSON (SBool.Bool (Interpreter Fq)) => ToJSON ((:.:) (Vector n) SBool.Bool (Interpreter Fq)) where
  toJSON (Comp1 x) = toJSON x

deriving anyclass instance forall ud. FromJSON (MerkleEntry ud (Interpreter Fq))
deriving anyclass instance forall ud. ToJSON   (MerkleEntry ud (Interpreter Fq))
