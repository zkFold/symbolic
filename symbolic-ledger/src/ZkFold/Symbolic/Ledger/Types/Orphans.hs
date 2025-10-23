{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Ledger.Types.Orphans (

) where

import Control.Applicative (pure)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withBool, withObject, (.:), (.=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Kind (Type)
import Data.OpenApi (
  OpenApiType (..),
  ToSchema (..),
  declareSchemaRef,
  type_,
 )
import Data.OpenApi.Internal.Schema (named)
import Data.OpenApi.Lens (properties, required)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (
  Generic,
  Generic1,
  (:*:) (..),
  (:.:) (..),
 )
import GHC.IsList (IsList (..))
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (FromConstant (..), MultiplicativeMonoid (..), ToConstant (..))
import ZkFold.Algebra.EllipticCurve.Class (TwistedEdwards)
import ZkFold.Algebra.EllipticCurve.Class qualified as Elliptic
import ZkFold.Data.Collect (Collect (..))
import ZkFold.Data.MerkleTree (MerkleTreeSize)
import ZkFold.Data.Orphans ()
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool qualified as SBool
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleEntry, MerkleTree)
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum, constrained, unconstrain)
import Prelude (Integer, (.))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)
import ZkFold.Symbolic.Ledger.Types.Hash

newtype VectorTakingCtx n a (c :: Type) = VectorTakingCtx ((Vector n :.: a) c)
  deriving stock (Generic, Generic1)
  deriving anyclass (Collect m, SymbolicData)

instance
  (Collect (ConstrainedDatum c) (a c), Symbolic c)
  => Hashable (HashSimple c) (VectorTakingCtx n a c)
  where
  hasher = hashFn

deriving via
  (VectorTakingCtx n a c)
  instance
    (Collect (ConstrainedDatum c) (a c), Symbolic c)
    => Hashable (HashSimple c) ((Vector n :.: a) c)

instance
  Symbolic context
  => Hashable (HashSimple context) (FieldElement context)
  where
  hasher = hashFn

-- TODO: Can we move these Aeson instances to their respective type-definition module?
instance FromJSON (FieldElement RollupBF) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

instance ToSchema (FieldElement RollupBF) where
  declareNamedSchema _ = declareNamedSchema (Proxy @RollupBF)

instance FromJSON (SBool.Bool RollupBF) where
  parseJSON = withBool "Bool" $ \b -> pure $ fromConstant b

instance ToJSON (FieldElement RollupBF) where
  toJSON v = toJSON (toConstant v :: RollupBF)

instance ToJSON (SBool.Bool RollupBF) where
  toJSON b = toJSON (SBool.fromBool b Haskell.== one)

instance ToSchema (SBool.Bool RollupBF) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Haskell.Bool)

-- Composition Vector n :.: a is encoded as a JSON array of a
instance
  forall n a
   . (ToSchema (a RollupBF), KnownNat n, Typeable a)
  => ToSchema ((:.:) (Vector n) a RollupBF)
  where
  declareNamedSchema _ = declareNamedSchema (Proxy @[a RollupBF])

deriving anyclass instance forall ud. FromJSON (MerkleEntry ud RollupBF)

deriving anyclass instance forall ud. ToJSON (MerkleEntry ud RollupBF)

deriving anyclass instance forall ud. (KnownNat ud, KnownNat (ud - 1)) => ToSchema (MerkleEntry ud RollupBF)

instance KnownUInt n RollupBF => ToJSON (Int n RollupBF) where
  toJSON = toJSON . toConstant

instance KnownUInt n RollupBF => FromJSON (Int n RollupBF) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

instance forall n. KnownNat n => ToSchema (Int n RollupBF) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Integer)

-- Matches JSON encoding of UInt as a natural number.
instance forall n. KnownNat n => ToSchema (UInt n RollupBF) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Natural)

instance KnownFFA a RollupBF => ToJSON (FFA a RollupBF) where
  toJSON v = toJSON (toConstant v)

-- FFA values are encoded as integers
instance KnownFFA a RollupBF => ToSchema (FFA a RollupBF) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Integer)

instance KnownFFA a RollupBF => ToJSON (AffinePoint curve (FFA a) RollupBF) where
  toJSON p =
    let Elliptic.AffinePoint x y = affinePoint p
     in object ["x" .= x, "y" .= y]

instance KnownFFA a RollupBF => FromJSON (FFA a RollupBF) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

-- TODO: What if the parsed point is not on the curve?
instance KnownFFA a RollupBF => FromJSON (AffinePoint curve (FFA a) RollupBF) where
  parseJSON =
    withObject "AffinePoint" $ \o -> do
      x <- o .: "x"
      y <- o .: "y"
      pure (AffinePoint (Elliptic.AffinePoint x y))

-- Apparently, there is an issue with having instance for arbitrary curve, so we hardcode the Jubjub curve for now.
instance
  KnownFFA a RollupBF
  => ToSchema (AffinePoint (TwistedEdwards "jubjub") (FFA a) RollupBF)
  where
  declareNamedSchema _ = do
    xyRef <- declareSchemaRef (Proxy @(FFA a RollupBF))
    let schema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties .~ fromList [("x", xyRef), ("y", xyRef)]
            & required .~ ["x", "y"]
    pure (named "AffinePoint" schema)

instance
  ( ToJSON (h RollupBF)
  , ToJSON (a RollupBF)
  , SymbolicData a
  )
  => ToJSON (Base.Hash h a RollupBF)
  where
  toJSON Base.Hash {..} =
    let v = constrained (Haskell.const SBool.true) hValue
     in object ["hash" .= hHash, "value" .= v]

-- TODO: What if the parsed hash does not match the expected hash?
instance
  ( FromJSON (h RollupBF)
  , FromJSON (a RollupBF)
  , SymbolicData a
  )
  => FromJSON (Base.Hash h a RollupBF)
  where
  parseJSON =
    withObject "Hash" $ \o -> do
      h <- o .: "hash"
      v <- o .: "value"
      pure Base.Hash {hHash = h, hValue = unconstrain v}

instance
  forall h a
   . ( ToSchema (h RollupBF)
     , ToSchema (a RollupBF)
     , Typeable h
     , Typeable a
     )
  => ToSchema (Base.Hash h a RollupBF)
  where
  declareNamedSchema _ = do
    hRef <- declareSchemaRef (Proxy @(h RollupBF))
    vRef <- declareSchemaRef (Proxy @(a RollupBF))
    let schema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties .~ fromList [("hash", hRef), ("value", vRef)]
            & required .~ ["hash", "value"]
    pure (named "Hash" schema)

instance
  KnownMerkleTree d
  => ToJSON (MerkleTree d RollupBF)
  where
  toJSON = toJSON . toConstant

instance
  KnownMerkleTree d
  => FromJSON (MerkleTree d RollupBF)
  where
  parseJSON v = (fromConstant @(Vector (MerkleTreeSize d) RollupBF)) <$> parseJSON v

-- MerkleTree is encoded as a JSON array of field elements
instance
  forall d
   . KnownNat d
  => ToSchema (MerkleTree d RollupBF)
  where
  declareNamedSchema _ = declareNamedSchema (Proxy @[RollupBF])

-- Product (:*:) is encoded as a JSON tuple; reuse tuple schema
instance
  forall f g
   . ( ToSchema (f RollupBF)
     , ToSchema (g RollupBF)
     , Typeable f
     , Typeable g
     )
  => ToSchema ((:*:) f g RollupBF)
  where
  declareNamedSchema _ = declareNamedSchema (Proxy @(f RollupBF, g RollupBF))
