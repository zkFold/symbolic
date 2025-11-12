{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Ledger.Types.Orphans (

) where

import Control.Applicative (pure)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withBool, withObject, (.:), (.=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.OpenApi (
  OpenApiType (..),
  ToSchema (..),
  declareSchemaRef,
  type_,
 )
import Data.OpenApi.Lens (properties, required)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1, (:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (FromConstant (..), MultiplicativeMonoid (..), ToConstant (..))
import ZkFold.Algebra.EllipticCurve.Class qualified as Elliptic
import ZkFold.Data.MerkleTree (MerkleTreeSize)
import ZkFold.Data.Orphans ()
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Ctx, Symbolic)
import ZkFold.Symbolic.Data.Bool (fromBool)
import ZkFold.Symbolic.Data.Bool qualified as SBool
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Combinators (KnownRegisterSize, RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleEntry, MerkleTree)
import ZkFold.Symbolic.Data.Payloaded (payloaded, restored)
import ZkFold.Symbolic.Data.UInt (UInt)
import Prelude (Integer, (.))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Algebra.EllipticCurve.Class (TwistedEdwards)
import GHC.IsList (IsList(..))
import Data.OpenApi.Internal.Schema (named)
import GHC.Natural (Natural)

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

-- TODO: Can we move these Aeson instances to their respective type-definition module?
instance FromJSON (FieldElement RollupBFInterpreter) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

instance ToSchema (FieldElement RollupBFInterpreter) where
  declareNamedSchema _ = declareNamedSchema (Proxy @RollupBF)

instance FromJSON (SBool.Bool RollupBFInterpreter) where
  parseJSON = withBool "Bool" $ \b -> pure $ fromConstant b

instance ToJSON (FieldElement RollupBFInterpreter) where
  toJSON v = toJSON (toConstant v :: RollupBF)

instance ToJSON (SBool.Bool RollupBFInterpreter) where
  toJSON b = toJSON (fromBool b Haskell.== one)

instance ToSchema (SBool.Bool RollupBFInterpreter) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Haskell.Bool)


instance forall n a. FromJSON (a RollupBFInterpreter) => FromJSON ((:.:) (Vector n) a RollupBFInterpreter) where
  parseJSON v = Comp1 <$> parseJSON v

instance forall n a. ToJSON (a RollupBFInterpreter) => ToJSON ((:.:) (Vector n) a RollupBFInterpreter) where
  toJSON (Comp1 x) = toJSON x

-- Composition Vector n :.: a is encoded as a JSON array of a
instance
  forall n a
   . (ToSchema (a RollupBFInterpreter), KnownNat n, Typeable a)
  => ToSchema ((:.:) (Vector n) a RollupBFInterpreter)
  where
  declareNamedSchema _ = declareNamedSchema (Proxy @[a RollupBFInterpreter])

deriving anyclass instance forall ud. FromJSON (MerkleEntry ud RollupBFInterpreter)

deriving anyclass instance forall ud. ToJSON (MerkleEntry ud RollupBFInterpreter)

deriving anyclass instance forall ud. (KnownNat ud, KnownNat (ud - 1)) => ToSchema (MerkleEntry ud RollupBFInterpreter)

instance forall n r. (KnownRegisterSize r, KnownNat n) => ToJSON (Int n r RollupBFInterpreter) where
  toJSON = toJSON . toConstant

instance forall n r. (KnownRegisterSize r, KnownNat n) => FromJSON (Int n r RollupBFInterpreter) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

instance forall n r. (KnownNat n, Typeable r) => ToSchema (Int n r RollupBFInterpreter) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Integer)

-- Matches JSON encoding of UInt as a natural number.
instance forall n r. (KnownNat n, Typeable r) => ToSchema (UInt n r RollupBFInterpreter) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Natural)

instance
  forall a
   . KnownFFA a 'Auto RollupBFInterpreter
  => ToJSON (FFA a 'Auto RollupBFInterpreter)
  where
  toJSON v = toJSON (toConstant v)

instance
  forall a
   . KnownFFA a 'Auto RollupBFInterpreter
  => FromJSON (FFA a 'Auto RollupBFInterpreter)
  where
  parseJSON v = fromConstant @Integer <$> parseJSON v

-- FFA values are encoded as integers
instance
  forall a
   . KnownFFA a 'Auto RollupBFInterpreter
  => ToSchema (FFA a 'Auto RollupBFInterpreter)
  where
  declareNamedSchema _ = declareNamedSchema (Proxy @Integer)

instance
  forall curve a
   . KnownFFA a 'Auto RollupBFInterpreter
  => ToJSON (AffinePoint curve (FFA a 'Auto) RollupBFInterpreter)
  where
  toJSON p =
    let Elliptic.AffinePoint x y = affinePoint p
     in object ["x" .= x, "y" .= y]

-- TODO: What if the parsed point is not on the curve?
instance
  forall curve a
   . KnownFFA a 'Auto RollupBFInterpreter
  => FromJSON (AffinePoint curve (FFA a 'Auto) RollupBFInterpreter)
  where
  parseJSON =
    withObject "AffinePoint" $ \o -> do
      x <- o .: "x"
      y <- o .: "y"
      pure (AffinePoint (Elliptic.AffinePoint x y))

-- Apparently, there is an issue with having instance for arbitrary curve, so we hardcode the Jubjub curve for now.
instance forall a. KnownFFA a 'Auto RollupBFInterpreter => ToSchema (AffinePoint (TwistedEdwards "jubjub") (FFA a 'Auto) RollupBFInterpreter) where
  declareNamedSchema _ = do
    xyRef <- declareSchemaRef (Proxy @(FFA a 'Auto RollupBFInterpreter))
    let schema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties .~ fromList [("x", xyRef), ("y", xyRef)]
            & required .~ ["x", "y"]
    pure (named "AffinePoint" schema)


instance
  ( ToJSON (h RollupBFInterpreter)
  , ToJSON (a RollupBFInterpreter)
  , SymbolicData a
  )
  => ToJSON (Base.Hash h a RollupBFInterpreter)
  where
  toJSON Base.Hash {..} =
    let Identity v = restored hValue
     in object ["hash" .= hHash, "value" .= v]

-- TODO: What if the parsed hash does not match the expected hash?
instance
  ( FromJSON (h RollupBFInterpreter)
  , FromJSON (a RollupBFInterpreter)
  , SymbolicData a
  )
  => FromJSON (Base.Hash h a RollupBFInterpreter)
  where
  parseJSON =
    withObject "Hash" $ \o -> do
      h <- o .: "hash"
      v <- o .: "value"
      pure Base.Hash {hHash = h, hValue = payloaded (Identity v)}

instance
  forall h a
   . ( ToSchema (h RollupBFInterpreter)
     , ToSchema (a RollupBFInterpreter)
     , Typeable h
     , Typeable a
     )
  => ToSchema (Base.Hash h a RollupBFInterpreter)
  where
  declareNamedSchema _ = do
    hRef <- declareSchemaRef (Proxy @(h RollupBFInterpreter))
    vRef <- declareSchemaRef (Proxy @(a RollupBFInterpreter))
    let schema =
          Haskell.mempty
            & type_ ?~ OpenApiObject
            & properties .~ fromList [("hash", hRef), ("value", vRef)]
            & required .~ ["hash", "value"]
    pure (named "Hash" schema)

instance
  KnownMerkleTree d
  => ToJSON (MerkleTree d RollupBFInterpreter)
  where
  toJSON = toJSON . toConstant

instance
  KnownMerkleTree d
  => FromJSON (MerkleTree d RollupBFInterpreter)
  where
  parseJSON v = (fromConstant @(Vector (MerkleTreeSize d) RollupBF)) <$> parseJSON v

-- MerkleTree is encoded as a JSON array of field elements
instance
  forall d
   . KnownNat d
  => ToSchema (MerkleTree d RollupBFInterpreter)
  where
  declareNamedSchema _ = declareNamedSchema (Proxy @[RollupBF])

instance
  ( ToJSON (f RollupBFInterpreter)
  , ToJSON (g RollupBFInterpreter)
  )
  => ToJSON ((:*:) f g RollupBFInterpreter)
  where
  toJSON (x :*: y) = toJSON (x, y)

instance
  ( FromJSON (f RollupBFInterpreter)
  , FromJSON (g RollupBFInterpreter)
  )
  => FromJSON ((:*:) f g RollupBFInterpreter)
  where
  parseJSON v = do
    (x, y) <- parseJSON v
    pure (x :*: y)


-- Product (:*:) is encoded as a JSON tuple; reuse tuple schema
instance
  forall f g
   . ( ToSchema (f RollupBFInterpreter)
     , ToSchema (g RollupBFInterpreter)
     , Typeable f
     , Typeable g
     )
  => ToSchema ((:*:) f g RollupBFInterpreter)
  where
  declareNamedSchema _ = declareNamedSchema (Proxy @(f RollupBFInterpreter, g RollupBFInterpreter))