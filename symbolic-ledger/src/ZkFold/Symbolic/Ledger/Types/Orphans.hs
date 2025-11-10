{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Ledger.Types.Orphans (

) where

import Control.Applicative (pure)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withBool, withObject, (.:), (.=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1, (:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat)
import ZkFold.Algebra.Class (FromConstant (..), MultiplicativeMonoid (..), ToConstant (..))
import ZkFold.Algebra.EllipticCurve.Class qualified as Elliptic
import ZkFold.Data.Orphans ()
import ZkFold.Data.MerkleTree (MerkleTreeSize)
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
import Prelude (Integer, (.))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Hash

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

deriving anyclass instance forall ud. ToJSON (MerkleEntry ud RollupBFInterpreter)

instance forall n r. (KnownRegisterSize r, KnownNat n) => ToJSON (Int n r RollupBFInterpreter) where
  toJSON = toJSON . toConstant

instance forall n r. (KnownRegisterSize r, KnownNat n) => FromJSON (Int n r RollupBFInterpreter) where
  parseJSON v = fromConstant @Integer <$> parseJSON v

instance
  forall a
   . KnownFFA a 'Auto RollupBFInterpreter
  => ToJSON (FFA a 'Auto RollupBFInterpreter)
  where
  toJSON v = toJSON (toConstant v)

instance
  forall curve a
   . KnownFFA a 'Auto RollupBFInterpreter
  => ToJSON (AffinePoint curve (FFA a 'Auto) RollupBFInterpreter)
  where
  toJSON p =
    let Elliptic.AffinePoint x y = affinePoint p
     in object ["x" .= x, "y" .= y]

instance
  forall a
   . KnownFFA a 'Auto RollupBFInterpreter
  => FromJSON (FFA a 'Auto RollupBFInterpreter)
  where
  parseJSON v = fromConstant @Integer <$> parseJSON v

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
  KnownMerkleTree d
  => ToJSON (MerkleTree d RollupBFInterpreter)
  where
  toJSON = toJSON . toConstant

instance
  KnownMerkleTree d
  => FromJSON (MerkleTree d RollupBFInterpreter)
  where
  parseJSON v = (fromConstant @(Vector (MerkleTreeSize d) RollupBF)) <$> parseJSON v

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
