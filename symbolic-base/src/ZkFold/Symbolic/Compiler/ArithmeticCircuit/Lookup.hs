{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup where

import           Control.DeepSeq        (NFData (..))
import           Data.Aeson
import           Data.Aeson.TH          (deriveToJSON)
import           Data.Bool              (otherwise)
import           Data.ByteString        (ByteString)
import           Data.Eq                (Eq (..))
import           Data.Maybe             (Maybe (..))
import           Data.Ord               (Ord (..))
import           Data.Semigroup         (Semigroup (..))
import           Data.Set               (Set)
import           Data.Typeable          (Typeable)
import qualified Data.Typeable          as T
import           GHC.Generics           (Generic, Par1, (:*:))
import           Prelude                (Show, undefined)
import qualified Type.Reflection        as R

import           ZkFold.Data.ByteString ()
import Data.Foldable (Foldable)
import Data.Functor (Functor)

newtype FunctionId f = FunctionId { funcHash :: ByteString }
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON)

-- | @LookupTable a f@ is a type of compact lookup table descriptions using ideas from relational algebra.
-- @a@ is a base field type, @f@ is a functor such that @f a@ is a type whose subset this lookup table describes.
data LookupTable a f where
  -- | @Ranges@ describes a set of disjoint segments of the base field.
  Ranges :: Set (a, a) -> LookupTable a Par1
  -- | @Product t u@ is a cartesian product of tables @t@ and @u@.
  Product :: LookupTable a f -> LookupTable a g -> LookupTable a (f :*: g)
  -- | @Plot f x@ is a plot of a function @f@ with @x@ as a domain.
  Plot ::
    (Functor f, Functor g, Typeable f, Typeable g) =>
    FunctionId (f a -> g a) -> LookupTable a f -> LookupTable a (f :*: g)

deriving instance Eq a => Eq (LookupTable a f)
deriving instance Ord a => Ord (LookupTable a f)
deriving instance Show a => Show (LookupTable a f)

$(deriveToJSON defaultOptions ''LookupTable)

instance NFData a => NFData (LookupTable a f) where
  rnf (Ranges rs)     = rnf rs
  rnf (Product t1 t2) = rnf (t1, t2)
  rnf (Plot i t)      = rnf (i, t)

instance (ToJSON a, ToJSONKey a) => ToJSONKey (LookupTable a f)

data LookupType a =
  forall f. (Foldable f, Typeable f) => LookupType { lTable :: LookupTable a f }

asRange :: LookupType a -> Maybe (Set (a, a))
asRange (LookupType (Ranges rs)) = Just rs
asRange _                        = Nothing

castT ::
  forall f g a. (Typeable f, Typeable g) =>
  LookupTable a f -> Maybe (LookupTable a g)
castT x
  | Just R.HRefl <- tf `R.eqTypeRep` tg = Just x
  | otherwise = Nothing
  where
    tf = R.typeRep :: R.TypeRep f
    tg = R.typeRep :: R.TypeRep g

deriving instance Show a => Show (LookupType a)

instance Eq a => Eq (LookupType a) where
  LookupType lt1 == LookupType lt2 = Just lt1 == castT lt2

instance Ord a => Ord (LookupType a) where
  LookupType lt1 `compare` LookupType lt2 =
    (T.typeRep lt1 `compare` T.typeRep lt2)
    <> (Just lt1 `compare` castT lt2)

instance ToJSON a => ToJSON (LookupType a) where
  toJSON (LookupType lt) = toJSON lt

instance (ToJSON a, ToJSONKey a) => ToJSONKey (LookupType a)

-- TODO: Finish this instance
instance FromJSON a => FromJSON (LookupType a) where
  parseJSON = undefined

instance (FromJSON a, FromJSONKey a) => FromJSONKey (LookupType a)

instance NFData a => NFData (LookupType a) where
  rnf (LookupType tbl) = rnf tbl
