{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup where

import           Control.DeepSeq
import           Data.Aeson.Types
import           Data.ByteString  (ByteString)
import           Data.Set
import qualified Data.Text        as T
import           Data.Typeable
import           GHC.Base
import           GHC.Generics     (Generic, Par1, (:*:))
import           Prelude          (Show)

newtype FunctionId f = FunctionId { funcHash :: ByteString }
  deriving (Eq, Ord, Show, Generic)

-- | @LookupTable a f@ is a type of compact lookup table descriptions using ideas from relational algebra.
-- @a@ is a base field type, @f@ is a functor such that @f a@ is a type whose subset this lookup table describes.
data LookupTable a f where
  -- | @Ranges@ describes a set of disjoint segments of the base field.
  Ranges :: Set (a, a) -> LookupTable a Par1
  -- | @Product t u@ is a cartesian product of tables @t@ and @u@.
  Product :: LookupTable a f -> LookupTable a g -> LookupTable a (f :*: g)
  -- | @Plot f x@ is a plot of a function @f@ with @x@ as a domain.
  Plot :: FunctionId (f a -> g a) -> LookupTable a f -> LookupTable a (f :*: g)

deriving instance (Eq a) => Eq (LookupTable a f)
deriving instance (Ord a) => Ord (LookupTable a f)
deriving instance (Show a) => Show (LookupTable a f)

instance (ToJSON a) => ToJSON (LookupTable a f) where
  toJSON (Ranges p)    = toJSON p
  toJSON (Product _ _) = String . T.pack $ "Product"
  toJSON (Plot _ _)    = String . T.pack $ "Plot"

instance (ToJSON a) => ToJSONKey (LookupTable a f)

instance (FromJSON a) => FromJSON (LookupTable a f) where
  parseJSON = undefined

instance (FromJSON a) => FromJSONKey (LookupTable a f)

data LookupType a = forall f. Typeable f => LookupType { lTable :: LookupTable a f }

asRange :: LookupType a -> Maybe (Set (a, a))
asRange (LookupType (Ranges rs)) = Just rs
asRange _ = Nothing

deriving instance (Show a) => Show (LookupType a)

instance (Eq a, Typeable a) => Eq (LookupType a) where
  LookupType lt1 == LookupType lt2 = Just lt1 == cast lt2

instance (Ord a, Typeable a) => Ord (LookupType a) where
  LookupType lt1 `compare` LookupType lt2 =
    (typeRep lt1 `compare` typeRep lt2)
    <> (Just lt1 `compare` cast lt2)

instance ToJSON a => ToJSON (LookupType a) where
  toJSON (LookupType lt) = toJSON lt

instance ToJSON a => ToJSONKey (LookupType a)

instance FromJSON a => FromJSON (LookupType a) where
  parseJSON (Object v) = v .: "lookupType"
  parseJSON invalid    =
    prependFailure "parsing LookupType failed, "
        (typeMismatch "Object" invalid)

instance (FromJSON a, FromJSONKey a) => FromJSONKey (LookupType a)

instance NFData (LookupType a) where
  rnf = rwhnf
