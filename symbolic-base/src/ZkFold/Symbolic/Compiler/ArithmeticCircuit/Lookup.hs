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


data LookupType a = forall f. (Functor f, Typeable f) => LookupType { lTable :: LookupTable a f }

deriving instance (Show a) => Show (LookupType a)

tryEq :: (Typeable f, Typeable g) => LookupTable a f -> LookupTable a g -> Bool
tryEq a b = typeRep a == typeRep b

instance Eq a => Eq (LookupType a) where
  (==) :: LookupType a -> LookupType a -> Bool
  (==) (LookupType lt1) (LookupType lt2) = tryEq lt1 lt2

tryCompare :: (Typeable f, Typeable g) => LookupTable a f -> LookupTable a g -> Ordering
tryCompare a b = compare (typeRep a) (typeRep b)-- then compare a (fromJust (cast b)) else EQ

instance Ord a => Ord (LookupType a) where
  compare :: LookupType a -> LookupType a -> Ordering
  compare (LookupType lt1) (LookupType lt2) = tryCompare lt1 lt2

instance (ToJSON a) => ToJSON (LookupType a) where
  toJSON (LookupType lt) = toJSON lt
instance (ToJSON a) => ToJSONKey (LookupType a)

instance (FromJSON a) => FromJSON (LookupType a) where
  parseJSON (Object v) = v .: "lookupType"
  parseJSON invalid    =
    prependFailure "parsing LookupType failed, "
        (typeMismatch "Object" invalid)
instance (FromJSONKey a, FromJSON a) => FromJSONKey (LookupType a)


-- | @LookupTable a f@ is a type of compact lookup table descriptions using ideas from relational algebra.
-- @a@ is a base field type, @f@ is a functor such that @f a@ is a type whose subset this lookup table describes.
data LookupTable a f where
  -- | @Ranges@ describes a set of disjoint intervals on the base field.
  Ranges :: Set (a, a) -> LookupTable a Par1
  -- | @Product t u@ is a cartesian product of tables @t@ and @u@.
  Product :: LookupTable a f -> LookupTable a g -> LookupTable a (f :*: g)
  -- | @Plot f x@ is a plot of a function @f@ with @x@ as a domain.
  Plot :: FunctionId (f a -> g a) -> LookupTable a f -> LookupTable a (f :*: g)


newtype FunctionId f = FunctionId { funcHash :: ByteString }
  deriving (Eq, Ord, Show, Generic)

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

instance NFData (LookupType a) where
  rnf = rwhnf

isRange :: LookupType a -> Bool
isRange (LookupType l) = case l of
  Ranges _ -> True
  _        -> False

fromRange :: LookupType a -> Set (a, a)
fromRange (LookupType (Ranges a)) = a
fromRange _                       = error "it's not Range lookup"
