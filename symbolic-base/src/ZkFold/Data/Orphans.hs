{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Data.Orphans where

import           Control.DeepSeq  (NFData, NFData1)
import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Data.Functor     (Functor, (<$>))
import           Data.Functor.Rep (Representable (..), WrappedRep (..))
import           GHC.Generics     (Par1 (..), U1 (..), (:*:) (..), (:.:))
import Test.QuickCheck (Arbitrary (..))
import Control.Monad (return)
import Control.Applicative ((<*>))

instance NFData (U1 a)
instance NFData1 U1
instance NFData1 Par1
instance NFData a => NFData (Par1 a)
instance (NFData1 f, NFData1 g, NFData a, NFData (f a), NFData (g a)) => NFData ((:*:) f g a)
instance (NFData1 f, NFData1 g) => NFData1 (f :*: g)
instance (NFData1 f, NFData1 g, NFData a, NFData (f a), NFData (f (g a))) => NFData ((:.:) f g a)
instance (Functor f, NFData1 f, NFData1 g) => NFData1 (f :.: g)

instance Arbitrary (U1 a) where
  arbitrary = return U1
instance Arbitrary a => Arbitrary (Par1 a) where
  arbitrary = Par1 <$> arbitrary
instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary ((f :*: g) a) where
  arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (U1 a)
instance FromJSON a => FromJSON (U1 a)
instance ToJSON a => ToJSON (Par1 a)
instance FromJSON a => FromJSON (Par1 a)
instance (ToJSON a, ToJSON (f a), ToJSON (g a)) => ToJSON ((:*:) f g a)
instance (FromJSON a, FromJSON (f a), FromJSON (g a)) => FromJSON ((:*:) f g a)
instance (ToJSON a, ToJSON (f (g a)), ToJSON (g a)) => ToJSON ((:.:) f g a)
instance (FromJSON a, FromJSON (f (g a)), FromJSON (g a)) => FromJSON ((:.:) f g a)

deriving newtype instance Binary (Rep f) => Binary (WrappedRep f)
