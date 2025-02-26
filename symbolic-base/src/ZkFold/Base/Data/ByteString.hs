{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Base.Data.ByteString
  ( Binary (..)
  , Binary1
  , toByteString
  , fromByteString
  , putWord8
  , getWord8
  , skip
  , LittleEndian (..)
  , BigEndian (..)
  ) where

import           Control.Applicative    (many)
import qualified Data.Aeson             as Aeson
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.Text.Encoding     as Text
import           GHC.Generics           (Par1, U1, (:*:), (:.:))
import           Numeric.Natural        (Natural)
import           Prelude
import           Test.QuickCheck        (Arbitrary (..))

instance Aeson.FromJSON Strict.ByteString where
  parseJSON o =
    either fail return
    . Base64.decode
    . Text.encodeUtf8
    =<< Aeson.parseJSON o

instance Aeson.ToJSON Strict.ByteString where
  toJSON = Aeson.toJSON . Text.decodeUtf8 . Base64.encode

instance Aeson.FromJSONKey Strict.ByteString
instance Aeson.ToJSONKey Strict.ByteString

class (forall a. Binary a => Binary (f a)) => Binary1 f
instance (forall a. Binary a => Binary (f a)) => Binary1 f
instance Binary (U1 a)
instance Binary a => Binary (Par1 a)
instance (Binary (f a), Binary (g a)) => Binary ((f :*: g) a)
instance Binary (f (g a)) => Binary ((f :.: g) a)

toByteString :: Binary a => a -> Strict.ByteString
toByteString = Lazy.toStrict . runPut . put

fromByteString :: Binary a => Strict.ByteString -> Maybe a
fromByteString x = case runGetOrFail get (Lazy.fromStrict x) of
  Left _ -> Nothing
  Right (leftover, _, a) ->
    if Lazy.null leftover then Just a else Nothing

-- Little-endian encoding for unsigned & unsized integers
-- un little, deux little, trois little endians
newtype LittleEndian = LittleEndian {unLittleEndian :: Natural}
  deriving stock (Read, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)
instance Binary LittleEndian where
  get = do
    ns <- many getWord8
    let accum (!pw :: Natural, !acc :: Natural) w8 = (pw * 256, pw * fromIntegral w8 + acc)
        littleEndian = LittleEndian (snd $ foldl' accum (1, 0) ns)
    return littleEndian
  put (LittleEndian n)
    | n == 0 = mempty
    | otherwise =
      let (n', r) = n `divMod` 256
      in putWord8 (fromIntegral r) <> put (LittleEndian n')
instance Arbitrary LittleEndian where
  arbitrary = fromInteger . abs <$> arbitrary

-- Big-endian encoding for unsigned & unsized integers
newtype BigEndian = BigEndian {unBigEndian :: Natural}
  deriving stock (Read, Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)
instance Binary BigEndian where
  get = do
    ns <- many getWord8
    let accum n w8 = n * 256 + fromIntegral w8
        bigEndian = BigEndian (foldl' accum 0 ns)
    return bigEndian
  put (BigEndian n)
    | n == 0 = mempty
    | otherwise =
      let (n', r) = n `divMod` 256
      in put (BigEndian n') <> putWord8 (fromIntegral r)
instance Arbitrary BigEndian where
  arbitrary = fromInteger . abs <$> arbitrary
