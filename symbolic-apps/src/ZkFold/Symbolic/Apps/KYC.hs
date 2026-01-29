{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Apps.KYC where

import Data.Aeson
import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import ZkFold.Algebra.Class (FromConstant (fromConstant), PrimeField)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Data.Eq (Eq ((==)), elem)
import ZkFold.Data.Vector (Vector, head, tail, toVector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool, not, (&&))
import ZkFold.Symbolic.Data.ByteString (ByteString, concat, resize, toWords)
import ZkFold.Symbolic.Data.Ord ((>=))
import ZkFold.Symbolic.Data.UInt
import Prelude (String, error, ($), (.))

type KYCByteString context = ByteString 256 context

type KYCHash context = UInt 256 context

{-
>>> type Prime256_1 = 28948022309329048855892746252171976963363056481941560715954676764349967630337
>>> :{
exKYC :: KYCData (Zp Prime256_1)
exKYC = KYCData
  (fromConstant (1000 :: Natural))
  (fromConstant (2000 :: Natural))
  (fromConstant (3000 :: Natural))
  (fromConstant (4000 :: Natural))
:}
>>> encode exKYC
"{\"kycHash\":\"bb8\",\"kycID\":4000,\"kycType\":\"3e8\",\"kycValue\":\"7d0\"}"
-}
data KYCData n context = KYCData
  { kycType :: KYCByteString context
  , kycValue :: ByteString n context
  , kycHash :: KYCHash context
  , kycID :: UInt 64 context
  }
  deriving Generic

data User context = User
  { userAge :: UInt 64 context
  , userCountry :: ByteString 128 context
  }
  deriving Generic

instance (Symbolic c, KnownUInt 256 c, KnownUInt 64 c) => FromJSON (KYCData 256 c)

instance
  (PrimeField (Zp p), 3 <= p, KnownUInt 256 (Zp p), KnownUInt 64 (Zp p))
  => ToJSON (KYCData 256 (Zp p))

isCitizen :: Symbolic c => KYCByteString c -> Vector n (KYCByteString c) -> Bool c
isCitizen = elem

kycExample
  :: forall n rsc context
   . ( Symbolic context
     , KnownUInt 64 context
     , KnownNat n
     , KnownNat rsc
     )
  => KYCData n context -> KYCHash context -> Bool context
kycExample kycData hash =
  let
    v :: Vector 3 (ByteString 64 context)
    v = toWords $ resize $ kycValue kycData

    correctHash :: Bool context
    correctHash = hash == kycHash kycData

    user :: User context
    user = User (beBSToUInt $ head v) (concat $ tail v)

    validAge :: Bool context
    validAge = userAge user >= fromConstant (18 :: Natural)

    validCountry :: Bool context
    validCountry = not $ elem (userCountry user) (restrictedCountries @rsc)
   in
    correctHash && validAge && validCountry

userA :: forall c. (Symbolic c, KnownUInt 64 c) => User c
userA = User (fromConstant (25 :: Natural)) (fromConstant $ iso3166 "JPN")

iso3166 :: String -> Natural
iso3166 = \case
  "DEU" -> 276
  "FRA" -> 250
  "GBR" -> 826
  "JPN" -> 392
  "USA" -> 840
  _ -> error "Unknown ISO country code"

restrictedCountries
  :: forall m context
   . (Symbolic context, KnownNat m)
  => Vector m (ByteString 128 context)
restrictedCountries =
  fromJust $
    toVector $
      fromConstant . iso3166
        <$> [ "FRA"
            , "DEU"
            ]
