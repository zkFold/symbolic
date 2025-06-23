{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Apps.KYC where

import Data.Aeson
import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import ZkFold.Algebra.Class (FromConstant (fromConstant))
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Data.Vector (Vector, head, tail, toVector)
import ZkFold.Symbolic.Class (Symbolic (BaseField))
import ZkFold.Symbolic.Data.Bool (Bool, not, (&&))
import ZkFold.Symbolic.Data.ByteString (ByteString, Resize (resize), concat, toWords)
import ZkFold.Symbolic.Data.Combinators (
  Ceil,
  GetRegisterSize,
  Iso (..),
  KnownRegisterSize,
  KnownRegisters,
  RegisterSize (..),
 )
import ZkFold.Symbolic.Data.Eq (Eq ((==)), elem)
import ZkFold.Symbolic.Data.Ord (Ord ((>=)))
import ZkFold.Symbolic.Data.UInt (OrdWord, UInt)
import ZkFold.Symbolic.Interpreter (Interpreter)
import Prelude (String, error, ($), (.))

type KYCByteString context = ByteString 256 context

type KYCHash context = UInt 256 Auto context

{-
>>> type Prime256_1 = 28948022309329048855892746252171976963363056481941560715954676764349967630337
>>> :{
exKYC :: KYCData (Interpreter (Zp Prime256_1))
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
  , kycID :: UInt 64 Auto context
  }
  deriving Generic

data User r context = User
  { userAge :: UInt 64 r context
  , userCountry :: ByteString 128 context
  }
  deriving Generic

instance Symbolic context => FromJSON (KYCData 256 context)

instance Symbolic (Interpreter (Zp p)) => ToJSON (KYCData 256 (Interpreter (Zp p)))

isCitizen :: Symbolic c => KYCByteString c -> Vector n (KYCByteString c) -> Bool c
isCitizen = elem

kycExample
  :: forall n r rsc context
   . ( Eq (KYCHash context)
     , KnownNat (Ceil (GetRegisterSize (BaseField context) 64 r) OrdWord)
     , KnownNat n
     , KnownNat rsc
     , KnownRegisterSize r
     , KnownRegisters context 64 r
     , Symbolic context
     )
  => KYCData n context -> KYCHash context -> Bool context
kycExample kycData hash =
  let
    v :: Vector 3 (ByteString 64 context)
    v = toWords $ resize $ kycValue kycData

    correctHash :: Bool context
    correctHash = hash == kycHash kycData

    user :: User r context
    user = User (from $ head v) (concat $ tail v)

    validAge :: Bool context
    validAge = userAge user >= fromConstant (18 :: Natural)

    validCountry :: Bool context
    validCountry = not $ elem (userCountry user) (restrictedCountries @rsc)
   in
    correctHash && validAge && validCountry

userA
  :: forall r context
   . ( KnownRegisterSize r
     , Symbolic context
     )
  => User r context
userA =
  User
    (fromConstant (25 :: Natural))
    (fromConstant $ iso3166 "JPN")

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
   . ( KnownNat m
     , Symbolic context
     )
  => Vector m (ByteString 128 context)
restrictedCountries =
  fromJust $
    toVector $
      fromConstant . iso3166
        <$> [ "FRA"
            , "DEU"
            ]
