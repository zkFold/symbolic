{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT.Google (GooglePayload (..)) where

import           Control.DeepSeq                    (NFData)
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import qualified Data.Aeson                         as JSON
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Maybe                         (fromMaybe)
import           Data.Scientific                    (toBoundedInteger)
import qualified Data.Text                          as T
import           Generic.Random                     (genericArbitrary, uniform)
import           GHC.Generics                       (Generic, Par1 (..))
import           Prelude                            (fmap, type (~), ($), (.))
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary (..))

import qualified ZkFold.Base.Data.Vector            as V
import qualified ZkFold.Symbolic.Algorithms.RSA     as RSA
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.JWT.RS256
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), (@+))


-- | Json Web Token payload with information about the issuer, bearer and TTL
--
data GooglePayload ctx
    = GooglePayload
        { plIss           :: VarByteString 256 ctx
        -- ^ Issuer (who created and signed the token)
        , plAzp           :: VarByteString 1024 ctx
        -- ^ Authorized party (the party to which the token was issued)
        , plAud           :: VarByteString 1024 ctx
        -- ^ Audience (who or what the token is intended for)
        , plSub           :: VarByteString 256 ctx
        -- ^ Subject (whom the token refers to)
        , plHd            :: VarByteString 256 ctx
        -- ^ Hosted domain
        , plEmail         :: VarByteString 512 ctx
        -- ^ User email limited to 64 characters
        , plEmailVerified :: VarByteString 40 ctx
        -- ^ "true" or "false", max 5 bytes
        , plAtHash        :: VarByteString 256 ctx
        -- ^ Access token hash value
        , plName          :: VarByteString 512 ctx
        -- ^ Full name limited to 64 characters
        , plPicture       :: VarByteString 1024 ctx
        -- ^ URL to the profile picture limited to 128 characters
        , plGivenName     :: VarByteString 256 ctx
        -- ^ Given name limited to 32 characters
        , plFamilyName    :: VarByteString 256 ctx
        -- ^ Family name limited to 32 characters
        , plIat           :: VarByteString 80 ctx
        -- ^ Issued at (seconds since Unix epoch), a decimal number
        , plExp           :: VarByteString 80 ctx
        -- ^ Expiration time (seconds since Unix epoch), a decimal number
        }
    deriving Generic

deriving instance
    ( P.Eq (ctx (V.Vector 40))
    , P.Eq (ctx (V.Vector 80))
    , P.Eq (ctx (V.Vector 256))
    , P.Eq (ctx (V.Vector 512))
    , P.Eq (ctx (V.Vector 1024))
    , P.Eq (ctx Par1)
    ) => P.Eq (GooglePayload ctx)
deriving instance
    ( P.Show (ctx (V.Vector 40))
    , P.Show (ctx (V.Vector 80))
    , P.Show (ctx (V.Vector 256))
    , P.Show (ctx (V.Vector 512))
    , P.Show (ctx (V.Vector 1024))
    , P.Show (ctx Par1)
    ) => P.Show (GooglePayload ctx)
deriving instance Symbolic ctx => SymbolicData (GooglePayload ctx)
deriving instance Symbolic ctx => SymbolicInput (GooglePayload ctx)
instance Symbolic ctx => Arbitrary (GooglePayload ctx) where
    arbitrary = genericArbitrary uniform

instance Symbolic ctx => FromJSON (GooglePayload ctx) where
    parseJSON = genericParseJSON (aesonPrefix snakeCase) . stringify
        where
            -- We store everything as ByteStrings for simplicity.
            -- We need to convert ints and bools to strings to avoid conversion errors
            --
            stringify :: JSON.Value -> JSON.Value
            stringify (JSON.Number s) =
                JSON.String (T.pack . P.show . fromMaybe (P.error "instance FromJSON JWT :: Invalid integer") . toBoundedInteger @P.Int $ s)
            stringify (JSON.Bool b)   = JSON.String (T.pack $ P.show b)
            stringify (JSON.Object o) = JSON.Object $ fmap stringify o
            stringify rest            = rest

instance (Symbolic ctx, Context (GooglePayload ctx) ~ ctx) => IsSymbolicJSON (GooglePayload ctx) where
    type MaxLength (GooglePayload ctx) = 7088
    toJsonBits GooglePayload{..} =
                    (fromType @"{\"iss\":\"")   @+ plIss
        `VB.append` (fromType @"\",\"azp\":\"") @+ plAzp
        `VB.append` (fromType @"\",\"aud\":\"") @+ plAud
        `VB.append` (fromType @"\",\"sub\":\"") @+ plSub
        `VB.append` (fromType @"\",\"hd\":\"")  @+ plHd
        `VB.append` (fromType @"\",\"email\":\"") @+ plEmail
        `VB.append` (fromType @"\",\"email_verified\":") @+ plEmailVerified
        `VB.append` (fromType @",\"at_hash\":\"") @+ plAtHash
        `VB.append` (fromType @"\",\"name\":\"")  @+ plName
        `VB.append` (fromType @"\",\"picture\":\"") @+ plPicture
        `VB.append` (fromType @"\",\"given_name\":\"")  @+ plGivenName
        `VB.append` (fromType @"\",\"family_name\":\"") @+ plFamilyName
        `VB.append` (fromType @"\",\"iat\":") @+ plIat
        `VB.append` (fromType @",\"exp\":")   @+ plExp
        `VB.append` (fromType @"}")

instance
  ( Symbolic ctx
  , NFData (ctx (V.Vector 8))
  , NFData (ctx (V.Vector 9456))
  ) => IsBits (GooglePayload ctx) where
    type BitCount (GooglePayload ctx) = 9456
    toBits = toAsciiBits

instance (Symbolic ctx, TokenBits (GooglePayload ctx), RSA.RSA 2048 10328 ctx) => IsTokenPayload "RS256" (GooglePayload ctx) where
    signPayload jPayload SigningKey{..} = (jHeader, signature)
        where
            jHeader = TokenHeader "RS256" prvKid "JWT"

            signature = RSA.signVar (tokenBits jHeader jPayload) prvKey

    verifyJWT jHeader jPayload signature Certificate{..} = (tokenVerified, secretHash)
        where
            (sigVerified, secretHash) = RSA.verifyVar (tokenBits jHeader jPayload) signature pubKey
            tokenVerified = pubKid == hdKid jHeader && sigVerified
