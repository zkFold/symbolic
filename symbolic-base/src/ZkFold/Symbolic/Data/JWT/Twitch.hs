{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT.Twitch (TwitchPayload (..)) where

import           Control.DeepSeq                    (NFData)
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Generic.Random                     (genericArbitrary, uniform)
import           GHC.Generics                       (Generic, Par1 (..))
import           Prelude                            (type (~), (.))
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary (..))

import qualified ZkFold.Base.Data.Vector            as V
import           ZkFold.Symbolic.Algorithms.RSA     as RSA
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators   hiding (toBits)
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.JWT.RS256
import           ZkFold.Symbolic.Data.JWT.Utils
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), (@+))

-- | Json Web Token payload with information about the user
-- https://dev.twitch.tv/docs/extensions/reference/#jwt-schema
--
data TwitchPayload ctx
    = TwitchPayload
        { twChannelId    :: VarByteString 512 ctx
        -- ^ Numeric ID of the channel from which your front-end is being served.
        , twExp          :: VarByteString 80 ctx
        -- ^ Expiration time (seconds since Unix epoch), a decimal number.
        , twIsUnlinked   :: VarByteString 40 ctx
        -- ^ true when the token is for a user that previously shared identity; otherwise, false.
        , twOpaqueUserId :: VarByteString 512 ctx
        -- ^ Identifies the session using this JWT.
        , twPubsubPerms  :: PubSubPerms ctx
        -- ^ Defines the ability of the token holder to send and listen to messages for your extension.
        , twRole         :: VarByteString 88 ctx
        -- ^ Type of user for whom this JWT has been signed. This is required.
        , twUserId       :: VarByteString 512 ctx
        -- ^ The user's Twitch user ID. This is provided only for users who allow your extension to identify them.
        }
    deriving Generic

-- | @pListen@ and @pSend@ contain the topics the associated user is allowed to listen to and publish to, respectively.
--
data PubSubPerms ctx =
    PubSubPerms
        { pListen :: VarByteString 2048 ctx
        , pSend   :: VarByteString 2048 ctx
        }
    deriving Generic

deriving instance
    ( P.Eq (ctx (V.Vector 2048))
    , P.Eq (ctx Par1)
    ) => P.Eq (PubSubPerms ctx)
deriving instance
    ( P.Show (ctx (V.Vector 2048))
    , P.Show (ctx Par1)
    ) => P.Show (PubSubPerms ctx)
deriving instance Symbolic ctx => SymbolicData (PubSubPerms ctx)
deriving instance Symbolic ctx => SymbolicInput (PubSubPerms ctx)
instance Symbolic ctx => Arbitrary (PubSubPerms ctx) where
    arbitrary = genericArbitrary uniform

instance (Symbolic ctx, Context (PubSubPerms ctx) ~ ctx) => IsSymbolicJSON (PubSubPerms ctx) where
    type MaxLength (PubSubPerms ctx) = 4248
    toJsonBits PubSubPerms{..} =
                    (fromType @"{\"listen\":") @+ pListen
        `VB.append` (fromType @",\"send\":")   @+ pSend
        `VB.append` (fromType @"}")

instance Symbolic ctx => FromJSON (PubSubPerms ctx) where
    parseJSON = genericParseJSON (aesonPrefix snakeCase) . stringify

deriving instance
    ( P.Eq (ctx (V.Vector 40))
    , P.Eq (ctx (V.Vector 80))
    , P.Eq (ctx (V.Vector 88))
    , P.Eq (ctx (V.Vector 512))
    , P.Eq (ctx (V.Vector 2048))
    , P.Eq (ctx Par1)
    ) => P.Eq (TwitchPayload ctx)

deriving instance
    ( P.Show (ctx (V.Vector 40))
    , P.Show (ctx (V.Vector 80))
    , P.Show (ctx (V.Vector 88))
    , P.Show (ctx (V.Vector 512))
    , P.Show (ctx (V.Vector 2048))
    , P.Show (ctx Par1)
    ) => P.Show (TwitchPayload ctx)
deriving instance Symbolic ctx => SymbolicData (TwitchPayload ctx)
deriving instance Symbolic ctx => SymbolicInput (TwitchPayload ctx)
instance Symbolic ctx => Arbitrary (TwitchPayload ctx) where
    arbitrary = genericArbitrary uniform

instance Symbolic ctx => FromJSON (TwitchPayload ctx) where
    parseJSON = genericParseJSON (aesonPrefix snakeCase) . stringify

instance (Symbolic ctx) => IsSymbolicJSON (TwitchPayload ctx) where
    type MaxLength (TwitchPayload ctx) = 6792
    toJsonBits TwitchPayload{..} =
                    (fromType @"{\"channel_id\":\"")   @+ twChannelId
        `VB.append` (fromType @"\",\"exp\":\"") @+ twExp
        `VB.append` (fromType @"\",\"is_unlinked\":") @+ twIsUnlinked
        `VB.append` (fromType @",\"opaque_user_id\":\"") @+ twOpaqueUserId
        `VB.append` (fromType @"\",\"pubsub_perms\":")  @+ toJsonBits twPubsubPerms
        `VB.append` (fromType @",\"role\":\"") @+ twRole
        `VB.append` (fromType @"\",\"user_id\":\"") @+ twUserId
        `VB.append` (fromType @"\"}")

instance
  ( Symbolic ctx
  , NFData (ctx (V.Vector 8))
  , NFData (ctx (V.Vector 9056))
  ) => IsBits (TwitchPayload ctx) where
    type BitCount (TwitchPayload ctx) = 9056
    toBits = toAsciiBits

instance (Symbolic ctx, TokenBits (TwitchPayload ctx), RSA.RSA 2048 10328 ctx) => IsTokenPayload "RS256" (TwitchPayload ctx) where
    signPayload jPayload SigningKey{..} = (jHeader, signature)
        where
            jHeader = TokenHeader "RS256" prvKid "JWT"
            signature = RSA.signVar (tokenBits jHeader jPayload) prvKey

    verifyJWT jHeader jPayload signature Certificate{..} = (tokenVerified, secretHash)
        where
            (sigVerified, secretHash) = RSA.verifyVar (tokenBits jHeader jPayload) signature pubKey
            tokenVerified = pubKid == hdKid jHeader && sigVerified
