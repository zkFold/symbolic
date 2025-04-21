{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT
    ( IsSymbolicJSON (..)
    , IsBits (..)
    , IsTokenPayload (..)
    , TokenBits
    , TokenHeader (..)
    , SigningAlgorithm (..)
    , toAsciiBits
    , tokenBits
    ) where

import           Control.DeepSeq                    (NFData, force)
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Constraint                    (withDict)
import           Data.Kind                          (Type)
import           GHC.Generics                       (Generic)
import           GHC.TypeLits                       (Symbol)
import           Prelude                            (type (~), ($), (.))
import qualified Prelude                            as P

import           ZkFold.Algebra.Number
import           ZkFold.Data.HFunctor.Classes       (HEq, HNFData, HShow)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators   hiding (toBits)
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.JWT.Utils
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), (@+))

-- | Types than can be represented as a Symbolic JSON string
--
class IsSymbolicJSON a where
    type MaxLength a :: Natural

    toJsonBits :: a -> VarByteString (MaxLength a) (Context a)

-- | Types than can be serialised
--
class IsBits a where
    type BitCount a :: Natural

    toBits :: a -> VarByteString (BitCount a) (Context a)

-- | Signing algorithm for JWT (such as RS256)
--
class SigningAlgorithm (alg :: Symbol) where
    type SKey alg (ctx :: (Type -> Type) -> Type) :: Type
    type VKey alg (ctx :: (Type -> Type) -> Type) :: Type
    type Signature alg (ctx :: (Type -> Type) -> Type) :: Type
    type Hash alg (ctx :: (Type -> Type) -> Type) :: Type

-- | Types that can act as JWT Payload
--
class IsTokenPayload (alg :: Symbol) a where
    signPayload
        :: a
        -> SKey alg (Context a)
        -> (TokenHeader (Context a), Signature alg (Context a))

    verifyJWT
        :: TokenHeader (Context a)
        -> a
        -> Signature alg (Context a)
        -> VKey alg (Context a)
        -> (Bool (Context a), Hash alg (Context a))

-- | Json Web Token header with information about encryption algorithm and signature
--
data TokenHeader ctx
    = TokenHeader
        { hdAlg :: VarByteString 72 ctx
        -- ^ Signature or encryption algorithm
        , hdKid :: VarByteString 320 ctx
        -- ^ Key ID
        , hdTyp :: VarByteString 32 ctx
        -- ^ Type of token
        }
    deriving Generic

deriving instance HEq ctx => P.Eq (TokenHeader ctx)
deriving instance HShow ctx => P.Show (TokenHeader ctx)
deriving instance HNFData ctx => NFData (TokenHeader ctx)

deriving instance Symbolic ctx => SymbolicData (TokenHeader ctx)
deriving instance Symbolic ctx => SymbolicInput (TokenHeader ctx)

instance Symbolic ctx => FromJSON (TokenHeader ctx) where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Symbolic ctx => IsSymbolicJSON (TokenHeader ctx) where

    type MaxLength (TokenHeader ctx) = 648
    toJsonBits TokenHeader{..} = force $
                    (fromType @"{\"alg\":\"")   @+ hdAlg
        `VB.append` (fromType @"\",\"kid\":\"") @+ hdKid
        `VB.append` (fromType @"\",\"typ\":\"") @+ hdTyp
        `VB.append` (fromType @"\"}")

instance Symbolic ctx => IsBits (TokenHeader ctx) where
    type BitCount (TokenHeader ctx) = 864
    toBits = toAsciiBits

toAsciiBits
    :: forall a ctx
    .  IsSymbolicJSON a
    => Context a ~ ctx
    => KnownNat (MaxLength a)
    => Symbolic ctx
    => a -> VarByteString (ASCII (Next6 (MaxLength a))) ctx
toAsciiBits = withNext6 @(MaxLength a) $ withDict (mulMod @(MaxLength a)) $ base64ToAscii . padBytestring6 . toJsonBits

type TokenBits a = (IsBits a, KnownNat (872 + BitCount a))

tokenBits
    :: forall p ctx
    .  Symbolic ctx
    => Context p ~ ctx
    => TokenBits p
    => TokenHeader ctx
    -> p
    -> VarByteString (864 + 8 + BitCount p) ctx
tokenBits h p =  force $
       toBits h
    @+ (fromType @".")
    @+ toBits p
