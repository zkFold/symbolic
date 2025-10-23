{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT (
  IsSymbolicJSON (..),
  IsBits (..),
  IsTokenPayload (..),
  TokenBits,
  TokenHeader (..),
  SigningAlgorithm (..),
  toAsciiBits,
  tokenBits,
) where

import Control.DeepSeq (NFData, force)
import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Constraint (withDict)
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (Symbol)
import Prelude (($), (.))
import qualified Prelude as P

import ZkFold.Algebra.Number
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Combinators hiding (toBits)
import ZkFold.Symbolic.Data.JWT.Utils
import ZkFold.Symbolic.Data.VarByteString (VarByteString (..), (@+))
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import ZkFold.Symbolic.Data.V2 (SymbolicData)
import ZkFold.Symbolic.V2 (Symbolic)
import ZkFold.Symbolic.Compat (CompatData)

-- | Types than can be represented as a Symbolic JSON string
class IsSymbolicJSON a c where
  type MaxLength a :: Natural
  toJsonBits :: a c -> VarByteString (MaxLength a) c

-- | Types than can be serialised
class IsBits a c where
  type BitCount a :: Natural
  toBits :: a c -> VarByteString (BitCount a) c

-- | Signing algorithm for JWT (such as RS256)
class SigningAlgorithm (alg :: Symbol) where
  type SKey alg (ctx :: Type) :: Type
  type VKey alg (ctx :: Type) :: Type
  type Signature alg (ctx :: Type) :: Type
  type Hash alg (ctx :: Type) :: Type

-- | Types that can act as JWT Payload
class IsTokenPayload (alg :: Symbol) a c where
  signPayload :: a c -> SKey alg c -> (TokenHeader c, Signature alg c)
  verifyJWT :: TokenHeader c -> a c -> Signature alg c -> VKey alg c -> (CompatData Bool c, Hash alg c)

-- | Json Web Token header with information about encryption algorithm and signature
data TokenHeader ctx
  = TokenHeader
  { hdAlg :: VarByteString 72 ctx
  -- ^ Signature or encryption algorithm
  , hdKid :: VarByteString 320 ctx
  -- ^ Key ID
  , hdTyp :: VarByteString 32 ctx
  -- ^ Type of token
  }
  deriving (Generic, Generic1, SymbolicData, P.Eq, P.Show, NFData)

instance Symbolic ctx => FromJSON (TokenHeader ctx) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Symbolic ctx => IsSymbolicJSON TokenHeader ctx where
  type MaxLength TokenHeader = 648
  toJsonBits TokenHeader {..} =
      force $
        (fromType @"{\"alg\":\"")
          @+ hdAlg
          `VB.append` (fromType @"\",\"kid\":\"")
          @+ hdKid
          `VB.append` (fromType @"\",\"typ\":\"")
          @+ hdTyp
          `VB.append` (fromType @"\"}")

instance Symbolic ctx => IsBits TokenHeader ctx where
  type BitCount TokenHeader = 864
  toBits = toAsciiBits

toAsciiBits
  :: forall a ctx
   . IsSymbolicJSON a ctx
  => KnownNat (MaxLength a)
  => Symbolic ctx
  => a ctx -> VarByteString (ASCII (Next6 (MaxLength a))) ctx
toAsciiBits =
  withNext6 @(MaxLength a) $ withDict (mulMod @(MaxLength a)) $
    base64ToAscii . padBytestring6 . toJsonBits

type TokenBits a c = (IsBits a c, KnownNat (872 + BitCount a))

tokenBits
  :: forall p ctx
   . Symbolic ctx
  => TokenBits p ctx
  => TokenHeader ctx
  -> p ctx
  -> VarByteString (864 + 8 + BitCount p) ctx
tokenBits h p = force $ toBits h @+ (fromType @".") @+ toBits p
