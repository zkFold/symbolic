{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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

import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Constraint ((\\))
import Data.Constraint.Nat
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (Symbol)
import Prelude (($), (.))

import ZkFold.Algebra.Number
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.JWT.Utils
import ZkFold.Symbolic.Data.VarByteString (VarByteString (..), fromType, (@+))
import qualified ZkFold.Symbolic.Data.VarByteString as VB

-- | Types than can be represented as a Symbolic JSON string
class (KnownNat (MaxLength a), 1 <= MaxLength a) => IsSymbolicJSON a c where
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
  verifyJWT :: TokenHeader c -> a c -> Signature alg c -> VKey alg c -> (Bool c, Hash alg c)

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
  deriving (Generic, Generic1, SymbolicData)

instance Symbolic ctx => FromJSON (TokenHeader ctx) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Symbolic ctx => IsSymbolicJSON TokenHeader ctx where
  type MaxLength TokenHeader = 648
  toJsonBits TokenHeader {..} =
    fromType @"{\"alg\":\""
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
  => Symbolic ctx
  => a ctx -> VarByteString (ASCII (Next6 (MaxLength a))) ctx
toAsciiBits =
  base64ToAscii . padBytestring6 . toJsonBits
    \\ knownNext6 @(MaxLength a)
    \\ mulMod @(MaxLength a)
    \\ timesMonotone1 @1 @(Div (MaxLength a + 5) 6) @6
    \\ divMonotone1 @6 @(MaxLength a + 5) @6
    \\ plusMonotone1 @1 @(MaxLength a) @5

type TokenBits a c = (IsBits a c, KnownNat (872 + BitCount a))

tokenBits
  :: forall p ctx
   . Symbolic ctx
  => TokenBits p ctx
  => TokenHeader ctx
  -> p ctx
  -> VarByteString (864 + 8 + BitCount p) ctx
tokenBits h p =
  toBits h @+ (fromType @".") @+ toBits p
    \\ leTrans @1 @872 @(872 + BitCount p)
    \\ plusMonotone2 @872 @0 @(BitCount p)
    \\ zeroLe @(BitCount p)
