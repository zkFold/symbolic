{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT
    ( IsSymbolicJSON (..)
    , IsBits (..)
    , IsTokenPayload (..)
    , TokenHeader (..)
    , SigningAlgorithm (..)
    , toAsciiBits
    ) where

import           Control.DeepSeq                    (NFData, force)
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import qualified Data.Aeson                         as JSON
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Constraint                    (Dict (..), withDict, (:-) (..))
import           Data.Constraint.Nat                (Max, divNat, minusNat, plusNat, timesNat)
import           Data.Constraint.Unsafe             (unsafeAxiom, unsafeSNat)
import           Data.Kind                          (Type)
import           Data.Maybe                         (fromMaybe)
import           Data.Scientific                    (toBoundedInteger)
import qualified Data.Text                          as T
import           Generic.Random                     (genericArbitrary, uniform)
import           GHC.Generics                       (Generic, Par1 (..))
import           GHC.TypeLits                       (Symbol, withKnownNat)
import           Prelude                            (fmap, pure, type (~), ($), (.), (<$>))
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor          (hmap)
import qualified ZkFold.Base.Data.Vector            as V
import           ZkFold.Base.Data.Vector            ((!!))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString    (ByteString (..), concat, toWords)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.JWT.Utils
import           ZkFold.Symbolic.Data.UInt
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), wipeUnassigned, (@+))
import           ZkFold.Symbolic.MonadCircuit       (newAssigned)


class IsSymbolicJSON a where
    type MaxLength a :: Natural

    toJsonBits :: a -> VarByteString (MaxLength a) (Context a)

class IsBits a where
    type BitCount a :: Natural

    toBits :: a -> VarByteString (BitCount a) (Context a)

class SigningAlgorithm (alg :: Symbol) (ctx :: (Type -> Type) -> Type) where
    type SKey alg ctx :: Type
    type VKey alg ctx :: Type
    type Signature alg ctx :: Type

class IsTokenPayload (alg :: Symbol) a where
    signPayload
        :: a
        -> SKey alg (Context a)
        -> Signature alg (Context a)

    verifyJWT
        :: TokenHeader (Context a)
        -> a
        -> Signature alg (Context a)
        -> VKey alg (Context a)
        -> Bool (Context a)

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

deriving instance
    ( P.Eq (ctx (V.Vector 72))
    , P.Eq (ctx (V.Vector 320))
    , P.Eq (ctx (V.Vector 32))
    , P.Eq (ctx Par1)
    ) => P.Eq (TokenHeader ctx)
deriving instance
    ( P.Show (ctx (V.Vector 72))
    , P.Show (ctx (V.Vector 320))
    , P.Show (ctx (V.Vector 32))
    , P.Show (ctx Par1)
    ) => P.Show (TokenHeader ctx)
deriving instance
    ( NFData (ctx (V.Vector 72))
    , NFData (ctx (V.Vector 320))
    , NFData (ctx (V.Vector 32))
    , NFData (ctx Par1)
    ) => NFData (TokenHeader ctx)

deriving instance Symbolic ctx => SymbolicData (TokenHeader ctx)
deriving instance Symbolic ctx => SymbolicInput (TokenHeader ctx)

instance Symbolic ctx => FromJSON (TokenHeader ctx) where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance
    ( Symbolic ctx
    , Context (TokenHeader ctx) ~ ctx
    , NFData (VarByteString (MaxLength (TokenHeader ctx)) ctx)
    ) => IsSymbolicJSON (TokenHeader ctx) where

    type MaxLength (TokenHeader ctx) = 648
    toJsonBits TokenHeader{..} = force $
                    (fromType @"{\"alg\":\"")   @+ hdAlg
        `VB.append` (fromType @"\",\"kid\":\"") @+ hdKid
        `VB.append` (fromType @"\",\"typ\":\"") @+ hdTyp
        `VB.append` (fromType @"\"}")

instance
 ( Symbolic ctx
 , NFData (ctx (V.Vector 8))
 , NFData (ctx (V.Vector 648))
 , NFData (ctx (V.Vector 864))
 , NFData (ctx Par1)
 ) => IsBits (TokenHeader ctx) where
    type BitCount (TokenHeader ctx) = 864
    toBits = toAsciiBits

toAsciiBits
    :: forall a ctx
    .  IsSymbolicJSON a
    => Context a ~ ctx
    => KnownNat (MaxLength a)
    => Symbolic ctx
    => NFData (ctx (V.Vector 8))
    => NFData (ctx (V.Vector (ASCII (Next6 (MaxLength a)))))
    => a -> VarByteString (ASCII (Next6 (MaxLength a))) ctx
toAsciiBits = withNext6 @(MaxLength a) $ withDict (mulMod @(MaxLength a)) $ base64ToAscii . padBytestring6 . toJsonBits
