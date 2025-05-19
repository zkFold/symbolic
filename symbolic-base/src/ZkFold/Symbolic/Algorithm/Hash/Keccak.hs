{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Remove these options.

-- TODO: Review language extensions.
module ZkFold.Symbolic.Algorithm.Hash.Keccak
    ( 
        padding,
    --   AlgorithmSetup (..)
    -- , Keccak
    -- , keccak
    -- , keccakVar
    -- , PaddedLength
    ) where

import           Control.DeepSeq                               (force)
import           Control.Monad                                 (forM_)
import           Control.Monad.ST                              (ST, runST)
import           Data.Bits                                     (shiftL, Bits ((.|.)))
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Constraint.Unsafe
import           Data.Kind                                     (Type)
import qualified Data.STRef                                    as ST
import           Data.Type.Bool                                (If)
import           ZkFold.Symbolic.Data.Bool                     (Bool (..), BoolType (..))
import           Data.Type.Equality                            (type (~))
import qualified Data.Vector                                   as V
import qualified Data.Vector.Mutable                           as VM
import           GHC.Generics                                  (Par1 (..))
import           GHC.TypeLits                                  (Symbol)
import           GHC.TypeNats                                  (type (<=?), withKnownNat)
import           Data.Function                                 ((&))
import           Prelude                                       (Int, id, pure, zip, ($!), ($), (.), (>>=), undefined)
import qualified Prelude                                       as P

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Data.HFunctor                          (hmap)
import           ZkFold.Data.Vector                            (Vector (..), fromVector, reverse, unsafeToVector)
-- import           ZkFold.Symbolic.Algorithm.Hash.Keccak.Constants (keccakRoundConstants, keccakRotationOffsets)
import           ZkFold.Symbolic.Class                         (BaseField, Symbolic, fromCircuitF)
import           ZkFold.Symbolic.Data.Bool                     (Bool (..), BoolType (..))
import           ZkFold.Symbolic.Data.ByteString               (ByteString (..), ShiftBits (..), concat, set, toWords,
                                                                truncate)
import           ZkFold.Symbolic.Data.Combinators              (Iso (..), RegisterSize (..), Resize (..), expansionW,
                                                                ilog2)
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FieldElement             (FieldElement (..))
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt                     (UInt)
import qualified ZkFold.Symbolic.Data.VarByteString            as VB
import           ZkFold.Symbolic.Data.VarByteString            (VarByteString (..))
import           ZkFold.Symbolic.MonadCircuit                  (newAssigned)
import Data.Word (Word8)
import qualified Data.ByteString as B

-- | Standard Keccak and SHA3 hashes have width of sponge state to be 1600 bits.
type Width :: Natural
type Width = 1600

-- | Length of the hash result for a given rate.
type ResultSize rate = Div (Width - rate) 16

-- TODO: Not have it at type level if it's not required.
-- | Capacity of the sponge construction for a given rate.
type Capacity rate = Width - rate



-- | Keccak is a family of hashing functions with almost identical implementations but different parameters.
-- This class links these varying parts with the appropriate algorithm.
class
    ( KnownNat (Rate algorithm)
    , Mod (Rate algorithm) 8 ~ 0
    -- This constraint is needed so that dividing by 8 does not lead to zero.
    , (1 <=? Div (Rate algorithm) 8) ~ 'P.True
    ) => AlgorithmSetup (algorithm :: Symbol) (context :: (Type -> Type) -> Type) where
    type Rate algorithm :: Natural
    -- ^ The rate of the sponge construction, in bits.

    padByte :: Word8
    -- ^ Delimited suffix for the padding.

instance AlgorithmSetup "Keccak256" c where
    type Rate "Keccak256" = 1088
    padByte = 0x01

instance AlgorithmSetup "SHA3-256" c where
    type Rate "SHA3-256" = 1088
    padByte = 0x06

-- | Constraints required for a type-safe Keccak
type Keccak algorithm context k = (AlgorithmSetup algorithm context, KnownNat k, 
  Mod k 8 ~ 0, 
  KnownNat (PaddedLengthBytesFromBits k (Rate algorithm)),
  KnownNat (PaddedLengthBits k (Rate algorithm)),
  Symbolic context)

keccak
    :: forall (algorithm :: Symbol) context k
    .  Keccak algorithm context k
    => ByteString k context -> ByteString (ResultSize (Rate algorithm)) context
keccak =
    let paddedMessage = undefined
    in undefined
    
-- | Number of bytes from a given number of bits assuming that the number of bits is a multiple of 8.
type BytesFromBits bits = Div bits 8

-- | Length of the padded message in bits.
type PaddedLengthBits msgBits rateBits = (PaddedLengthBytesFromBits msgBits rateBits) * 8

-- | Length of the padded message in bytes.
type PaddedLengthBytes msgBytes rateBytes = (msgBytes + (rateBytes - (Mod msgBytes rateBytes)))

-- | Length of the padded message in bytes from bits.
type PaddedLengthBytesFromBits msgBits rateBits = (PaddedLengthBytes (BytesFromBits msgBits) (BytesFromBits rateBits))


-- | Additional bytes for padding.
padLenBytes :: Natural -> Natural -> Natural
padLenBytes msgBytes rateBytes =
    P.fromIntegral @P.Integer @Natural (P.fromIntegral rateBytes - (P.fromIntegral (msgBytes `mod` rateBytes)))

-- | Length of the padded message in bytes.
paddedLenBytes :: Natural -> Natural -> Natural
paddedLenBytes msgBytes rateBytes =
    msgBytes + padLenBytes msgBytes rateBytes

-- | Length of the padded message in bits.
paddedLenBits :: Natural -> Natural -> Natural
paddedLenBits msgBits rateBits = (paddedLenBytes (div msgBits 8) (div rateBits 8)) * 8

padding
    :: forall (algorithm :: Symbol) context k
    .  Keccak algorithm context k
    => ByteString k context -> ByteString (PaddedLengthBits k (Rate algorithm)) context
padding msg =
    let msgBytes = value @k `div` 8
        rateBytes = value @(Rate algorithm) `div` 8
        padLengthBytes = padLenBytes msgBytes rateBytes
        -- @fromIntegral@ below is safe as we have hard-coded instances for rate bytes.
        padByteString = 
            let emptyBS = B.replicate (P.fromIntegral padLengthBytes) 0
                bsAfterPadByte = (B.head emptyBS .|. (padByte @algorithm @context)) `B.cons` B.tail emptyBS
            in B.init bsAfterPadByte `B.append` (B.singleton (B.last bsAfterPadByte .|. 0x80))
    in (resize msg `shiftBitsL` (padLengthBytes * 8)) || (fromConstant padByteString)
    
