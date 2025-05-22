{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

-- TODO: Remove these options.
-- TODO: Format by fourmolu.

-- TODO: Review language extensions.
module ZkFold.Symbolic.Algorithm.Hash.Keccak (
  ResultSizeInBytes,
  ResultSizeInBits,
  AlgorithmSetup (..),
  Keccak,
  padding,
  toBlocks,
  keccak,
  -- TODO: Mention exports that are mainly for testing.
  absorbBlock,
  emptyState,
  --   AlgorithmSetup (..)
  -- , Keccak
  -- , keccak
  -- , keccakVar
  -- , PaddedLength
) where

import           Control.Monad.ST                                (ST)
import           Data.Bits                                       (Bits ((.|.)))
import qualified Data.ByteString                                 as B
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Data                                       (Proxy (..))
import           Data.Function                                   (flip, (&))
import           Data.Kind                                       (Type)
import           Data.Semialign                                  (Zip (..))
import           Data.Type.Equality                              (type (~))
import qualified Data.Vector                                     as V
import qualified Data.Vector.Mutable                             as VM
import           Data.Word                                       (Word8)
import           GHC.TypeLits                                    (SomeNat (..), Symbol)
import           GHC.TypeNats                                    (someNatVal, type (<=?))
import           Prelude                                         (($), (.), (<$>))
import qualified Prelude                                         as P

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Data.HFunctor                            (hmap)
import           ZkFold.Data.Vector                              (Vector (..), backpermute, chunks, concatMap, generate,
                                                                  head, indexed, mapWithIx, reverse, slice, unfold,
                                                                  (!!))
import           ZkFold.Symbolic.Algorithm.Hash.Keccak.Constants
import           ZkFold.Symbolic.Class                           (Symbolic)
import           ZkFold.Symbolic.Data.Bool                       (BoolType (..))
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Ord

-- TODO: Is this Width / LaneWidth?

-- NOTE: Code is NOT parameterized over `LaneWidth` at all places, so changing this value could break the code.

-- | Width of each lane (in bits) in the Keccak sponge state.
type LaneWidth :: Natural
type LaneWidth = 64

-- | Standard Keccak and SHA3 hashes have width of sponge state to be 1600 bits.
type Width :: Natural
type Width = 1600

-- | Length of the hash result (in bytes) for a given rate.
type ResultSizeInBytes rateBits = Div (Capacity rateBits) 16

-- | Length of the hash result (in bits) for a given rate.
type ResultSizeInBits rateBits = ResultSizeInBytes rateBits * 8

-- | Capacity of the sponge construction for a given rate.
type Capacity rate = Width - rate

numRounds :: Natural
numRounds = value @NumRounds

{- | Keccak is a family of hashing functions with almost identical implementations but different parameters.
This class links these varying parts with the appropriate algorithm.
-}
class
  ( KnownNat (Rate algorithm)
  , Mod (Rate algorithm) 8 ~ 0
  , -- Requiring rate to be a multiple of 64. As we would eventually obtain 64 bit words.
    Mod (Div (Rate algorithm) 8) 8 ~ 0
  , -- This constraint is needed so that dividing by 8 does not lead to zero. Given the above constraints, this essentially means that `Rate algorithm` is not zero.
    (1 <=? Div (Rate algorithm) 8) ~ 'P.True
  , KnownNat (Div (Rate algorithm) LaneWidth)
  , KnownNat (Div (Capacity (Rate algorithm)) 16 * 8)
  , (ResultSizeInBits (Rate algorithm) <=? SqueezeLanesToExtract algorithm * 64) ~ 'P.True
  , KnownNat (Div ((Div (Capacity (Rate algorithm)) 16 + 8) - 1) 8)
  ) =>
  AlgorithmSetup (algorithm :: Symbol) (context :: (Type -> Type) -> Type)
  where
  type Rate algorithm :: Natural
  -- ^ The rate of the sponge construction, in bits.

  padByte :: Word8
  -- ^ Delimited suffix for the padding.

instance AlgorithmSetup "Keccak256" c where
  type Rate "Keccak256" = 1088
  padByte = 0x01

instance AlgorithmSetup "SHA3-512" c where
  type Rate "SHA3-512" = 576
  padByte = 0x06

instance AlgorithmSetup "SHA3-384" c where
  type Rate "SHA3-384" = 832
  padByte = 0x06

instance AlgorithmSetup "SHA3-256" c where
  type Rate "SHA3-256" = 1088
  padByte = 0x06

instance AlgorithmSetup "SHA3-224" c where
  type Rate "SHA3-224" = 1152
  padByte = 0x06

-- | Constraints required for a type-safe Keccak
type Keccak algorithm context k =
  ( AlgorithmSetup algorithm context
  , KnownNat k
  , Mod k 8 ~ 0
  , ((Div (PaddedLengthBytesFromBits k (Rate algorithm)) 8) * 64) ~ (PaddedLengthBits k (Rate algorithm))
  , -- This constraint is actually true as `NumBlocks` is a number which is a multiple of `Rate` by 64 and since `LaneWidth` is 64, it get's cancelled out and what we have is something which is a multiple of `Rate` by `Rate` which is certainly integral.
    ((Div (NumBlocks k (Rate algorithm)) (Div (Rate algorithm) LaneWidth)) * Div (Rate algorithm) LaneWidth) ~ NumBlocks k (Rate algorithm)
  , Symbolic context
  )

-- TODO: Needed to say algorithm :: Symbol?
keccak ::
  forall (algorithm :: Symbol) context k.
  Keccak algorithm context k =>
  ByteString k context -> ByteString (ResultSizeInBits (Rate algorithm)) context
keccak bs =
  padding @algorithm @context @k bs
    & toBlocks @algorithm @context @k
    & absorbBlock @algorithm @context @k emptyState
    & squeeze @algorithm @context

-- | Number of bytes from a given number of bits assuming that the number of bits is a multiple of 8.
type BytesFromBits bits = Div bits 8

-- | Length of the padded message in bits.
type PaddedLengthBits msgBits rateBits = (PaddedLengthBytesFromBits msgBits rateBits) * 8

-- | Length of the padded message in bytes.
type PaddedLengthBytes msgBytes rateBytes = (msgBytes + (rateBytes - (Mod msgBytes rateBytes)))

-- | Length of the padded message in bytes from bits.
type PaddedLengthBytesFromBits msgBits rateBits = (PaddedLengthBytes (BytesFromBits msgBits) (BytesFromBits rateBits))

type NumBlocks msgBits rateBits = Div (PaddedLengthBytesFromBits msgBits rateBits) 8

-- | Additional bytes for padding.
padLenBytes :: Natural -> Natural -> Natural
padLenBytes msgBytes rateBytes =
  P.fromIntegral @P.Integer @Natural (P.fromIntegral rateBytes - (P.fromIntegral (msgBytes `mod` rateBytes)))

padding ::
  forall (algorithm :: Symbol) context k.
  Keccak algorithm context k =>
  ByteString k context -> ByteString (PaddedLengthBits k (Rate algorithm)) context
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

toBlocks ::
  forall (algorithm :: Symbol) context k.
  Keccak algorithm context k =>
  ByteString (PaddedLengthBits k (Rate algorithm)) context -> Vector (NumBlocks k (Rate algorithm)) (ByteString 64 context)
toBlocks msg =
  let byteWords = (toWords msg :: Vector (PaddedLengthBytesFromBits k (Rate algorithm)) (ByteString 8 context))
      byteWordsBitReversed = reverseBits <$> byteWords
   in reverseBits <$> (toWords $ concat byteWordsBitReversed)

-- TODO: Instead of `ByteString 64 context`, shall I be utilizing something like UInt 64? It could have an advantage in better communicating my bit ordering in words (which is most significant bit first).

type AbsorbChunkSize algorithm = Div (Rate algorithm) LaneWidth

absorbBlock ::
  forall (algorithm :: Symbol) context k.
  Keccak algorithm context k =>
  Vector NumLanes (ByteString 64 context) -> Vector (NumBlocks k (Rate algorithm)) (ByteString 64 context) -> Vector NumLanes (ByteString 64 context)
absorbBlock state blocks =
  let blockChunks :: Vector (Div (NumBlocks k (Rate algorithm)) (AbsorbChunkSize algorithm)) (Vector (AbsorbChunkSize algorithm) (ByteString 64 context)) = chunks blocks
   in P.foldl -- TODO: Use foldl'?
        ( \accState chunk ->
            -- TODO: Perhaps this can be optimized.
            let state' =
                  mapWithIx
                    ( \z el ->
                        if div z 5 + 5 * mod z 5 < threshold
                          then el `xor` (chunk !! (div z 5 + 5 * mod z 5))
                          else el
                    )
                    accState
             in keccakF @context state'
        )
        state
        blockChunks
 where
  rate = value @(Rate algorithm)
  laneWidth = value @LaneWidth
  threshold = div rate laneWidth

-- TODO: Are accumulators required to be made strict?
-- TODO: Should some functions be asked to be made inlined?
keccakF :: forall context. (Symbolic context) => Vector NumLanes (ByteString 64 context) -> Vector NumLanes (ByteString 64 context)
keccakF state =
  P.snd $ P.foldl1 (.) (P.replicate (P.fromIntegral numRounds) f) (0, state)
 where
  f (r, s) = (P.succ r, iota @context r . chi @context . pi @context . rho @context . theta @context $ s)

theta :: forall context. Symbolic context => Vector NumLanes (ByteString 64 context) -> Vector NumLanes (ByteString 64 context)
theta state =
  concatMap @5 @5
    ( \(i, e) ->
        P.fmap
          (xor e)
          ( case someNatVal (i) of
              SomeNat (_ :: Proxy i) ->
                withDict (timesNat @i @5) $
                  slice @(i * 5) @5 state
          )
    )
    $ indexed d
 where
  c =
    -- TODO: Modify generate so that inner function knows that i >= 0 & < given size.
    generate @5
      ( \i ->
          P.foldl1
            xor
            ( case someNatVal (i) of
                SomeNat (_ :: Proxy i) ->
                  withDict (timesNat @i @5) $
                    slice @(i * 5) @5 @NumLanes state
            )
      )
  d = generate @5 (\i -> c !! (P.fromIntegral (((P.fromIntegral i :: P.Integer) - 1) `mod` 5)) `xor` rotateBitsL (c !! ((i + 1) `mod` 5)) 1)

rho :: forall context. Symbolic context => Vector NumLanes (ByteString 64 context) -> Vector NumLanes (ByteString 64 context)
rho state = zipWith (flip rotateBitsL) rotationConstants state

pi :: forall context. Vector NumLanes (ByteString 64 context) -> Vector NumLanes (ByteString 64 context)
pi state = backpermute state piConstants

chi :: forall context. Symbolic context => Vector NumLanes (ByteString 64 context) -> Vector NumLanes (ByteString 64 context)
chi b = mapWithIx subChi b
 where
  subChi z el = el `xor` (not (b !! mod (z + 5) 25) && (b !! mod (z + 10) 25))

iota :: forall context. Symbolic context => Natural -> Vector NumLanes (ByteString 64 context) -> Vector NumLanes (ByteString 64 context)
iota roundNumber state = modify (\v -> VM.write v 0 $ xor (roundConstants !! roundNumber) (head state)) state

type CeilDiv n d = Div (n + d - 1) d

type SqueezeLanesToExtract algorithm = CeilDiv (ResultSizeInBytes (Rate algorithm)) (Div (LaneWidth) 8)

squeeze ::
  forall algorithm context.
  ( AlgorithmSetup algorithm context
  ) =>
  Symbolic context =>
  Vector NumLanes (ByteString 64 context) -> (ByteString (ResultSizeInBits (Rate algorithm)) context)
squeeze state =
  truncate
    . concat
    . P.fmap (reverseEndianness @64)
    $ stateToBytes state
 where
  stateToBytes s = unfold @(SqueezeLanesToExtract algorithm) extract (0, s)
  threshold = div rate laneWidth
  extract (x, s)
    | x < threshold = (s !! (div x 5 + mod x 5 * 5), (P.succ x, s))
    | P.otherwise = extract (0, keccakF @context s)
  rate = value @(Rate algorithm)
  laneWidth = value @LaneWidth

modify :: (forall s. V.MVector s a -> ST s ()) -> Vector n a -> Vector n a
modify f (Vector v) = Vector $ V.modify f v

reverseBits :: forall n context. Symbolic context => ByteString n context -> ByteString n context
reverseBits (ByteString cbs) = ByteString $ (hmap reverse cbs)
