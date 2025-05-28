{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeOperators       #-}

-- Code is largely based on [`keccak`](https://github.com/aupiff/keccak) Haskell library.

module ZkFold.Symbolic.Algorithm.Hash.Keccak (
  ResultSizeInBytes,
  ResultSizeInBits,
  AlgorithmSetup (..),
  Keccak,
  keccak,
  keccakVar,

  -- * Mainly for testing.
  padding,
  paddingVar,
  toBlocks,
) where

import           Control.Monad.ST                                (ST)
import           Data.Bits                                       (Bits ((.|.)))
import qualified Data.ByteString                                 as B
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Constraint.Unsafe
import           Data.Data                                       (Proxy (..))
#if __GLASGOW_HASKELL__ < 910
import qualified Data.Foldable                                   as P (foldl')
#endif
import           Data.Function                                   (flip, (&))
import           Data.Functor.Rep                                (Representable (..))
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
import           ZkFold.Algebra.Field                            (fromZp)
import           ZkFold.Algebra.Number
import           ZkFold.Data.HFunctor                            (hmap)
import           ZkFold.Data.Vector                              (Vector (..), backpermute, chunks, concatMap,
                                                                  enumerate, head, mapWithIx, reverse, slice, unfold,
                                                                  (!!))
import           ZkFold.Symbolic.Algorithm.Hash.Keccak.Constants
import           ZkFold.Symbolic.Class                           (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool                       (BoolType (..))
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators                (Ceil, GetRegisterSize, Iso (..), NumberOfRegisters,
                                                                  RegisterSize (..))
import           ZkFold.Symbolic.Data.Conditional                (ifThenElse)
import           ZkFold.Symbolic.Data.FieldElement               (FieldElement)
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt                       (OrdWord, UInt)
import qualified ZkFold.Symbolic.Data.VarByteString              as VB
import           ZkFold.Symbolic.Data.VarByteString              (VarByteString (..))

{- | Width of each lane (in bits) in the Keccak sponge state.

This is state width by number of lanes. For computation efficiency, this number is usually a power of 2 and is 64 for our case.
-}
type LaneWidth :: Natural
type LaneWidth = Div Width NumLanes

-- | Standard Keccak and SHA3 hashes have width of sponge state to be 1600 bits.
type Width :: Natural
type Width = 1600

-- | Length of the hash result (in bytes) for a given rate.
type ResultSizeInBytes rateBits = Div (Capacity rateBits) 16

-- | Length of the hash result (in bits) for a given rate.
type ResultSizeInBits rateBits = ResultSizeInBytes rateBits * 8

-- | Capacity of the sponge construction for a given rate.
type Capacity rate = Width - rate

-- | See 'NumRounds' for more details.
numRounds :: Natural
numRounds = value @NumRounds

{- | Keccak is a family of hashing functions with almost identical implementations but different parameters.
This class links these varying parts with the appropriate algorithm.
-}
class
  ( KnownNat (Rate algorithm)
  , -- Padded message is a multiple of rate (in bits), and thus we want rate to be a multiple of 8 to obtain valid "byte"strings.
    Mod (Rate algorithm) 8 ~ 0
  , -- Requiring rate to be a multiple of 'LaneWidth'. As we would eventually obtain 'LaneWidth' bit words.
    Mod (Rate algorithm) LaneWidth ~ 0
  , -- This constraint is needed so that dividing by 8 does not lead to zero. Given the above constraints, it is equivalent to saying that rate is positive.
    (1 <=? Div (Rate algorithm) 8) ~ 'P.True
  , -- Redundant constraint but GHC is unable to derive it as such.
    (1 <=? Div (Rate algorithm) LaneWidth) ~ 'P.True
    -- Some of the code actually assumes that lane width is 64. This constraint is just a safety valve to let code break if developer tries changing value of @LaneWidth@.
  , LaneWidth ~ 64
  , KnownNat (Div (Rate algorithm) LaneWidth)
  , KnownNat (Div (Capacity (Rate algorithm)) 16 * 8)
  , (ResultSizeInBits (Rate algorithm) <=? SqueezeLanesToExtract algorithm * 64) ~ 'P.True
  , KnownNat (Div ((Div (Capacity (Rate algorithm)) 16 + 8) - 1) 8)
  ) =>
  AlgorithmSetup (algorithm :: Symbol)
  where
  type Rate algorithm :: Natural
  -- ^ The rate of the sponge construction, in bits.

  padByte :: Word8
  -- ^ Delimited suffix for the padding.

instance AlgorithmSetup "Keccak256" where
  type Rate "Keccak256" = 1088
  padByte = 0x01

instance AlgorithmSetup "SHA3-512" where
  type Rate "SHA3-512" = 576
  padByte = 0x06

instance AlgorithmSetup "SHA3-384" where
  type Rate "SHA3-384" = 832
  padByte = 0x06

instance AlgorithmSetup "SHA3-256" where
  type Rate "SHA3-256" = 1088
  padByte = 0x06

instance AlgorithmSetup "SHA3-224" where
  type Rate "SHA3-224" = 1152
  padByte = 0x06

-- | Constraints required for a type-safe Keccak
type Keccak algorithm context k =
  ( AlgorithmSetup algorithm
  , KnownNat k
  , -- So that we are dealing with "byte"strings.
    Mod k 8 ~ 0
  , Symbolic context
  , KnownNat
      ( NumberOfRegisters
          (BaseField context)
          (NumberOfBits (BaseField context))
          Auto
      )
  , KnownNat
      ( Ceil
          ( GetRegisterSize
              (BaseField context)
              (NumberOfBits (BaseField context))
              Auto
          )
          OrdWord
      )
  )

-- | Main function to obtain the hash of a given message.
keccak ::
  forall algorithm context k.
  Keccak algorithm context k =>
  ByteString k context -> ByteString (ResultSizeInBits (Rate algorithm)) context
keccak bs =
  padding @algorithm @context @k bs
    & toBlocks @algorithm @context @k
    & absorbBlocks @algorithm @context @k
    & squeeze @algorithm @context

{- | Like 'keccak' but for 'VarByteString'.

__NOTE__: It is assumed that length of 'ByteString' (in bits) inside 'VarByteString' is multiple of 8 (so that we have valid "byte" string).

__WARNING__: This function is not yet tested. See https://github.com/zkFold/symbolic/issues/598 & https://github.com/zkFold/symbolic/issues/597.
-}
keccakVar ::
  forall algorithm context k.
  Keccak algorithm context k =>
  VarByteString k context -> ByteString (ResultSizeInBits (Rate algorithm)) context
keccakVar msg =
  let VarByteString {..} = paddingVar @algorithm @context @k msg
   in toBlocks @algorithm @context @k bsBuffer
        & absorbBlocksVar @algorithm @context @k bsLength
        & squeeze @algorithm @context

-- | Number of bytes from a given number of bits assuming that the number of bits is a multiple of 8.
type BytesFromBits bits = Div bits 8

-- | Length of the padded message in bits.
type PaddedLengthBits msgBits rateBits = (PaddedLengthBytesFromBits msgBits rateBits) * 8

-- | Length of the padded message in bytes.
type PaddedLengthBytes msgBytes rateBytes = (msgBytes + (rateBytes - Mod msgBytes rateBytes))

-- | Length of the padded message in bytes from bits.
type PaddedLengthBytesFromBits msgBits rateBits = (PaddedLengthBytes (BytesFromBits msgBits) (BytesFromBits rateBits))

type NumBlocks msgBits rateBits = Div (PaddedLengthBytesFromBits msgBits rateBits) 8

withMessageLengthConstraints ::
  forall msgBits rateBits {r} {msgBytes} {rateBytes}.
  ( KnownNat msgBits
  , KnownNat rateBits
  , 1 <= Div rateBits 8
  , 1 <= Div rateBits LaneWidth
  , msgBytes ~ Div msgBits 8
  , rateBytes ~ Div rateBits 8
  ) =>
  ( ( KnownNat (PaddedLengthBytesFromBits msgBits rateBits)
    , KnownNat (PaddedLengthBits msgBits rateBits)
    , KnownNat
        ( Div
            (NumBlocks msgBits rateBits)
            (AbsorbChunkSize' rateBits)
        )
    , (Div (PaddedLengthBytesFromBits msgBits rateBits) 8) * 64 ~ PaddedLengthBits msgBits rateBits
    , (Div (NumBlocks msgBits rateBits) (Div rateBits LaneWidth)) * Div rateBits LaneWidth ~ NumBlocks msgBits rateBits
    ) =>
    r
  ) ->
  r
withMessageLengthConstraints =
  withDict (divNat @msgBits @8) $
    withDict (divNat @rateBits @8) $
      withDict (modNat @msgBytes @rateBytes) $
        withDict (modBound @msgBytes @rateBytes) $
          withDict (minusNat @rateBytes @(Mod msgBytes rateBytes)) $
            withDict (plusNat @msgBytes @(rateBytes - Mod msgBytes rateBytes)) $
              withDict (timesNat @(PaddedLengthBytesFromBits msgBits rateBits) @8) $
                withDict (divNat @(PaddedLengthBytesFromBits msgBits rateBits) @8) $
                  withDict (divNat @rateBits @LaneWidth) $
                    withDict (divNat @(NumBlocks msgBits rateBits) @(AbsorbChunkSize' rateBits)) $
                      withDict (withMessageLengthConstraints' @msgBits @rateBits)

withMessageLengthConstraints' ::
  forall msgBits rateBits.
  (KnownNat msgBits, KnownNat rateBits)
    :- ( -- Note that this constraint is true as @rateBits@ is a multiple of 64 and thus padded message is also a multiple of 64.
         (Div (PaddedLengthBytesFromBits msgBits rateBits) 8) * 64 ~ PaddedLengthBits msgBits rateBits
       , -- This constraint is actually true as `NumBlocks` is a number which is a multiple of `Rate` by 64 and since `LaneWidth` is 64, it get's cancelled out and what we have is something which is a multiple of `Rate` by `Rate` which is certainly integral.
         (Div (NumBlocks msgBits rateBits) (Div rateBits LaneWidth)) * Div rateBits LaneWidth ~ NumBlocks msgBits rateBits
       )
withMessageLengthConstraints' =
  Sub
    $ withDict
      (unsafeAxiom @((Div (PaddedLengthBytesFromBits msgBits rateBits) 8) * 64 ~ PaddedLengthBits msgBits rateBits))
    $ withDict
      (unsafeAxiom @((Div (NumBlocks msgBits rateBits) (Div rateBits LaneWidth)) * Div rateBits LaneWidth ~ NumBlocks msgBits rateBits))
      Dict

padding ::
  forall algorithm context k.
  Keccak algorithm context k =>
  ByteString k context -> ByteString (PaddedLengthBits k (Rate algorithm)) context
padding msg =
  withMessageLengthConstraints @k @(Rate algorithm) $
    let msgBytes = value @k `div` 8
        rateBytes = value @(Rate algorithm) `div` 8
        padLengthBytes = P.fromIntegral @P.Integer @Natural (P.fromIntegral rateBytes - P.fromIntegral (msgBytes `mod` rateBytes))
        -- @fromIntegral@ below is safe as we have hard-coded instances for rate bytes.
        padByteString =
          let emptyBS = B.replicate (P.fromIntegral padLengthBytes) 0
              bsAfterPadByte = (B.head emptyBS .|. (padByte @algorithm)) `B.cons` B.tail emptyBS
           in B.init bsAfterPadByte `B.append` B.singleton (B.last bsAfterPadByte .|. 0x80)
     in (resize msg `shiftBitsL` (padLengthBytes * 8)) || fromConstant padByteString

paddingVar ::
  forall algorithm context k.
  Keccak algorithm context k =>
  VarByteString k context -> VarByteString (PaddedLengthBits k (Rate algorithm)) context
paddingVar VarByteString {..} =
  withMessageLengthConstraints @k @(Rate algorithm) $
    let
      rateN = value @(Rate algorithm)
      bsLengthModRate = from @_ @(UInt (NumberOfBits (BaseField context)) Auto context) bsLength `mod` fromConstant rateN
      padLengthBits = fromConstant rateN - from bsLengthModRate
      paddedLengthBits = bsLength + padLengthBits
      padBS =
        let bs1 = fromConstant (0x80 :: Natural)
            bs2 = fromConstant (P.fromIntegral (padByte @algorithm) :: Natural)
         in (bs2 `VB.shiftL` (padLengthBits - fromConstant (8 :: Natural))) || bs1
      grown :: ByteString (PaddedLengthBits k (Rate algorithm)) context
      grown =
        let resized = resize bsBuffer
         in resized
              & (`VB.shiftL` padLengthBits)
     in
      VarByteString paddedLengthBits (grown || padBS)

toBlocks ::
  forall (algorithm :: Symbol) context k.
  Keccak algorithm context k =>
  ByteString (PaddedLengthBits k (Rate algorithm)) context -> Vector (NumBlocks k (Rate algorithm)) (ByteString LaneWidth context)
toBlocks msg =
  withMessageLengthConstraints @k @(Rate algorithm) $
    let byteWords = (toWords msg :: Vector (PaddedLengthBytesFromBits k (Rate algorithm)) (ByteString 8 context))
        byteWordsBitReversed = reverseBits <$> byteWords
     in reverseBits <$> toWords (concat byteWordsBitReversed)

type AbsorbChunkSize algorithm = AbsorbChunkSize' (Rate algorithm)

type AbsorbChunkSize' rateBits = Div rateBits LaneWidth

absorbBlocks ::
  forall (algorithm :: Symbol) context k.
  Keccak algorithm context k =>
  Vector (NumBlocks k (Rate algorithm)) (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
absorbBlocks blocks =
  withMessageLengthConstraints @k @(Rate algorithm) $
    let blockChunks :: Vector (Div (NumBlocks k (Rate algorithm)) (AbsorbChunkSize algorithm)) (Vector (AbsorbChunkSize algorithm) (ByteString LaneWidth context)) = chunks blocks
     in P.foldl'
          ( \accState chunk ->
              keccakF @context (updateStateInAbsorption @algorithm @context chunk threshold accState)
          )
          emptyState
          blockChunks
 where
  rate = value @(Rate algorithm)
  laneWidth = value @LaneWidth
  threshold = div rate laneWidth

{-# INLINE updateStateInAbsorption #-}
updateStateInAbsorption :: forall algorithm context. Symbolic context => Vector (AbsorbChunkSize algorithm) (ByteString LaneWidth context) -> Natural -> Vector NumLanes (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
updateStateInAbsorption chunk threshold =
  mapWithIx
    ( \z el ->
        if div z 5 + 5 * mod z 5 < threshold
          then el `xor` (chunk !! (div z 5 + 5 * mod z 5))
          else el
    )

absorbBlocksVar ::
  forall (algorithm :: Symbol) context k.
  Keccak algorithm context k =>
  -- | Actual length of the message (post padding) in bits.
  FieldElement context ->
  Vector (NumBlocks k (Rate algorithm)) (ByteString LaneWidth context) ->
  Vector NumLanes (ByteString LaneWidth context)
absorbBlocksVar paddedMsgLen blocks =
  withMessageLengthConstraints @k @(Rate algorithm) $
    let blockChunks :: Vector (Div (NumBlocks k (Rate algorithm)) (AbsorbChunkSize algorithm)) (Vector (AbsorbChunkSize algorithm) (ByteString LaneWidth context)) = chunks blocks
        actualChunksCount :: FieldElement context =
          let absorbChunkSize = fromConstant $ value @(AbsorbChunkSize algorithm)
              numBlocks = from @_ @(UInt (NumberOfBits (BaseField context)) Auto context) paddedMsgLen `div` fromConstant (value @LaneWidth)
           in from $ numBlocks `div` absorbChunkSize
        numChunksToDrop = fromConstant (value @(Div (NumBlocks k (Rate algorithm)) (AbsorbChunkSize algorithm))) - actualChunksCount
     in -- In this case, we need to drop first few chunks.
        P.foldl'
          ( \accState (fromZp -> ix, chunk) ->
              let state' = updateStateInAbsorption @algorithm @context chunk threshold accState
                  ixFE :: FieldElement context = fromConstant ix
               in ifThenElse
                    (ixFE < numChunksToDrop)
                    accState
                    (keccakF @context state')
          )
          emptyState
          (zip (tabulate P.id) blockChunks)
 where
  rate = value @(Rate algorithm)
  laneWidth = value @LaneWidth
  threshold = div rate laneWidth

-- TODO: Are accumulators required to be made strict? This should be checked for all recursive/folding functions. Issue to track this: https://github.com/zkFold/symbolic/issues/599.
keccakF ::
  forall context.
  Symbolic context =>
  Vector NumLanes (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
keccakF state =
  P.snd $ P.foldl1 (.) (P.replicate (P.fromIntegral numRounds) f) (0, state)
 where
  f (r, s) = (P.succ r, iota @context r . chi @context . pi @context . rho @context . theta @context $ s)

{-# INLINE theta #-}
theta ::
  forall context.
  Symbolic context =>
  Vector NumLanes (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
theta state =
  concatMap @5 @5
    ( \(i, e) ->
        P.fmap
          (xor e)
          ( case someNatVal i of
              SomeNat (_ :: Proxy i) ->
                withDict (timesNat @i @5) $
                  slice @(i * 5) @5 state
          )
    )
    $ enumerate d
 where
  c :: Vector 5 (ByteString LaneWidth context) =
    tabulate
      ( \i ->
          P.foldl1
            xor
            ( case someNatVal (fromZp i) of
                SomeNat (_ :: Proxy i) ->
                  withDict (timesNat @i @5) $
                    slice @(i * 5) @5 @NumLanes state
            )
      )
  d :: Vector 5 (ByteString LaneWidth context) = tabulate (\(fromZp -> i) -> c !! P.fromIntegral (((P.fromIntegral i :: P.Integer) - 1) `mod` 5) `xor` rotateBitsL (c !! ((i + 1) `mod` 5)) 1)

{-# INLINE rho #-}
rho ::
  forall context.
  Symbolic context =>
  Vector NumLanes (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
rho = zipWith (flip rotateBitsL) rotationConstants

{-# INLINE pi #-}
pi ::
  forall context.
  Vector NumLanes (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
pi state = backpermute state piConstants

{-# INLINE chi #-}
chi ::
  forall context.
  Symbolic context =>
  Vector NumLanes (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
chi b = mapWithIx subChi b
 where
  subChi z el = el `xor` (not (b !! mod (z + 5) 25) && (b !! mod (z + 10) 25))

{-# INLINE iota #-}
iota ::
  forall context.
  Symbolic context =>
  Natural -> Vector NumLanes (ByteString LaneWidth context) -> Vector NumLanes (ByteString LaneWidth context)
iota roundNumber state = modify (\v -> VM.write v 0 $ xor (roundConstants !! roundNumber) (head state)) state

type CeilDiv n d = Div (n + d - 1) d

type SqueezeLanesToExtract algorithm = CeilDiv (ResultSizeInBytes (Rate algorithm)) (Div LaneWidth 8)

squeeze ::
  forall algorithm context.
  AlgorithmSetup algorithm =>
  Symbolic context =>
  Vector NumLanes (ByteString LaneWidth context) -> ByteString (ResultSizeInBits (Rate algorithm)) context
squeeze state =
  truncate
    . concat
    . P.fmap (reverseEndianness @LaneWidth)
    $ stateToBytes state
 where
  stateToBytes s = unfold @(SqueezeLanesToExtract algorithm) extract (0, s)
  threshold = div rate laneWidth
  extract (x, s)
    | x < threshold = (s !! (div x 5 + mod x 5 * 5), (P.succ x, s))
    | P.otherwise = extract (0, keccakF @context s)
  rate = value @(Rate algorithm)
  laneWidth = value @LaneWidth

{-# INLINE modify #-}
modify :: (forall s. V.MVector s a -> ST s ()) -> Vector n a -> Vector n a
modify f (Vector v) = Vector $ V.modify f v

{-# INLINE reverseBits #-}
reverseBits :: forall n context. Symbolic context => ByteString n context -> ByteString n context
reverseBits (ByteString cbs) = ByteString $ hmap reverse cbs
