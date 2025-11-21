{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoDeriveAnyClass #-}
-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ZkFold.Symbolic.Data.UInt (
  StrictConv (..),
  StrictNum (..),
  RegisterSize (..),
  KnownRegisterSize (..),
  GetRegisterSize,
  NumberOfRegisters,
  KnownRegisters,
  Ceil,
  registerSize,
  withNumberOfRegisters,
  withGetRegisterSize,
  withCeilRegSize,
  UInt (..),
  OrdWord,
  isValidUInt,
  resize,
  toNative,
  asWords,
  expMod,
  exp65537Mod,
  eea,
  natural,
  register,
  productMod,
) where

import Control.DeepSeq
import qualified Data.Aeson as Aeson
import Data.Foldable (foldr)
import Data.Functor ((<$>))
import Data.List (iterate, unfoldr, find)
import Data.Tuple (swap)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Natural (naturalFromInteger)
import Test.QuickCheck (Arbitrary (..), chooseInteger)
import Prelude (
  Integer,
  error,
  flip,
  otherwise,
  return,
  ($),
  (++),
  (.),
  (<>),
  type (~),
 )
import qualified Prelude as Haskell

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.Vector (Vector (..))
import qualified ZkFold.Data.Vector as V
import ZkFold.Prelude (length, replicate, replicateA, (!!))
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString hiding (resize)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Class (Symbolic, Arithmetic)
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Data.Collect (Collect)
import Data.Kind (Type)
import Data.Type.Bool (If)
import Data.Type.Ord (type (>?), OrdCond)
import GHC.TypeNats (CmpNat, withKnownNat)
import Type.Errors (ErrorMessage(..), TypeError)
import ZkFold.Data.Iso (Iso (..))
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Constraint (withDict, Dict (..))
import Data.Constraint.Unsafe (unsafeSNat)

data RegisterSize = Auto | Fixed Natural deriving (Haskell.Eq, Haskell.Show)

class KnownRegisterSize (r :: RegisterSize) where
  regSize :: RegisterSize

instance KnownRegisterSize Auto where
  regSize = Auto

instance KnownNat n => KnownRegisterSize (Fixed n) where
  regSize = Fixed (value @n)

type family MaxAdded (regCount :: Natural) :: Natural where
  MaxAdded regCount =
    OrdCond
      (CmpNat regCount (2 ^ Log2 regCount))
      (TypeError (Text "Impossible"))
      (Log2 regCount)
      (1 + Log2 regCount)

type family MaxRegisterSize (n :: Natural) (regCount :: Natural) :: Natural where
  MaxRegisterSize n regCount = Div (Log2 n - MaxAdded regCount) 2

type family NumberOfRegisters' (n :: Natural) (bits :: Natural) (c :: [Natural]) :: Natural where
  NumberOfRegisters' _ _ '[] = 0
  NumberOfRegisters' n bits (x ': xs) =
    OrdCond
      (CmpNat bits (x * MaxRegisterSize n x))
      x
      x
      (NumberOfRegisters' n bits xs)

type family ListRange (from :: Natural) (to :: Natural) :: [Natural] where
  ListRange from from = '[from]
  ListRange from to = from ': ListRange (from + 1) to

type family NumberOfRegistersN n b r :: Natural where
  NumberOfRegistersN _ 0 _ = 0
  NumberOfRegistersN n b (Fixed b) = 1
  NumberOfRegistersN n b (Fixed rs) = If (Mod b rs >? 0) (Div b rs + 1) (Div b rs) -- if rs <= maxregsize a, ceil (n / rs)
  NumberOfRegistersN n b Auto = NumberOfRegisters' n b (ListRange 1 50) -- TODO: Compilation takes ages if this constant is greater than 10000.
  -- But it is weird anyway if someone is trying to store a value
  -- which requires more than 50 registers.

type NumberOfRegisters (a :: Type) (bits :: Natural) (r :: RegisterSize) =
  NumberOfRegistersN (Order a) bits r

type KnownRegisters c bits r = KnownNat (NumberOfRegisters c bits r)

type Ceil a b = Div (a + b - 1) b

type family GetRegisterSizeN n b r :: Natural where
  GetRegisterSizeN _ 0 _ = 0
  GetRegisterSizeN _ _ (Fixed rs) = rs
  GetRegisterSizeN n b Auto = Ceil b (NumberOfRegistersN n b Auto)

type GetRegisterSize (a :: Type) (bits :: Natural) (r :: RegisterSize) =
  GetRegisterSizeN (Order a) bits r

-- TODO (Issue #18): hide this constructor
newtype UInt (n :: Natural) (r :: RegisterSize) c = UInt
  {uintData :: Vector (NumberOfRegisters c n r) c}
  deriving (Generic, NFData, Haskell.Eq, Haskell.Show)
  deriving (Collect (ConstrainedDatum c), Eq) via (Vec (Vector (NumberOfRegisters c n r)) c)

instance SymbolicData (UInt n r) where
  type Layout (UInt n r) c = Vector (NumberOfRegisters c n r)
  type HasRep (UInt n r) c = KnownRegisters c n r

  toLayout = uintData
  fromLayout = UInt
  interpolate = error "TODO"

isValidUInt
  :: (Symbolic c, KnownNat n, KnownRegisterSize r)
  => UInt n r c -> Bool c
isValidUInt = error "TODO"

toNative
  :: forall n r c
   . (Symbolic c, KnownNat n, KnownRegisterSize r)
  => KnownNat (GetRegisterSize c n r)
  => UInt n r c -> FieldElement c
toNative (UInt _rs) = FieldElement $ error "TODO"
    -- symbolicF
    --   rs
    --   (Par1 . fromConstant . (`vectorToNatural` registerSize @c @n @r))
    --   (fmap Par1 . hornerW @(GetRegisterSize c n r) . toList)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => FromConstant Natural (UInt n r c) where
  fromConstant _ = UInt (error "TODO") -- embed @c $ naturalToVector @c @n @r c

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => FromConstant Integer (UInt n r c) where
  fromConstant = fromConstant . naturalFromInteger . (`Haskell.mod` (2 ^ value @n))

instance (Symbolic c, KnownRegisterSize r) => FromConstant Word64 (UInt 64 r c) where
  fromConstant = fromConstant . Haskell.toInteger

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Scale Natural (UInt n r c)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Scale Integer (UInt n r c)

instance MultiplicativeMonoid (UInt n r c) => Exponent (UInt n r c) Natural where
  (^) = natPow

-- | @expMod n pow modulus@ calculates @n^pow % modulus@ where all values are arithmetised
expMod
  :: forall c n p m r
   . Symbolic c
  => KnownRegisterSize r
  => KnownNat p
  => KnownNat n
  => KnownNat m
  => KnownNat (2 * m)
  => KnownRegisters c (2 * m) r
  => KnownNat (Ceil (GetRegisterSize c (2 * m) r) OrdWord)
  => UInt n r c
  -> UInt p r c
  -> UInt m r c
  -> UInt m r c
expMod n pow modulus = resize result
 where
  bits :: ByteString p c
  bits = from pow

  m' :: UInt (2 * m) r c
  m' = resize modulus

  n' :: UInt (2 * m) r c
  n' = resize n `mod` m'

  result :: UInt (2 * m) r c
  result = bitsPow (value @p) bits one n' m'

-- | n ^ 65537 `mod` modulus
exp65537Mod
  :: forall c n m r
   . Symbolic c
  => KnownRegisterSize r
  => KnownNat n
  => KnownNat m
  => KnownNat (2 * m)
  => KnownRegisters c (2 * m) r
  => KnownNat (Ceil (GetRegisterSize c (2 * m) r) OrdWord)
  => UInt n r c
  -> UInt m r c
  -> UInt m r c
-- exp65537Mod n modulus = resize $ Haskell.snd $ productMod n' n' m'
exp65537Mod n modulus = resize $ Haskell.snd $ productMod sq_2_16 n' m'
 where
  m' :: UInt (2 * m) r c
  m' = resize modulus

  n' :: UInt (2 * m) r c
  n' = resize n

  sq_2_16 = iterate (\x -> Haskell.snd $ productMod x x m') n' !! (16 :: Natural)

bitsPow
  :: forall c n p r
   . Symbolic c
  => KnownRegisterSize r
  => KnownNat n
  => KnownNat p
  => KnownRegisters c n r
  => KnownNat (Ceil (GetRegisterSize c n r) OrdWord)
  => Natural
  -> ByteString p c
  -> UInt n r c
  -> UInt n r c
  -> UInt n r c
  -> UInt n r c
bitsPow 0 _ res _ _ = res
bitsPow b bits res n m = bitsPow (b -! 1) bits newRes sq m
 where
  sq = Haskell.snd $ productMod n n m
  newRes = force $ ifThenElse (isSet bits (b -! 1)) (Haskell.snd $ productMod res n m) res

-- | Calculate @a * b `divMod` m@ using less constraints than would've been required by these operations used consequently
productMod
  :: forall c n r
   . Symbolic c
  => KnownRegisterSize r
  => KnownNat n
  => KnownRegisters c n r
  => KnownNat (Ceil (GetRegisterSize c n r) OrdWord)
  => UInt n r c
  -> UInt n r c
  -> UInt n r c
  -> (UInt n r c, UInt n r c)
productMod (UInt _aRegs) (UInt _bRegs) (UInt _mRegs) =
  case (value @n) of
    0 -> (zero, zero)
    _ -> (UInt (error "TODO") {- $ hmap fstP circuit -}, UInt (error "TODO") {- $ hmap sndP circuit -})
-- where
--  source = symbolic3F
--    aRegs
--    bRegs
--    mRegs
--    ( \ar br mr ->
--        let r = registerSize @c @n @r
--            a' = vectorToNatural ar r
--            b' = vectorToNatural br r
--            m' = vectorToNatural mr r
--         in naturalToVector @c @n @r ((a' * b') `div` m')
--              :*: naturalToVector @c @n @r ((a' * b') `mod` m')
--    )
--    \ar br mr ->
--      (liftA2 (:*:) `on` traverse unconstrained)
--        ( tabulate $
--            register @c @n @r @c
--              ( ( natural @c @c @n @r (at <$> ar)
--                    * natural @c @c @n @r (at <$> br)
--                )
--                  `div` natural @c @c @n @r (at <$> mr)
--              )
--        )
--        ( tabulate $
--            register @c @n @r @c
--              ( ( natural @c @c @n @r (at <$> ar)
--                    * natural @c @c @n @r (at <$> br)
--                )
--                  `mod` natural @c @c @n @r (at <$> mr)
--              )
--        )

  -- \| Unconstrained @div@ part.
--  dv = hmap fstP source

  -- \| Unconstrained @mod@ part.
--  md = hmap sndP source

--  Bool eqCase = (UInt aRegs :: UInt n r c) `unsafeMulNoPad` UInt bRegs == UInt dv `unsafeMulNoPad` UInt mRegs + UInt md

--  Bool ltCase = (UInt md :: UInt n r c) < UInt mRegs

--  circuit = fromCircuit3F eqCase ltCase (dv `hpair` md) \(Par1 e) (Par1 l) dm -> do
--    constraint (($ e) - one)
--    constraint (($ l) - one)
--    return dm

log2 :: Natural -> Haskell.Double
log2 = Haskell.logBase 2 . Haskell.fromIntegral

numberOfRegisters :: forall a n r. (Finite a, KnownNat n, KnownRegisterSize r) => Natural
numberOfRegisters = case regSize @r of
  Auto ->
    fromMaybe (error "too many bits, field is not big enough") $
      find (\c -> c * maxRegisterSize c Haskell.>= value @n) [0 .. maxRegisterCount]
   where
    maxRegisterCount = 2 ^ bitLimit
    bitLimit = Haskell.floor $ log2 (order @a)
    maxRegisterSize 0 = 0
    maxRegisterSize regCount =
      let maxAdded = Haskell.ceiling $ log2 regCount
       in Haskell.floor $ (bitLimit -! maxAdded) % 2
  Fixed rs -> Haskell.ceiling (value @n % rs)

registerSize :: forall a n r. (Finite a, KnownNat n, KnownRegisterSize r) => Natural
registerSize = case regSize @r of
  Auto -> Haskell.ceiling (value @n % numberOfRegisters @a @n @r)
  Fixed rs -> rs

withGetRegisterSize
  :: forall n r a {k}. (KnownNat n, KnownRegisterSize r, Finite a)
  => (KnownNat (GetRegisterSize a n r) => k) -> k
withGetRegisterSize = withDict @(KnownNat (GetRegisterSize a n r))
  $ withKnownNat @(GetRegisterSize a n r) (unsafeSNat (registerSize @a @n @r))
      Dict

withCeilRegSize :: forall rs ow {k}. (KnownNat rs, KnownNat ow) => (KnownNat (Ceil rs ow) => k) -> k
withCeilRegSize = withDict @(KnownNat (Ceil rs ow))
  $ withKnownNat @(Ceil rs ow)
      (unsafeSNat (Haskell.div (value @rs + value @ow -! 1) (value @ow)))
      Dict

cast :: forall a n r. (Finite a, KnownNat n, KnownRegisterSize r) => Natural -> ([Natural], Natural, [Natural])
cast n =
  let base = 2 ^ registerSize @a @n @r
      registers =
        flip unfoldr n \case
          0 -> Haskell.Nothing
          x -> Haskell.Just (swap $ x `Haskell.divMod` base)
      r = numberOfRegisters @a @n @r -! 1
   in case greedySplitAt r registers of
        (lo, hi : rest) -> (lo, hi, rest)
        (lo, []) -> (lo ++ replicate (r -! length lo) zero, zero, [])
 where
  greedySplitAt 0 xs = ([], xs)
  greedySplitAt _ [] = ([], [])
  greedySplitAt m (x : xs) =
    let (ys, zs) = greedySplitAt (m -! 1) xs
     in (x : ys, zs)

-- | Extended Euclidean algorithm.
-- Exploits the fact that @s_i@ and @t_i@ change signs in turns on each iteration, so it adjusts the formulas correspondingly
-- and never requires signed arithmetic.
-- (i.e. it calculates @x = b - a@ instead of @x = a - b@ when @a - b@ is negative
-- and changes @y - x@ to @y + x@ on the following iteration)
-- This only affects Bezout coefficients, remainders are calculated without changes as they are always non-negative.
--
-- If the algorithm is used to calculate Bezout coefficients,
-- it requires that @a@ and @b@ are coprime, @b@ is not 1 and @a@ is not 0, otherwise the optimisation above is not valid.
--
-- If the algorithm is only used to find @gcd(a, b)@ (i.e. @s@ and @t@ will be discarded), @a@ and @b@ can be arbitrary integers.
eea
  :: forall n c r
   . SemiEuclidean (UInt n r c)
  => KnownNat n
  => AdditiveGroup (UInt n r c)
  => UInt n r c -> UInt n r c -> (UInt n r c, UInt n r c, UInt n r c)
eea a b = eea' 1 a b one zero zero one
 where
  iterations :: Natural
  iterations = value @n * 2 + 1

  eea'
    :: Natural
    -> UInt n r c
    -> UInt n r c
    -> UInt n r c
    -> UInt n r c
    -> UInt n r c
    -> UInt n r c
    -> (UInt n r c, UInt n r c, UInt n r c)
  eea' iteration oldR r oldS s oldT t
    | iteration Haskell.== iterations = (oldS, oldT, oldR)
    | otherwise =
        ( bool recS (if Haskell.even iteration then b - oldS else oldS) (r == zero)
        , bool recT (if Haskell.odd iteration then a - oldT else oldT) (r == zero)
        , bool recR oldR (r == zero)
        )
   where
    quotient = oldR `div` r

    (recS, recT, recR) =
      eea'
        (iteration + 1)
        r
        (oldR - quotient * r)
        s
        (quotient * s + oldS)
        t
        (quotient * t + oldT)

--------------------------------------------------------------------------------

instance (Arithmetic a, KnownNat n, KnownRegisterSize r) => ToConstant (UInt n r a) where
  type Const (UInt n r a) = Natural
  toConstant (UInt xs) = vectorToNatural xs (registerSize @a @n @r)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => MultiplicativeMonoid (UInt n r c) where
  one = fromConstant (1 :: Natural)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Semiring (UInt n r c)

highRegisterSize :: forall a n r. (Finite a, KnownNat n, KnownRegisterSize r) => Natural
highRegisterSize = value @n -! registerSize @a @n @r * (numberOfRegisters @a @n @r -! 1)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Arbitrary (UInt n r c) where
  arbitrary
    | value @n == 0 = return zero
    | otherwise = do
        lo <- replicateA (numberOfRegisters @c @n @r -! 1) (toss $ registerSize @c @n @r)
        hi <- toss (highRegisterSize @c @n @r)
        return $ UInt $ V.unsafeToVector (lo <> [hi])
   where
    toss b = fromConstant <$> chooseInteger (0, 2 ^ b - 1)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Iso (ByteString n c) (UInt n r c) where
  from (ByteString _)
    | value @n == 0 = zero
    | otherwise =
        UInt $ error "TODO"
--          symbolicF
--            b
--            (naturalToVector @c @n @r . Haskell.foldl (\y p -> toConstant p + 2 * y) 0)
--            ( \bits -> do
--                let bsBits = V.fromVector bits
--                V.unsafeToVector . Haskell.reverse
--                  <$> fromBits (highRegisterSize @c @n @r) (registerSize @c @n @r) bsBits
--            )

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Iso (UInt n r c) (ByteString n c) where
  from (UInt _)
    | value @n == 0 = ByteString $ V.unsafeToVector []
    | otherwise =
        ByteString $ error "TODO"
--          symbolicF
--            u
--            (\v -> V.unsafeToVector $ fromConstant <$> toBsBits (vectorToNatural v (registerSize @c @n @r)) (value @n))
--            ( \ui -> do
--                let regs = V.fromVector ui
--                V.unsafeToVector
--                  <$> toBits (Haskell.reverse regs) (highRegisterSize @c @n @r) (registerSize @c @n @r)
--            )

instance (Symbolic c, KnownRegisterSize r, NumberOfBits c ~ n) => Iso (FieldElement c) (UInt n r c) where
  from a = from (from a :: ByteString n c)

instance (Symbolic c, KnownRegisterSize r, NumberOfBits c ~ n) => Iso (UInt n r c) (FieldElement c) where
  from a = from (from a :: ByteString n c)

-- --------------------------------------------------------------------------------

resize
  :: forall c n k r s
   . ( Symbolic c, KnownNat n, KnownNat k
     , KnownRegisterSize r, KnownRegisterSize s
     )
  => UInt n r c -> UInt k s c
resize (UInt _bits)
    | value @n == 0 = zero
    | value @k == 0 = zero
    | otherwise =
        UInt $ error "TODO"
--          symbolicF
--            bits
--            (\l -> naturalToVector @c @k @s (vectorToNatural l (registerSize @c @n @r)))
--            ( \v -> do
--                let regs = V.fromVector v
--                    ns = replicate (numberOfRegisters @c @n @r -! 1) n ++ [highRegisterSize @c @n @r]
--                    ks = replicate (numberOfRegisters @c @k @s -! 1) k ++ [highRegisterSize @c @k @s]
--                    zs = zip ns regs
--                resZ <- helper zs ks
--                let (_, res) = Haskell.unzip resZ
--                return $ V.unsafeToVector res
--            )
--   where
--    n = registerSize @c @n @r
--    k = registerSize @c @k @s

--    helper :: MonadCircuit i c w m => [(Natural, i)] -> [Natural] -> m [(Natural, i)]
--    helper _ [] = return []
--    helper [] (a : as) = do
--      ([(a, fromConstant @c zero)] <>) Haskell.<$> helper [] as
--    helper ((xN, xI) : xs) acc@(a : as)
--      | xN > a = do
--          (l, h) <- splitExpansion a (xN -! a) xI
--          ([(a, l)] <>) Haskell.<$> helper ((xN -! a, h) : xs) as
--      | xN == a = ([(a, xI)] <>) Haskell.<$> helper xs as
--      | otherwise = case xs of
--          [] -> ([(xN, xI)] <>) Haskell.<$> helper [] as
--          ((yN, yI) : ys) -> do
--            let newN = xN + yN
--            newI <- newAssigned (\j -> j xI + scale ((2 :: Natural) ^ xN) (j yI))
--            helper ((newN, newI) : ys) acc

-- | "natural" value from vector of registers.
natural
  :: forall w a n r
   . (PrimeField w, Finite a, KnownNat n, KnownRegisterSize r)
  => Vector (NumberOfRegisters a n r) w -> IntegralOf w
natural = foldr (\i c -> toIntegral i + fromConstant base * c) zero
 where
  base :: Natural
  base = 2 ^ registerSize @a @n @r

-- | @register n i@ returns @i@-th register of @n@.
register
  :: forall f n r a
   . (PrimeField f, KnownNat n, KnownRegisterSize r, Finite a)
  => IntegralOf f -> Zp (NumberOfRegisters a n r) -> f
register c i =
  fromConstant ((c `div` fromConstant (2 ^ shift :: Natural)) `mod` base)
 where
  rs = registerSize @a @n @r
  base = fromConstant (2 ^ rs :: Natural)
  shift = Haskell.fromIntegral (toConstant i) * rs

instance
  ( Symbolic c
  , KnownNat n
  , KnownRegisterSize r
  , KnownRegisters c n r
  , regSize ~ GetRegisterSize c n r
  , KnownNat (Ceil regSize OrdWord)
  )
  => SemiEuclidean (UInt n r c)
  where
  divMod _num@(UInt _nm) _den@(UInt _dn) =
    (UInt (error "TODO") {- $ hmap fstP circuit -}, UInt (error "TODO") {- $ hmap sndP circuit -})
   -- where
    -- \| Computes unconstrained registers of @div@ and @mod@.
    -- source = symbolic2F
    --  nm
    --  dn
    --  ( \n d ->
    --      let r = registerSize @c @n @r
    --          n' = vectorToNatural n r
    --          d' = vectorToNatural d r
    --       in naturalToVector @c @n @r (n' `div` d')
    --            :*: naturalToVector @c @n @r (n' `mod` d')
    --  )
    --  \n d ->
    --    (liftA2 (:*:) `on` traverse unconstrained)
    --      ( tabulate $
    --         register @c @n @r @c
    --            ( natural @c @c @n @r (at <$> n)
    --                `div` natural @c @c @n @r (at <$> d)
    --            )
    --      )
    --      ( tabulate $
    --          register @c @n @r @c
    --            ( natural @c @c @n @r (at <$> n)
    --                `mod` natural @c @c @n @r (at <$> d)
    --            )
    --      )

    -- \| Unconstrained @div@ part.
    -- dv = hmap fstP source

    -- \| Unconstrained @mod@ part.
    -- md = hmap sndP source

    -- \| divMod first constraint: @numerator = denominator * div + mod@.
    -- This should always be true.
    -- Bool eqCase = den * UInt dv + UInt md == num

    -- \| divMod second constraint: @0 <= mod < denominator@.
    -- This should always be true.
    -- Bool ltCase = UInt md < den

    -- \| Computes properly constrained registers of @div@ and @mod@.
    -- circuit = fromCircuit3F eqCase ltCase (dv `hpair` md) \(Par1 e) (Par1 l) dm -> do
    --   constraint (($ e) - one)
    --   constraint (($ l) - one)
    --   return dm

asWords
  :: forall wordSize regSize ctx k
   . Symbolic ctx
  => KnownNat (Ceil regSize wordSize)
  => KnownNat wordSize
  => Vector k ctx -- @k@ registers of size up to @regSize@
  -> Vector (k * Ceil regSize wordSize) ctx -- @k * wordsPerReg@ registers of size @wordSize@
asWords _ = error "TODO" --  fromCircuitF v $ \regs -> do
--  words <- Haskell.mapM (expansionW @wordSize wordsPerReg) regs
--  Haskell.pure $ V.reverse . V.unsafeToVector . Haskell.concat . V.fromVector $ words
-- where
--  wordsPerReg :: Natural
--  wordsPerReg = value @(Ceil regSize wordSize)

-- | Word size in bits used in comparisons. Subject to change
type OrdWord = 16

instance
  ( Symbolic c
  , KnownNat n
  , KnownRegisterSize r
  , KnownRegisters c n r
  , regSize ~ GetRegisterSize c n r
  , KnownNat (Ceil regSize OrdWord)
  )
  => Ord (UInt n r c)
  where
  type OrderingOf (UInt n r c) = Ordering c

  ordering x y z o = bool (bool x y (o == eq)) z (o == gt)

  compare x y = bool (bool lt eq (x == y)) gt (x > y)

  x <= y = y >= x

  x < y = y > x

  UInt u1 >= UInt u2 =
    let w1 = asWords @OrdWord @regSize u1
        w2 = asWords @OrdWord @regSize u2
     in bitwiseGE @OrdWord w1 w2

  UInt u1 > UInt u2 =
    let w1 = asWords @OrdWord @regSize u1
        w2 = asWords @OrdWord @regSize u2
     in bitwiseGT @OrdWord w1 w2

  max x y = bool x y (x < y)

  min x y = bool x y (x > y)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => AdditiveSemigroup (UInt n r c) where
  UInt _xc + UInt _yc
    | value @n == 0 = zero
    | otherwise =
        UInt $ error "TODO"
--          symbolic2F
--            xc
--            yc
--            ( \u v ->
--                naturalToVector @c @n @r $
--                  vectorToNatural u (registerSize @c @n @r) + vectorToNatural v (registerSize @c @n @r)
--            )
--            ( \xv yv -> do
--                j <- newAssigned (Haskell.const zero)
--                let xs = V.fromVector xv
--                    ys = V.fromVector yv
--                    midx = Haskell.init xs
--                    z = Haskell.last xs
--                    midy = Haskell.init ys
--                    w = Haskell.last ys
--                (zs, c) <-
--                  flip runStateT j $
--                    traverse StateT $
--                      Haskell.zipWith (fullAdder $ registerSize @c @n @r) midx midy
--                k <- fullAdded z w c
--                (ks, _) <- splitExpansion (highRegisterSize @c @n @r) 1 k
--                return $ V.unsafeToVector (zs ++ [ks])
--            )

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Zero (UInt n r c) where
  zero = fromConstant (0 :: Natural)

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => AdditiveMonoid (UInt n r c)

instance
  ( Symbolic c
  , KnownNat n
  , KnownRegisterSize r
  )
  => AdditiveGroup (UInt n r c)
  where
  UInt _x - UInt _y =
    UInt $ error "TODO"
--      symbolic2F
--        x
--        y
--        ( \u v ->
--            naturalToVector @c @n @r $
--              vectorToNatural u (registerSize @c @n @r)
--                + (2 ^ value @n)
--                -! vectorToNatural v (registerSize @c @n @r)
--        )
--        ( \xv yv -> do
--            let is = V.fromVector xv
--                js = V.fromVector yv
--            case Z.zip is js of
--              [] -> return $ V.unsafeToVector []
--              [(i, j)] -> V.unsafeToVector <$> solve1 i j
--              ((i, j) : rest) ->
--                let (z, w) = Haskell.last rest
--                    (ris, rjs) = Haskell.unzip $ Haskell.init rest
--                 in V.unsafeToVector <$> solveN (i, j) (ris, rjs) (z, w)
--        )
--   where
--    t :: c
--    t = (one + one) ^ registerSize @c @n @r

--    solve1 :: MonadCircuit i c w m => i -> i -> m [i]
--    solve1 i j = do
--      z0 <- newAssigned (\v -> v i - v j + fromConstant t)
--      (z, _) <- splitExpansion (highRegisterSize @c @n @r) 1 z0
--      return [z]

--    solveN :: MonadCircuit i c w m => (i, i) -> ([i], [i]) -> (i, i) -> m [i]
--    solveN (i, j) (is, js) (i', j') = do
--      s <- newAssigned (\v -> v i - v j + fromConstant t)
--      let r = registerSize @c @n @r
--      (k, b0) <- splitExpansion r 1 s
--      (zs, b) <- flip runStateT b0 $ traverse StateT (Haskell.zipWith (fullSub r) is js)
--      d <- newAssigned (\v -> v i' - v j')
--      s'0 <- newAssigned (\v -> v d + v b + fromConstant (2 ^ highRegisterSize @c @n @r -! 1 :: Natural))
--      (s', _) <- splitExpansion (highRegisterSize @c @n @r) 1 s'0
--      return (k : zs <> [s'])

  negate :: UInt n r c -> UInt n r c
  negate (UInt _x) =
    UInt $ error "TODO"
--      symbolicF
--        x
--        (\v -> naturalToVector @c @n @r (2 ^ value @n) -! vectorToNatural v (registerSize @c @n @r))
--        ( \xv ->
--            if Haskell.null (V.fromVector xv)
--              then return xv
--              else do
--                j <- newAssigned (Haskell.const zero)
--                let xs = V.fromVector xv
--                    y = 2 ^ registerSize @c @n @r
--                    ys = replicate (numberOfRegisters @c @n @r -! 2) (2 ^ registerSize @c @n @r -! 1)
--                    y' = 2 ^ highRegisterSize @c @n @r -! 1
--                    ns
--                      | numberOfRegisters @c @n @r Haskell.== 1 = [y' + 1]
--                      | otherwise = (y : ys) <> [y']
--                    (init_ns, last_ns) = fromJust $ unsnoc ns
--                    (init_xs, last_xs) = fromJust $ unsnoc xs
--                (zs, p) <- flip runStateT j $ traverse StateT (Haskell.zipWith negateN init_ns init_xs)
--                (zp_head, _) <- negateNH last_ns last_xs p
--                return $ V.unsafeToVector (zs <> [zp_head])
--        )
--   where
--    negateN :: MonadCircuit i c w m => Natural -> i -> i -> m (i, i)
--    negateN n i b = do
--      r <- newAssigned (\v -> fromConstant n - v i + v b)
--      splitExpansion (registerSize @c @n @r) 1 r

--    negateNH :: MonadCircuit i c w m => Natural -> i -> i -> m (i, i)
--    negateNH n i b = do
--      r <- newAssigned (\v -> fromConstant n - v i + v b)
--      splitExpansion (highRegisterSize @c @n @r) 1 r

type SecondNextPow2 n = 2 ^ (Log2 (2 * n - 1) + 1)

withNumberOfRegisters
  :: forall n r a {k}
  . (KnownNat n, KnownRegisterSize r, Finite a)
  => (KnownRegisters a n r => k) -> k
withNumberOfRegisters = withDict @(KnownRegisters a n r)
  $ withKnownNat @(NumberOfRegisters a n r)
      (unsafeSNat (numberOfRegisters @a @n @r)) Dict

withSecondNextNBits
  :: forall n {r}. KnownNat n => (KnownNat (Log2 (2 * n - 1) + 1) => r) -> r
withSecondNextNBits = withDict @(KnownNat (Log2 (2 * n - 1) + 1))
  $ withKnownNat @(Log2 (2 * n - 1) + 1)
      (unsafeSNat (ilog2 (2 * value @n -! 1) + 1))
      Dict

instance
  ( Symbolic c
  , KnownNat n
  , KnownRegisterSize rs
  )
  => MultiplicativeSemigroup (UInt n rs c)
  where
  UInt x * UInt _y = UInt $
    case (value @n) of
      0 -> x
      _ ->
        withNumberOfRegisters @n @rs @c $
          withSecondNextNBits @(NumberOfRegisters c n rs) $
            trimRegisters @c @n @rs $
              mulFFT @c xPadded yPadded
   where
    xPadded, yPadded :: Vector (SecondNextPow2 (NumberOfRegisters c n rs)) c
    xPadded = withNumberOfRegisters @n @rs @c $ error "TODO" -- fromCircuitF x padSecondNextPow2
    yPadded = withNumberOfRegisters @n @rs @c $ error "TODO" -- fromCircuitF y padSecondNextPow2

trimRegisters
  :: forall c n rs k
   . Symbolic c
  => KnownNat n
  => KnownRegisterSize rs
  => Vector (2 ^ k) c -> Vector (NumberOfRegisters c n rs) c
trimRegisters _ = error "TODO" -- fromCircuitF c $ \regs -> do
--  let rs = take (numberOfRegisters @c @n @rs) $ V.fromVector regs
--      lows = Haskell.init rs
--      hi = Haskell.last rs
--  z <- newAssigned (const zero)
--  (newLows, carry) <- foldlM step ([], z) lows

--  let highOverflow = registerSize @c @n @rs + maxOverflow @c @n @rs -! highRegisterSize @c @n @rs
--  s <- newAssigned (\p -> p carry + p hi)
--  (newHi, _) <- splitExpansion (highRegisterSize @c @n @rs) highOverflow s

--  pure $ V.unsafeToVector $ Haskell.reverse (newHi : newLows)
-- where
--  step :: forall i w m. MonadCircuit i c w m => ([i], i) -> i -> m ([i], i)
--  step (acc, cr) r = do
--    s <- newAssigned (\p -> p cr + p r)
--    (l, h) <- splitExpansion (registerSize @c @n @rs) (maxOverflow @c @n @rs) s
--    pure (l : acc, h)

mulFFT
  :: forall c k
   . Symbolic c
  => KnownNat k
  => Vector (2 ^ k) c -> Vector (2 ^ k) c -> Vector (2 ^ k) c
mulFFT _x _y = c
 where
  _xHat, _yHat :: Vector (2 ^ k) c
  _xHat = error "TODO"
  _yHat = error "TODO"

  c :: Vector (2 ^ k) c
  c = error "TODO" -- ifft $ fromCircuit2F xHat yHat $ \u v ->
    -- V.unsafeToVector <$> zipWithM (\i j -> newAssigned $ \p -> p i * p j) (V.fromVector u) (V.fromVector v)

instance
  ( Symbolic c
  , KnownNat n
  , KnownRegisterSize r
  )
  => Ring (UInt n r c)

--------------------------------------------------------------------------------

class StrictConv b a where
  strictConv :: b -> a

instance (Symbolic c, KnownNat n, KnownRegisterSize rs) => StrictConv Natural (UInt n rs c) where
  strictConv n
    | value @n == 0 = zero
    | otherwise = case cast @c @n @rs n of
        (lo, hi, []) -> UInt $ V.unsafeToVector $ fromConstant <$> (lo <> [hi])
        _ -> error "strictConv: overflow"

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => StrictConv (Zp p) (UInt n r c) where
  strictConv = strictConv . toConstant

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => StrictConv (FieldElement c) (UInt n r c) where
  strictConv _a =
    UInt $ error "TODO"
--      symbolicF
--        a
--        (\p -> V.unsafeToVector [unPar1 p])
--        ( \xv -> do
--            let i = unPar1 xv
--                len = Haskell.min (getNatural @n) (numberOfBits @c)
--            bits <- Haskell.reverse <$> expansion len i
--            V.unsafeToVector <$> fromBits (highRegisterSize @c @n @r) (registerSize @c @n @r) bits
--        )

class StrictNum a where
  strictAdd :: a -> a -> a
  strictSub :: a -> a -> a
  strictMul :: a -> a -> a

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => StrictNum (UInt n r c) where
  strictAdd (UInt _x) (UInt _y)
    | value @n == 0 = zero
    | otherwise =
        UInt $ error "TODO"
--         symbolic2F
--            x
--            y
--            ( \u v ->
--                naturalToVector @c @n @r $
--                  vectorToNatural u (registerSize @c @n @r) + vectorToNatural v (registerSize @c @n @r)
--            )
--            ( \xv yv -> do
--                j <- newAssigned (Haskell.const zero)
--                let xs = V.fromVector xv
--                    ys = V.fromVector yv
--                    z = Haskell.last xs
--                    w = Haskell.last ys
--                    midx = Haskell.init xs
--                    midy = Haskell.init ys
--                (zs, c) <-
--                  flip runStateT j $
--                    traverse StateT $
--                      Haskell.zipWith (fullAdder $ registerSize @c @n @r) midx midy
--                k <- fullAdded z w c
--                (ks, _) <- splitExpansion (highRegisterSize @c @n @r) 1 k
--                return $ V.unsafeToVector (zs ++ [ks])
--            )

  strictSub (UInt _x) (UInt _y) =
    UInt $ error "TODO"
--      symbolic2F
--        x
--        y
--        ( \u v ->
--            naturalToVector @c @n @r $
--              vectorToNatural u (registerSize @c @n @r) -! vectorToNatural v (registerSize @c @n @r)
--        )
--        ( \xv yv -> do
--            case V.fromVector $ Z.zip xv yv of
--              [] -> return $ V.unsafeToVector []
--              [(i, j)] -> V.unsafeToVector <$> solve1 i j
--              ((i, j) : rest) ->
--                let (z, w) = Haskell.last rest
--                    (ris, rjs) = Haskell.unzip $ Haskell.init rest
--                 in V.unsafeToVector <$> solveN (i, j) (ris, rjs) (z, w)
--        )
--   where
--    t :: c
--    t = (one + one) ^ registerSize @c @n @r - one

--    solve1 :: MonadCircuit i c w m => i -> i -> m [i]
--    solve1 i j = do
--      z <- newAssigned (\v -> v i - v j)
--      _ <- expansion (highRegisterSize @c @n @r) z
--      return [z]

--    solveN :: MonadCircuit i c w m => (i, i) -> ([i], [i]) -> (i, i) -> m [i]
--    solveN (i, j) (is, js) (i', j') = do
--      s <- newAssigned (\v -> v i - v j + fromConstant (t + one))
--      let r = registerSize @c @n @r
--      (k, b0) <- splitExpansion r 1 s
--      (zs, b) <- flip runStateT b0 $ traverse StateT (Haskell.zipWith (fullSub r) is js)
--      k' <- newAssigned (\v -> v i' - v j')
--      s' <- newAssigned (\v -> v k' + v b - one)
--      _ <- expansion (highRegisterSize @c @n @r) s'
--      return (k : zs <> [s'])

  strictMul (UInt _x) (UInt _y) =
    UInt $ error "TODO"
--      symbolic2F
--        x
--        y
--        ( \u v ->
--            naturalToVector @c @n @r $
--              vectorToNatural u (registerSize @c @n @r)
--              * vectorToNatural v (registerSize @c @n @r)
--        )
--        ( \xv yv -> do
--            case V.fromVector $ Z.zip xv yv of
--              [] -> return $ V.unsafeToVector []
--              [(i, j)] -> V.unsafeToVector <$> solve1 i j
--              ((i, j) : rest) ->
--                let (z, w) = Haskell.last rest
--                    (ris, rjs) = Haskell.unzip $ Haskell.init rest
--                 in V.unsafeToVector <$> solveN (i, j) (ris, rjs) (z, w)
--        )
--   where
--    solve1 :: MonadCircuit i c w m => i -> i -> m [i]
--    solve1 i j = do
--      z <- newAssigned $ \v -> v i * v j
--      _ <- expansion (highRegisterSize @c @n @r) z
--      return [z]

--    solveN :: MonadCircuit i c w m => (i, i) -> ([i], [i]) -> (i, i) -> m [i]
--    solveN (i, j) (is, js) (i', j') = do
--      let cs = fromList $ zip [0 ..] (i : is ++ [i'])
--          ds = fromList $ zip [0 ..] (j : js ++ [j'])
--          r = numberOfRegisters @c @n @r
--      -- single addend for lower register
--      q <- newAssigned (\v -> v i * v j)
--      -- multiple addends for middle registers
--      qs <- for [1 .. r -! 2] $ \k ->
--        for [0 .. k] $ \l ->
--          newAssigned (\v -> v (cs ! l) * v (ds ! (k -! l)))
--      -- lower register
--      (p, c) <- splitExpansion (registerSize @c @n @r) (registerSize @c @n @r) q
--      -- middle registers
--      (ps, c') <-
--        flip runStateT c $
--          for qs $
--            StateT . \rs c' -> do
--              s <- foldrM (\k l -> newAssigned (\v -> v k + v l)) c' rs
--              splitExpansion (registerSize @c @n @r) (maxOverflow @c @n @r) s
--      -- high register
--      p' <-
--        foldrM
--          ( \k l -> do
--              k' <- newAssigned (\v -> v (cs ! k) * v (ds ! (r -! (k + 1))))
--              newAssigned (\v -> v l + v k')
--          )
--          c'
--          [0 .. r -! 1]
--      _ <- expansion (highRegisterSize @c @n @r) p'
--      -- all addends higher should be zero
--      for_ [r .. r * 2 -! 2] $ \k ->
--        for_ [k -! r + 1 .. r -! 1] $ \l ->
--          constraint (\v -> v (cs ! l) * v (ds ! (k -! l)))
--      return (p : ps <> [p'])

vectorToNatural :: (ToConstant a, Const a ~ Natural) => Vector n a -> Natural -> Natural
vectorToNatural v n = foldr (\l r -> fromConstant l + b * r) 0 vs
 where
  vs = Haskell.map toConstant $ V.fromVector v :: [Natural]
  b = 2 ^ n

instance (Symbolic c, KnownNat n, KnownRegisterSize r) => Aeson.FromJSON (UInt n r c) where
  parseJSON = Haskell.fmap strictConv . Aeson.parseJSON @Natural

instance (PrimeField (Zp p), KnownNat n, KnownRegisterSize r) => Aeson.ToJSON (UInt n r (Zp p)) where
  toJSON = Aeson.toJSON . toConstant
