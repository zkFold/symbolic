{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Use concatMap" -}
{- HLINT ignore "Use zipWithM" -}

module ZkFold.Symbolic.Data.UIntData where

import Control.Applicative (pure, (<*>))
import Data.Bool (otherwise)
import Data.Constraint (Dict (..), unmapDict, withDict, (:-), (\\))
import Data.Constraint.Nat
import Data.Constraint.Unsafe (unsafeAxiom)
import Data.Foldable (foldr)
import Data.Function (const, flip, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List (uncons)
import Data.Maybe (Maybe (..), fromJust, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Semialign (zipWith)
import Data.Semigroup ((<>))
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import Data.Type.Equality (type (~))
import Data.Type.Ord (OrderingI (..))
import qualified Data.Vector as UV
import GHC.Enum (Enum (toEnum))
import GHC.Generics (Generic, Generic1, type (:*:) (..))
import GHC.Integer (Integer)
import GHC.Natural (naturalFromInteger)
import GHC.Real (even, odd)
import GHC.TypeNats
import Generic.Random (genericArbitrary, uniform)
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.DFT (genericDft)
import ZkFold.Algebra.Field (Zp, fromZp)
import ZkFold.Algebra.Number
import ZkFold.Control.BackwardsState
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq, (==))
import ZkFold.Data.Ord (Ord, (<))
import ZkFold.Data.Product (toPair)
import ZkFold.Data.Summoner (NumExpr (..), Summon (summon))
import ZkFold.Data.Vector (Vector, toV)
import qualified ZkFold.Data.Vector as V
import ZkFold.Prelude (length, log2ceiling)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.ByteString
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (FieldElement), fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput, isValid)
import ZkFold.Symbolic.Data.Register
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

data UIntData n k c = MkUIntData
  { highRegister :: Register (Mod n k) c
  , lowRegisters :: Vector (Div n k) (Register k c)
  }
  deriving (Collect (ConstrainedDatum c), Eq, Generic, Generic1, Show, SymbolicData)

type MaxAdded n k = k + Log2 (Max (Div n k) 1) + 1

type KnownUIntData n k =
  ( KnownNat n
  , KnownNat (2 ^ n)
  , KnownNat k
  , KnownNat (Div n k)
  , KnownNat (Mod n k)
  , 1 <= k
  )

knownUIntData
  :: forall n k. (KnownNat n, KnownNat k, 1 <= k) :- KnownUIntData n k
knownUIntData = unmapDict \Dict ->
  Dict \\ divNat @n @k \\ modNat @n @k \\ pow2Nat @n

instance KnownUIntData n k => SymbolicInput (UIntData n k)

instance (KnownUIntData n k, Symbolic c) => Arbitrary (UIntData n k c) where
  arbitrary = genericArbitrary uniform

instance (KnownUIntData n k, Symbolic c) => Ord (UIntData n k c)

instance {-# OVERLAPPING #-} FromConstant (UIntData n k c) (UIntData n k c)

instance
  {-# OVERLAPPING #-}
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => Scale (UIntData n k c) (UIntData n k c)

uintDataFromSemiEuclidean
  :: (SemiEuclidean a, FromConstant a c, KnownUIntData n k)
  => a -> UIntData n k c
uintDataFromSemiEuclidean x = evalBState x do
  MkUIntData
    <$> MkBState limb
    <*> sequence (pure $ MkBState limb)

instance
  (KnownUIntData n k, FromConstant a (Zp (2 ^ n)), FromConstant Natural c)
  => FromConstant a (UIntData n k c)
  where
  fromConstant = uintDataFromSemiEuclidean . fromZp @(2 ^ n) . fromConstant

uintDataToIntegral :: (KnownNat k, Symbolic c) => UIntData n k c -> IntegralOf c
uintDataToIntegral (MkUIntData hi lo) = regsToIntegral (V.reverse lo) hi

instance (KnownNat n, KnownNat k, Arithmetic a) => ToConstant (UIntData n k a) where
  type Const (UIntData n k a) = Zp (2 ^ n)
  toConstant = withDict (pow2Nat @n) (fromConstant . uintDataToIntegral)

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => Scale Natural (UIntData n k c)

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => Scale Integer (UIntData n k c)

add
  :: forall n k c
   . (KnownUIntData n k, Symbolic c)
  => UIntData n k c -> UIntData n k c -> (UIntData n k c, Bool c)
MkUIntData hi lo `add` MkUIntData hi' lo' = flip runBState false do
  MkUIntData <$> singleAdd hi hi' <*> sequence (zipWith singleAdd lo lo')
 where
  singleAdd
    :: KnownNat r
    => Register r c -> Register r c -> BState (Bool c) (Register r c)
  singleAdd x y = MkBState \c -> regToBool <$> splitR (adder x y c)

instance (KnownUIntData n k, Symbolic c) => AdditiveSemigroup (UIntData n k c) where
  x + y = fst (add x y)

instance (KnownNat (Div n k), Symbolic c) => Zero (UIntData n k c) where
  zero = MkUIntData zero zero

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => AdditiveMonoid (UIntData n k c)

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => AdditiveGroup (UIntData n k c)
  where
  negate (MkUIntData hi lo) = evalBState true do
    MkUIntData
      <$> singleNeg hi
      <*> traverse singleNeg lo
   where
    singleNeg :: KnownNat r => Register r c -> BState (Bool c) (Register r c)
    singleNeg x = MkBState \c ->
      let (y, c') = negateR x in (ifThenElse c y (not x), c && c')

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => Exponent (UIntData n k c) Natural
  where
  (^) = natPow

regCount :: UIntData n k c -> Int
regCount = P.succ . UV.length . toV . lowRegisters

multiply
  :: forall n k c
   . (Symbolic c, KnownUIntData n k, KnownNat (MaxAdded n k))
  => Int
  -> UIntData n k c
  -> UIntData n k c
  -> (UV.Vector (FieldElement c), Register (MaxAdded n k) c, UIntData n k c)
multiply rank left right =
  let w2n = fromJust $ rootOfUnity (toEnum rank) -- FIXME if rank is too big
      dftLen = 2 P.^ rank
      padLen = dftLen P.- regCount left
      toVec (MkUIntData hi (toV -> lo)) =
        genericDft rank w2n $
          UV.replicate padLen zero
            <> UV.singleton (regToFE hi)
            <> UV.map regToFE lo
      (padding, fromJust . UV.uncons -> (hi0, V.Vector -> lo0)) =
        UV.splitAt padLen $
          fmap (// fromConstant (toEnum dftLen :: Natural)) $
            genericDft rank (finv w2n) $
              zipWith (*) (toVec left) (toVec right)
      (result, carry) = flip runBState zero do
        MkUIntData <$> propagate hi0 <*> traverse propagate lo0
   in (padding, carry, result)
 where
  propagate
    :: forall r s
     . (KnownNat r, KnownNat s)
    => FieldElement c -> BState (Register s c) (Register r c)
  propagate x = MkBState \(FieldElement . fromRegister -> y) ->
    splitR (MkRegister $ fromFieldElement $ x + y)

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => MultiplicativeSemigroup (UIntData n k c)
  where
  x * y = let (_, _, z) = multiply (P.succ $ log2ceiling $ regCount x) x y in z

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => MultiplicativeMonoid (UIntData n k c)
  where
  one = fromConstant (one :: Natural)

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => Semiring (UIntData n k c)

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => Ring (UIntData n k c)

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => SemiEuclidean (UIntData n k c)
  where
  x `divMod` y =
    toPair
      $ assert
        ( \p@(d :*: m) ->
            isValid p && ((y == zero && d == zero) || m < y) && d * y + m == x
        )
      $ (\(d, m) -> uintDataFromSemiEuclidean d :*: uintDataFromSemiEuclidean m)
      $ uintDataToIntegral x `divMod` uintDataToIntegral y

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => Euclidean (UIntData n k c)
  where
  -- \| Exploits the fact that @s_i@ and @t_i@ change signs in turns on each
  -- iteration, so it adjusts the formulas correspondingly and never requires
  -- signed arithmetic. (i.e. it calculates @x = b - a@ instead of @x = a - b@
  -- when @a - b@ is negative and changes @y - x@ to @y + x@ on the following
  -- iteration) This only affects Bezout coefficients, remainders are calculated
  -- without changes as they are always non-negative.
  --
  -- If the algorithm is used to calculate Bezout coefficients, it requires that
  -- @a@ and @b@ are coprime, @b@ is not 1 and @a@ is not 0, otherwise the
  -- optimisation above is not valid.
  --
  -- If the algorithm is only used to find @gcd(a, b)@ (i.e. @s@ and @t@ will be
  -- discarded), @a@ and @b@ can be arbitrary integers.
  eea a b = eea' 1 a b one zero zero one
   where
    iterations :: Natural
    iterations = value @n * 2 + 1

    eea'
      :: Natural
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> UIntData n k c
      -> (UIntData n k c, UIntData n k c, UIntData n k c)
    eea' iteration oldR r oldS s oldT t
      | iteration == iterations = (oldS, oldT, oldR)
      | otherwise =
          ( bool recS (if even iteration then b - oldS else oldS) (r == zero)
          , bool recT (if odd iteration then a - oldT else oldT) (r == zero)
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

instance
  (KnownUIntData n k, KnownNat (MaxAdded n k), Symbolic c)
  => StrictNum (UIntData n k c)
  where
  x +! y = let (z, c) = add x y in assert (\_ -> not c) z

  x *! y =
    let (d, c, z) = multiply (log2ceiling $ regCount x) x y
     in assert (\_ -> all (== zero) d && c == zero) z

  MkUIntData hi lo -! MkUIntData hi' lo' =
    let (result, carry) = flip runBState true do
          MkUIntData
            <$> singleSub hi hi'
            <*> sequence (zipWith singleSub lo lo')
     in assert (const carry) result
   where
    singleSub
      :: KnownNat r
      => Register r c -> Register r c -> BState (Bool c) (Register r c)
    singleSub x y = MkBState \c -> regToBool <$> splitR (adder x (not y) c)

data SomeUInt k c
  = forall r.
  KnownNat r =>
  MkSomeUInt {hi :: Register r c, lo :: [Register k c]}

type UIntBuilder k c = (Natural, Natural) -> SomeUInt k c

regToBuilder :: KnownNat r => Register r c -> UIntBuilder k c
regToBuilder r _ = MkSomeUInt r []

uintBuilder
  :: forall n k c. KnownUIntData n k => UIntData n k c -> UIntBuilder k c
uintBuilder (MkUIntData hi lo) _ = MkSomeUInt hi (V.fromVector lo)

appendReg
  :: forall q l c
   . (KnownNat q, KnownNat l, Symbolic c)
  => Register q c -> UIntBuilder l c -> UIntBuilder l c
appendReg r f t@(targetLen, targetRegSize) = case f t of
  MkSomeUInt (hi :: Register r c) lo ->
    let h' = appendR hi r
     in withDict (plusNat @r @q) $
          if length lo == targetLen
            then
              if value @r < targetRegSize
                then MkSomeUInt h' lo
                else MkSomeUInt hi lo
            else appendHiReg h' lo
 where
  appendHiReg
    :: forall q'. KnownNat q' => Register q' c -> [Register l c] -> SomeUInt l c
  appendHiReg h l
    | length l == targetLen = MkSomeUInt h l
    | otherwise = case cmpNat @l @q' Proxy Proxy of
        GTI -> MkSomeUInt h l
        EQI -> MkSomeUInt (zero :: Register 0 c) (h : l)
        LTI ->
          withDict (minusNat @q' @l) $
            let (l0, h' :: Register (q' - l) c) =
                  splitR h
                    \\ plusCommutes @l @(q' - l)
                    \\ plusMinusInverse3 @l @q'
             in appendHiReg h' (l0 : l)

appendUIntData
  :: forall n k l c
   . (KnownUIntData n k, KnownNat l, Symbolic c)
  => UIntData n k c -> UIntBuilder l c -> UIntBuilder l c
appendUIntData (MkUIntData hi lo) builder =
  appendReg hi (foldr appendReg builder lo)

-- FIXME: assumes that l cannot be bigger than needed
buildUIntData
  :: forall n l c
   . (KnownUIntData n l, Symbolic c)
  => UIntBuilder l c -> UIntData n l c
buildUIntData f = case f (value @(Div n l), value @(Mod n l)) of
  MkSomeUInt (h :: Register r c) l ->
    case V.toVector l of
      Just low -> MkUIntData (resizeR h) low
      Nothing ->
        MkUIntData zero $ V.unfold (fromMaybe (zero, []) . uncons) (resizeR h : l)

resizeUIntData
  :: forall n l m k c
   . (Symbolic c, KnownUIntData m k, KnownUIntData n l)
  => UIntData m k c -> UIntData n l c
resizeUIntData uintData =
  buildUIntData $
    appendUIntData uintData $
      \_ -> MkSomeUInt (zero :: Register 0 c) []

concatUIntData
  :: (KnownUIntData m k, KnownUIntData n l, KnownUIntData (m + n) l, Symbolic c)
  => UIntData m k c -> UIntData n l c -> UIntData (m + n) l c
concatUIntData hi lo = buildUIntData $ appendUIntData hi (uintBuilder lo)

shiftLUIntData
  :: forall n m k c
   . (KnownUIntData m k, KnownUIntData n k, KnownUIntData (m + n) k, Symbolic c)
  => UIntData m k c -> UIntData (m + n) k c
shiftLUIntData = (`concatUIntData` zero)

shiftRUIntData
  :: forall n m k c
   . (KnownUIntData m k, KnownUIntData n k, KnownUIntData (m + n) k, Symbolic c)
  => UIntData (m + n) k c -> UIntData m k c
shiftRUIntData (MkUIntData hi lo) =
  withDict (summon @((NConst n :+ (NConst k :-! NConst 1)) :/ NConst k)) $
    let (cmp :: Maybe (Dict (CeilDiv n k <= Div (m + n) k))) =
          case cmpNat @(CeilDiv n k) @(Div (m + n) k) Proxy Proxy of
            LTI -> Just Dict
            EQI -> Just Dict
            GTI -> Nothing
     in case cmp of
          Just Dict ->
            withDict (modBound @n @k) $
              withDict (minusNat @k @(Mod n k)) $
                let ( lo' :: Vector (Div (m + n) k - CeilDiv n k) (Register k c)
                      , V.head @(CeilDiv n k) -> (r0 :: Register k c)
                      ) =
                        V.splitAt lo
                          \\ minusNat @(Div (m + n) k) @(CeilDiv n k)
                          \\ plusMinusInverse3 @(CeilDiv n k) @(Div (m + n) k)
                    (_ :: Register (Mod n k) c, r :: Register (k - Mod n k) c) =
                      splitR r0
                        \\ plusCommutes @(Mod n k) @(k - Mod n k)
                        \\ plusMinusInverse3 @(Mod n k) @k
                 in buildUIntData $
                      appendReg hi $
                        foldr appendReg (regToBuilder r) lo'
          Nothing ->
            withDict (unsafeAxiom :: Dict (Mod n k <= Mod (m + n) k)) $
              withDict (minusNat @(Mod (m + n) k) @(Mod n k)) $
                let ( _ :: Register (Mod n k) c
                      , r :: Register (Mod (m + n) k - Mod n k) c
                      ) =
                        splitR hi
                          \\ plusCommutes @(Mod n k) @(Mod (m + n) k - Mod n k)
                          \\ plusMinusInverse3 @(Mod n k) @(Mod (m + n) k)
                 in buildUIntData (regToBuilder r)

shiftUIntData
  :: forall n k c
   . (KnownUIntData n k, Symbolic c)
  => UIntData n k c -> Integer -> UIntData n k c
shiftUIntData x s
  | s == 0 = x
  | s P.> 0 =
      withSomeSNat (naturalFromInteger s) \(SNat :: SNat s) ->
        resizeUIntData (shiftLUIntData @s x)
          \\ knownUIntData @s @k
          \\ knownUIntData @(n + s) @k
          \\ plusNat @n @s
  | otherwise =
      withSomeSNat (naturalFromInteger (-s)) \(SNat :: SNat s) ->
        case cmpNat @s @n Proxy Proxy of
          LTI ->
            resizeUIntData (shiftRUIntData @s @(n - s) x)
              \\ knownUIntData @s @k
              \\ knownUIntData @(n - s) @k
              \\ minusNat @n @s
              \\ plusMinusInverse3 @s @n
          EQI -> zero
          GTI -> zero

divModProp :: forall n k. (1 <= k) :- (n ~ Mod n k + Div n k * k)
divModProp =
  euclideanNat @k @n
    \\ plusCommutes @(Mod n k) @(Div n k * k)
    \\ timesCommutes @k @(Div n k)

beBSToUIntData
  :: forall n k c
   . (KnownUIntData n k, Symbolic c)
  => ByteString n c -> UIntData n k c
beBSToUIntData =
  (\(hi, lo) -> MkUIntData (beBSToReg hi) (beBSToReg <$> toWords lo))
    . withDict (divModProp @n @k) split

uintDataToBSbe
  :: forall n k c
   . (KnownUIntData n k, Symbolic c)
  => UIntData n k c -> ByteString n c
uintDataToBSbe (MkUIntData hi lo) =
  withDict (divModProp @n @k) $
    append (regToBSbe hi) $
      concat (regToBSbe <$> lo)

uintDataToFE :: (KnownNat k, Symbolic c) => UIntData n k c -> FieldElement c
uintDataToFE (MkUIntData hi lo) = regToFE $ appendConcatR (V.reverse lo) hi

data SomeRegister c = forall r. KnownNat r => MkSomeRegister (Register r c)

strictFEToUIntData
  :: forall n k c
   . (Symbolic c, KnownUIntData n k)
  => FieldElement c -> UIntData n k c
strictFEToUIntData (MkSomeRegister . feToReg -> r) =
  case runBState (MkUIntData <$> step <*> V.replicateM step) r of
    (result, MkSomeRegister carry) -> assert (\_ -> carry == zero) result
 where
  step :: forall r. KnownNat r => BState (SomeRegister c) (Register r c)
  step = MkBState \(MkSomeRegister (q :: Register q c)) ->
    case cmpNat @r @q Proxy Proxy of
      GTI -> (resizeR q, MkSomeRegister @_ @0 zero)
      EQI -> (q, MkSomeRegister @_ @0 zero)
      LTI ->
        MkSomeRegister @_ @(q - r) <$> splitR q
          \\ minusNat @q @r
          \\ plusCommutes @r @(q - r)
          \\ plusMinusInverse3 @r @q
