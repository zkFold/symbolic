{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.Register where

import Control.Applicative (pure, (<*>))
import Control.Monad (sequence)
import Control.Monad.State (MonadState (state), evalState, gets)
import Data.Bifunctor (Bifunctor (first))
import Data.Constraint (unmapDict, withDict, (:-), (\\))
import Data.Constraint.Nat (minusNat, plusCommutes, plusMinusInverse3, timesOne)
import Data.Constraint.Unsafe (unsafeAxiom)
import Data.Foldable (foldr)
import Data.Function (const, ($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Proxy (Proxy (..))
import Data.Tuple (fst, snd)
import Data.Type.Ord (OrderingI (..), type (>=))
import GHC.Generics (Generic1, Par1 (..), type (:*:) (..), type (:.:) (..))
import GHC.TypeNats (cmpNat)
import Test.QuickCheck (Arbitrary (..), chooseInteger)
import Text.Show (Show)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (Conditional (..), ifThenElse)
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq, (==))
import ZkFold.Data.Product (toPair)
import ZkFold.Data.Vector (Vector, item, singleton, unfold, zipWith)
import ZkFold.Symbolic.Boot (FieldElement (..))
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool (..), assert)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)
import ZkFold.Symbolic.Data.Witness (Witness (..))
import GHC.Stack (HasCallStack)

newtype Register (n :: Natural) c = MkRegister {fromRegister :: c}
  deriving stock (Functor, Generic1, Show)
  deriving anyclass SymbolicData
  deriving newtype Zero
  deriving (Collect (ConstrainedDatum c), Eq) via (FieldElement c)

instance KnownNat n => SymbolicInput (Register n) where
  isValid (MkRegister x) =
    let base = 2 ^ value @n
        bound = base -! 1
        m = fromConstant (toIntegral x `mod` fromConstant base)
     in constrain (m !<= bound) <$> (FieldElement x == FieldElement m)

instance (KnownNat n, Symbolic c) => Arbitrary (Register n c) where
  arbitrary = MkRegister . fromConstant <$> chooseInteger (0, 2 ^ value @n - 1)

instance (KnownNat n, Symbolic c) => Ord (Register n c) where
  type OrderingOf (Register n c) = Ordering c
  compare (MkRegister x) (MkRegister y) =
    let bound = 2 ^ (value @n + 1) -! 1
        res = unconstrainedCompare x y
        rfe = FieldElement (fromOrdering res)
        diff = FieldElement x - FieldElement y
     in assert isValid $
          constrain (($ fromBool (diff == zero)) * ($ fromOrdering res) =!= zero)
            . constrain (fromFieldElement ((rfe - one) * diff) !<= bound)
            . constrain (fromFieldElement ((rfe + one) * diff) !<= bound)
            <$> res

extendR :: forall n m c. Register m c -> Register (m + n) c
extendR = MkRegister . fromRegister

regToFE :: Register n c -> FieldElement c
regToFE = FieldElement . fromRegister

constant
  :: forall n c
   . (KnownNat n, FromConstant Natural c) => Register (Log2 n + 1) c
constant = MkRegister $ fromConstant (value @n)

adder
  :: Symbolic c => Register n c -> Register n c -> Bool c -> Register (n + 1) c
adder (regToFE -> x) (regToFE -> y) (Bool (FieldElement -> z)) =
  MkRegister $ fromFieldElement (x + y + z)

(.+.) :: Symbolic c => Register n c -> Register n c -> Register (n + 1) c
x .+. y = adder x y false

-- NOTE correct iff first operand is not less than second
(.-.) :: Symbolic c => Register n c -> Register n c -> Register n c
(regToFE -> x) .-. (regToFE -> y) = MkRegister $ fromFieldElement (x - y)

(.*.) :: Symbolic c => Register m c -> Register n c -> Register (m + n) c
(regToFE -> x) .*. (regToFE -> y) = MkRegister $ fromFieldElement (x * y)

divModR
  :: (Symbolic c, KnownNat m, KnownNat n)
  => Register (n + m + 1) c -> Register n c -> (Register m c, Register n c)
divModR x y =
  toPair $
    assert (\p@(d :*: m) -> isValid p && x == (y .*. d) .+. extendR m) $
      (\(d, m) -> fromInt d :*: fromInt m) $
        divMod (toInt x) (toInt y)
 where
  toInt = toIntegral . fromRegister
  fromInt :: FromConstant a c => a -> Register r c
  fromInt = MkRegister . fromConstant

divR
  :: (Symbolic c, KnownNat m, KnownNat n)
  => Register (n + m + 1) c -> Register n c -> Register m c
divR x y = fst (divModR x y)

modR
  :: (Symbolic c, KnownNat m, KnownNat n)
  => Register (n + m + 1) c -> Register n c -> Register n c
modR x y = snd (divModR x y)

shlR
  :: forall m n c
   . (KnownNat m, Symbolic c)
  => Register n c -> Register (m + n) c
shlR (regToFE -> x) =
  MkRegister $ fromFieldElement (((2 :: Natural) ^ value @m) `scale` x)

negateR
  :: forall n c
   . (KnownNat n, Symbolic c)
  => Register n c -> (Register n c, Bool c)
negateR (regToFE -> x) =
  let isZ = x == zero
   in (,isZ) $
        MkRegister $
          fromFieldElement $
            ifThenElse isZ x $
              fromConstant ((2 :: Natural) ^ value @n) - x

regsToIntegral
  :: forall m n k c
   . (KnownNat n, PrimeField c)
  => Vector m (Register n c) -> Register k c -> IntegralOf c
regsToIntegral
  (fmap (toIntegral . fromRegister) -> xs)
  (toIntegral . fromRegister -> y) =
    foldr (\r s -> r + scale ((2 :: Natural) ^ value @n) s) y xs

appendConcatR
  :: forall m n k c
   . (KnownNat n, Symbolic c)
  => Vector m (Register n c) -> Register k c -> Register (m * n + k) c
appendConcatR xs (regToFE -> y) =
  MkRegister $
    fromFieldElement $
      foldr (\r s -> regToFE r + scale ((2 :: Natural) ^ value @n) s) y xs

appendR
  :: forall m n c
   . (KnownNat m, Symbolic c)
  => Register m c -> Register n c -> Register (m + n) c
appendR = appendConcatR . singleton

concatR
  :: forall m n c
   . (KnownNat n, Symbolic c)
  => Vector m (Register n c) -> Register (m * n) c
concatR xs = appendConcatR xs (zero :: Register 0 c)

limb
  :: forall a n c
   . (SemiEuclidean a, KnownNat n, FromConstant a c)
  => a -> (Register n c, a)
limb x =
  let (d, m) = x `divMod` fromConstant ((2 :: Natural) ^ value @n)
   in (MkRegister (fromConstant m), d)

splitChunksR
  :: (KnownNat m, KnownNat n, KnownNat k, Symbolic c, HasCallStack)
  => Register (m * n + k) c -> (Vector m (Register n c), Register k c)
splitChunksR x =
  (\(Comp1 lo :*: hi) -> (lo, hi))
    $ assert (\v@(Comp1 lo :*: hi) -> isValid v && appendConcatR lo hi == x)
    $ evalState
      ( (:*:) . Comp1
          <$> sequence (pure $ state limb)
          <*> gets (MkRegister . fromConstant)
      )
    $ toIntegral (fromRegister x)

splitR
  :: (KnownNat m, KnownNat n, Symbolic c)
  => Register (m + n) c -> (Register m c, Register n c)
splitR = first item . splitChunksR

chunksR
  :: (KnownNat m, KnownNat n, Symbolic c)
  => Register (m * n) c -> Vector m (Register n c)
chunksR = fst . splitChunksR @_ @_ @0

ordSym :: forall m n. (m >= n) :- (n <= m)
ordSym = unmapDict (const unsafeAxiom) -- that's a pity

resizeR
  :: forall n m c
   . (KnownNat m, KnownNat n, Symbolic c)
  => Register m c -> Register n c
resizeR r = case cmpNat @m @n Proxy Proxy of
  LTI ->
    extendR @(n - m) r
      \\ plusCommutes @m @(n - m)
      \\ plusMinusInverse3 @m @n
  EQI -> r
  GTI ->
    fst (splitR @n @(m - n) r)
      \\ plusMinusInverse3 @n @m
      \\ plusCommutes @n @(m - n)
      \\ minusNat @m @n
      \\ ordSym @m @n

regToBool :: Register 1 c -> Bool c
regToBool = Bool . fromRegister

boolToReg :: Bool c -> Register 1 c
boolToReg = MkRegister . fromBool

fromBinaryR :: forall n c. Symbolic c => Vector n (Bool c) -> Register n c
fromBinaryR = withDict (timesOne @n) $ concatR . fmap boolToReg

bitsOfR
  :: forall n c. (KnownNat n, Symbolic c) => Register n c -> Vector n (Bool c)
bitsOfR = withDict (timesOne @n) $ fmap regToBool . chunksR

feToReg :: FieldElement c -> Register (NumberOfBits c) c
feToReg = MkRegister . fromFieldElement

bitsOfFE :: Symbolic c => FieldElement c -> Vector (NumberOfBits c) (Bool c)
bitsOfFE = bitsOfR . feToReg

andXor
  :: forall n c
   . (KnownNat n, Symbolic c)
  => Register n c -> Register n c -> (Register n c, Register n c)
andXor q r =
  let qb = bitsOfR (Witness <$> q)
      rb = bitsOfR (Witness <$> r)
   in toPair
        $ assert
          ( \((comb -> ca) :*: (comb -> cx)) ->
              withDict (plusCommutes @(2 * n - 1) @1) $
                extendR cx .+. shlR @1 ca == extendR (comb q .+. comb r)
          )
        $ fmap witness
        $ fromBinaryR (zipWith (&&) qb rb) :*: fromBinaryR (zipWith xor qb rb)
 where
  comb :: Register n c -> Register (2 * n - 1) c
  comb =
    MkRegister
      . unPar1
      . plot
        ( Par1
            . witness
            . fromRegister
            . concatR @n @2
            . fmap (extendR @1)
            . unfold limb
            . toIntegral
            . unPar1
        )
        (rangeTable $ 2 ^ value @n)
      . Par1
      . fromRegister

instance (KnownNat n, Symbolic c) => Conditional (Register n c) (Register n c) where
  bool f t c = MkRegister $ fromRegister ((c && t) .+. not (c || not f))

instance (KnownNat n, Symbolic c) => BoolType (Register n c) where
  false = zero
  true = MkRegister $ fromConstant ((2 :: Natural) ^ value @n -! 1)
  not (regToFE -> x) =
    MkRegister $ fromFieldElement (regToFE (true @(Register n c)) - x)
  x && y = fst (andXor x y)
  x `xor` y = snd (andXor x y)
  q || r = let (a, x) = andXor q r in MkRegister $ fromRegister (a .+. x)
