{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Algebra.Number (
  Natural,
  KnownNat,
  Prime,
  Log2,
  Mod,
  Div,
  Sqrt,
  value,
  integral,
  ilog2,
  pow2Nat,
  type (<=),
  type (*),
  type (+),
  type (-),
  type (^),
) where

import Data.Constraint (Dict (Dict), (:-) (Sub))
import Data.Constraint.Unsafe (unsafeSNat)
import Data.Function (($), (.))
import Data.Type.Bool (If)
import Data.Type.Ord (type (>?))
import GHC.Exts (proxy#)
import GHC.Natural (shiftLNatural)
import GHC.Num.Natural (naturalLog2)
import GHC.Real (Integral)
import qualified GHC.Real as Integral
import GHC.TypeNats

-- Use orphan instances for large publicly verified primes
class KnownNat p => Prime p

instance Prime 2

instance Prime 3

type family FindSqrt n x where
  FindSqrt n x = If ((x + 1) * (x + 1) >? n) x (FindSqrt n (x + 1))

type Sqrt n = FindSqrt n (2 ^ Div (Log2 n) 2)

value :: forall n. KnownNat n => Natural
value = natVal' (proxy# @n)

integral :: forall size n. (KnownNat size, Integral n) => n
integral = Integral.fromIntegral (value @size)

ilog2 :: Natural -> Natural
ilog2 = Integral.fromIntegral . naturalLog2

pow2Nat :: forall n. KnownNat n :- KnownNat (2 ^ n)
pow2Nat =
  Sub $ withKnownNat @(2 ^ n) (unsafeSNat $ shiftLNatural 1 $ integral @n) Dict
