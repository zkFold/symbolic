{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
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
  value,
  integral,
  ilog2,
  pow2Nat,
  CeilDiv,
  ceilDivNat,
  type (<=),
  type (*),
  type (+),
  type (-),
  type (^),
) where

import Data.Constraint (Dict (Dict), unmapDict, (:-) (Sub))
import Data.Constraint.Unsafe (unsafeSNat)
import Data.Function (($), (.))
import GHC.Exts (proxy#)
import GHC.Natural (shiftLNatural)
import GHC.Num.Natural (naturalLog2)
import GHC.Real (Integral)
import qualified GHC.Real as Integral
import GHC.TypeNats

import ZkFold.Data.Summoner (NumExpr (..), Summon (summon))

type CeilDiv m n = (m + (n - 1)) `Div` n

ceilDivNat
  :: forall m n. (KnownNat m, KnownNat n, 1 <= n) :- KnownNat (CeilDiv m n)
ceilDivNat = unmapDict \Dict ->
  summon @((NConst m :+ (NConst n :-! NConst 1)) :/ NConst n)

-- Use orphan instances for large publicly verified primes
class KnownNat p => Prime p

instance Prime 2

instance Prime 3

value :: forall n. KnownNat n => Natural
value = natVal' (proxy# @n)

integral :: forall size n. (KnownNat size, Integral n) => n
integral = Integral.fromIntegral (value @size)

ilog2 :: Natural -> Natural
ilog2 = Integral.fromIntegral . naturalLog2

pow2Nat :: forall n. KnownNat n :- KnownNat (2 ^ n)
pow2Nat =
  Sub $ withKnownNat @(2 ^ n) (unsafeSNat $ shiftLNatural 1 $ integral @n) Dict
