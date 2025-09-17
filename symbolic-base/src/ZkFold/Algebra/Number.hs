{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
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
  type (<=),
  type (*),
  type (+),
  type (-),
  type (^),
) where

import GHC.Exts (proxy#)
import GHC.Real (Integral)
import qualified GHC.Real as Integral
import GHC.TypeNats

-- Use orphan instances for large publicly verified primes
class KnownNat p => Prime p

value :: forall n. KnownNat n => Natural
value = natVal' (proxy# @n)

integral :: forall size n. (KnownNat size, Integral n) => n
integral = Integral.fromIntegral (value @size)

instance Prime 2
instance Prime 3
