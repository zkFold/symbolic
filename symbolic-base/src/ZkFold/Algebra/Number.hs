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
  KnownPrime,
  IsPrime,
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

import Data.Bool (Bool (..))
import Data.Kind (Constraint)
import Data.Type.Bool (If, type (&&))
import GHC.Exts (proxy#)
import GHC.Real (Integral)
import qualified GHC.Real as Integral
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeNats

-- Use orphan instances for large publicly verified primes
class KnownNat p => Prime p

value :: forall n. KnownNat n => Natural
value = natVal' (proxy# @n)

integral :: forall size n. (KnownNat size, Integral n) => n
integral = Integral.fromIntegral (value @size)

-- Use this overlappable instance for small enough primes and testing
instance {-# OVERLAPPABLE #-} (KnownNat p, KnownPrime p) => Prime p

type family KnownPrime p where
  KnownPrime p = If (IsPrime p) (() :: Constraint) (TypeError (NotPrimeError p))

type NotPrimeError p =
  'Text "Error: " ':<>: 'ShowType p ':<>: 'Text " is not a prime number."

type family IsPrime p where
  IsPrime 0 = 'False
  IsPrime 1 = 'False
  IsPrime 2 = 'True
  IsPrime 3 = 'True
  IsPrime n = NotDividesFromTo n 2 (AtLeastSqrt n)

type family NotZero n where
  NotZero 0 = 'False
  NotZero n = 'True

type family NotDividesFromTo dividend divisor0 divisor1 where
  NotDividesFromTo dividend divisor divisor = NotZero (dividend `Mod` divisor)
  NotDividesFromTo dividend divisor0 divisor1 =
    NotZero (dividend `Mod` divisor0) && NotDividesFromTo dividend (divisor0 + 1) divisor1

type family AtLeastSqrt n where
  AtLeastSqrt 0 = 0
  AtLeastSqrt n = 2 ^ (Log2 n `Div` 2 + 1)
