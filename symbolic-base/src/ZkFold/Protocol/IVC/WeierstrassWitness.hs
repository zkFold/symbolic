{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.WeierstrassWitness where

import Data.Function (($))
import GHC.Generics (Generic)
import ZkFold.Algebra.Class (
  AdditiveGroup (..),
  AdditiveMonoid (..),
  AdditiveSemigroup (..),
  MultiplicativeMonoid (..),
  Scale (..),
  SemiEuclidean (..),
  fromConstant,
 )
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..), Planar, Point, Weierstrass, pointXY)
import ZkFold.Algebra.Number (Natural)
import ZkFold.Control.Conditional (Conditional (..), ifThenElse)
import ZkFold.Data.Bool (BoolType)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Protocol.IVC.ForeignField (ForeignField)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.MonadCircuit (IntegralOf, ResidueField (..))
import Prelude (Integer, fromInteger, type (~))

newtype WeierstrassWitness ctx
  = WeierstrassWitness (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  deriving Generic

instance
  ( Symbolic ctx
  , BoolType (BooleanOf (IntegralOf (WitnessField ctx)))
  , Conditional (BooleanOf (IntegralOf (WitnessField ctx))) (BooleanOf (IntegralOf (WitnessField ctx)))
  , Conditional (BooleanOf (IntegralOf (WitnessField ctx))) (IntegralOf (WitnessField ctx))
  )
  => Eq (WeierstrassWitness ctx)
  where
  type
    BooleanOf (WeierstrassWitness ctx) =
      BooleanOf (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  WeierstrassWitness p1 == WeierstrassWitness p2 = p1 == p2
  WeierstrassWitness p1 /= WeierstrassWitness p2 = p1 /= p2

instance
  ( Symbolic ctx
  , BoolType b
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  )
  => Conditional b (WeierstrassWitness ctx)
  where
  bool (WeierstrassWitness p1) (WeierstrassWitness p2) b =
    WeierstrassWitness (bool p1 p2 b)

instance
  ( Symbolic ctx
  , Conditional b (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  )
  => AdditiveSemigroup (WeierstrassWitness ctx)
  where
  WeierstrassWitness p1 + WeierstrassWitness p2 =
    WeierstrassWitness (p1 + p2)

instance
  ( Symbolic ctx
  , Conditional b (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , Conditional b b
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  , Scale Natural (WeierstrassWitness ctx)
  )
  => AdditiveMonoid (WeierstrassWitness ctx)
  where
  zero = WeierstrassWitness zero

instance
  ( Symbolic ctx
  , Conditional (BooleanOf (IntegralOf (WitnessField ctx))) (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , Conditional b b
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  , Scale Natural (WeierstrassWitness ctx)
  , Scale Integer (WeierstrassWitness ctx)
  )
  => AdditiveGroup (WeierstrassWitness ctx)
  where
  negate (WeierstrassWitness p) =
    WeierstrassWitness (negate p)

instance
  {-# OVERLAPPING #-}
  ( Symbolic ctx
  , Conditional (BooleanOf (IntegralOf (WitnessField ctx))) (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , Conditional b b
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  )
  => Scale Natural (WeierstrassWitness ctx)
  where
  scale n (WeierstrassWitness p) =
    WeierstrassWitness (scale n p)

instance
  {-# OVERLAPPING #-}
  ( Symbolic ctx
  , Conditional (BooleanOf (IntegralOf (WitnessField ctx))) (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , Conditional b b
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  )
  => Scale Integer (WeierstrassWitness ctx)
  where
  scale n (WeierstrassWitness p) =
    WeierstrassWitness (scale n p)

instance
  ( Symbolic ctx
  , f ~ ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx))
  )
  => Planar f (WeierstrassWitness ctx)
  where
  pointXY x y =
    WeierstrassWitness (pointXY x y)

instance
  ( Symbolic ctx
  , Conditional (BooleanOf (IntegralOf (WitnessField ctx))) (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , Conditional b b
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  , Scale Natural (WeierstrassWitness ctx)
  , Scale Integer (WeierstrassWitness ctx)
  , Scale (WitnessField ctx) (WeierstrassWitness ctx)
  )
  => CyclicGroup (WeierstrassWitness ctx)
  where
  type
    ScalarFieldOf (WeierstrassWitness ctx) =
      WitnessField ctx
  pointGen =
    pointXY @(ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))
      ( fromConstant @Natural 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
      )
      ( fromConstant @Natural 0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1
      )

instance
  ( Symbolic ctx
  , Conditional (BooleanOf (IntegralOf (WitnessField ctx))) (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , Conditional b b
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  , w ~ WitnessField ctx
  , n ~ IntegralOf w
  , Scale Natural (WeierstrassWitness ctx)
  )
  => Scale w (WeierstrassWitness ctx)
  where
  scale w p =
    if n == zero
      then zero
      else
        scale (fromIntegral @w $ n `div` two) (p + p)
          + if n `mod` two == zero
            then zero
            else p
   where
    n = toIntegral w
    two = one + one
