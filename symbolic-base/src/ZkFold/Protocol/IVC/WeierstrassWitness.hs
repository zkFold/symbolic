{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.WeierstrassWitness where

import qualified Data.Bool as Haskell
import qualified Data.Eq as Haskell
import Data.Foldable (toList)
import Data.Function (($), (.))
import Data.String (fromString)
import GHC.Generics (Generic, Par1 (..), U1 (..), unPar1)
import ZkFold.Algebra.Class (
  AdditiveGroup (..),
  AdditiveMonoid (..),
  AdditiveSemigroup (..),
  MultiplicativeMonoid (..),
  Scale (..),
  SemiEuclidean (..),
  fromConstant,
  (*),
 )
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..), Planar, Point (..), Weierstrass (..), pointXY)
import ZkFold.Algebra.Number (Natural, value)
import ZkFold.Control.Conditional (Conditional (..), ifThenElse)
import ZkFold.Control.HApplicative (HApplicative (hunit))
import ZkFold.Data.Bool (BoolType)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Vector (Vector, unsafeToVector, (!!))
import ZkFold.Protocol.IVC.ForeignField (ForeignField)
import ZkFold.Protocol.IVC.Oracle (OracleSource (..))
import ZkFold.Symbolic.Class (Symbolic (..), embedW)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.MonadCircuit (IntegralOf, MonadCircuit (unconstrained), ResidueField (..))
import Prelude (Integer, Traversable (traverse), error, fromInteger, type (~))

newtype WeierstrassWitness ctx
  = WeierstrassWitness (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  deriving Generic

instance
  (BooleanOf (IntegralOf (WitnessField ctx)) ~ Haskell.Bool, Haskell.Eq (IntegralOf (WitnessField ctx)))
  => Haskell.Eq (WeierstrassWitness ctx)
  where
  WeierstrassWitness p1 == WeierstrassWitness p2 = p1 Haskell.== p2
  WeierstrassWitness p1 /= WeierstrassWitness p2 = p1 Haskell./= p2

instance
  ( Symbolic ctx
  , Conditional b (IntegralOf (WitnessField ctx))
  , Conditional b (Weierstrass "BLS12-381-G1" (Point (ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx)))))
  , Conditional b b
  , b ~ BooleanOf (IntegralOf (WitnessField ctx))
  , Scale Natural (WeierstrassWitness ctx)
  )
  => SymbolicData (WeierstrassWitness ctx)
  where
  type Context (WeierstrassWitness ctx) = ctx
  type Layout (WeierstrassWitness ctx) = Vector 5
  type Payload (WeierstrassWitness ctx) = U1
  arithmetize (WeierstrassWitness (Weierstrass (Point a b isInf))) =
    let a1 = fromIntegral $ toIntegral a `mod` fromConstant (value @BLS12_381_Scalar)
        a2 = fromIntegral $ toIntegral a `div` fromConstant (value @BLS12_381_Scalar)
        b1 = fromIntegral $ toIntegral b `mod` fromConstant (value @BLS12_381_Scalar)
        b2 = fromIntegral $ toIntegral b `div` fromConstant (value @BLS12_381_Scalar)
        isInf1 = fromIntegral $ bool zero one isInf
     in fromCircuitF hunit $ \_ -> traverse unconstrained (unsafeToVector @5 [a1, a2, b1, b2, isInf1])

  payload _ = U1

  restore (l, _) =
    let v = witnessF l
     in WeierstrassWitness
          ( Weierstrass
              ( Point
                  ( fromIntegral $ toIntegral (v !! 0) + toIntegral (v !! 1) * fromConstant (value @BLS12_381_Scalar)
                  )
                  ( fromIntegral $ toIntegral (v !! 2) + toIntegral (v !! 3) * fromConstant (value @BLS12_381_Scalar)
                  )
                  (fromIntegral @(ForeignField BLS12_381_Base (IntegralOf (WitnessField ctx))) (toIntegral $ v !! 4) == one)
              )
          )
  interpolate _ _ = error "Interpolation is not defined for WeierstrassWitness"

instance (SymbolicData (WeierstrassWitness ctx), w ~ WitnessField ctx) => OracleSource w (WeierstrassWitness ctx) where
  source = toList . payload

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
  )
  => CyclicGroup (WeierstrassWitness ctx)
  where
  type
    ScalarFieldOf (WeierstrassWitness ctx) =
      FieldElement ctx
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
  , w ~ FieldElement ctx
  , n ~ IntegralOf w
  , Scale Natural (WeierstrassWitness ctx)
  )
  => Scale w (WeierstrassWitness ctx)
  where
  scale f x =
    if n == zero
      then zero
      else
        scale f' (x + x)
          + if n `mod` two == zero
            then zero
            else x
   where
    two = one + one
    n = toIntegral $ unPar1 $ witnessF $ fromFieldElement f
    n' = n `div` two
    f' = FieldElement $ embedW $ Par1 $ fromIntegral n'
