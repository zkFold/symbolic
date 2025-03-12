{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.PlutoEris (Pluto_Point, Eris_Point) where

import           Prelude                                     (fromInteger, type (~), ($))
import qualified Prelude

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.PlutoEris (PlutoEris_p, PlutoEris_q)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type Pluto_Point ctx =
  Weierstrass "Pluto-Eris" (Point (FFA PlutoEris_p 'Auto ctx))

type Eris_Point ctx =
  Weierstrass "Pluto-Eris" (Point (FFA PlutoEris_q 'Auto ctx))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize PlutoEris_q (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA PlutoEris_p 'Auto ctx
  , KnownFFA PlutoEris_q 'Auto ctx
  ) => CyclicGroup (Pluto_Point ctx) where
  type ScalarFieldOf (Pluto_Point ctx) = FFA PlutoEris_q 'Auto ctx
  pointGen = pointXY
    (fromConstant (-2 :: Prelude.Integer))
    (fromConstant (7 :: Prelude.Integer))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize PlutoEris_q (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA PlutoEris_p 'Auto ctx
  , KnownFFA PlutoEris_q 'Auto ctx
  ) => Scale (FFA PlutoEris_q 'Auto ctx) (Pluto_Point ctx) where

    scale (FFA nativeSc uintSc) x = sum $ Prelude.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet (nativeBits `append` uintBits) b))
      [upper, upper -! 1 .. 0]
      (Prelude.iterate (\e -> e + e) x)
        where
            nativeBits :: ByteString nativeBits ctx
            nativeBits = ByteString $ binaryExpansion nativeSc 

            uintBits :: ByteString uintBits ctx
            uintBits = from uintSc

            upper :: Natural
            upper = value @(nativeBits + uintBits) -! 1

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize PlutoEris_p (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA PlutoEris_q 'Auto ctx
  , KnownFFA PlutoEris_p 'Auto ctx
  ) => CyclicGroup (Eris_Point ctx) where
  type ScalarFieldOf (Eris_Point ctx) = FFA PlutoEris_p 'Auto ctx
  pointGen = pointXY
    (fromConstant (-2 :: Prelude.Integer))
    (fromConstant (7 :: Prelude.Integer))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize PlutoEris_p (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA PlutoEris_q 'Auto ctx
  , KnownFFA PlutoEris_p 'Auto ctx
  ) => Scale (FFA PlutoEris_p 'Auto ctx) (Eris_Point ctx) where

    scale (FFA nativeSc uintSc) x = sum $ Prelude.zipWith
      (\b p -> bool @(Bool ctx) zero p (isSet (nativeBits `append` uintBits) b))
      [upper, upper -! 1 .. 0]
      (Prelude.iterate (\e -> e + e) x)
        where
            nativeBits :: ByteString nativeBits ctx
            nativeBits = ByteString $ binaryExpansion nativeSc 

            uintBits :: ByteString uintBits ctx
            uintBits = from uintSc

            upper :: Natural
            upper = value @(nativeBits + uintBits) -! 1
