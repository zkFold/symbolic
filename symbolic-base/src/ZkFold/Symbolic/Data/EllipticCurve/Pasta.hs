{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point, Vesta_Point) where

import           Prelude                                 (fromInteger, type (~), ($))
import qualified Prelude

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA

type Pallas_Point ctx =
  Weierstrass "Pasta" (Point (FFA FpModulus 'Auto ctx))

type Vesta_Point ctx =
  Weierstrass "Pasta" (Point (FFA FqModulus 'Auto ctx))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize FqModulus (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA FpModulus 'Auto ctx
  , KnownFFA FqModulus 'Auto ctx
  ) => CyclicGroup (Pallas_Point ctx) where
  type ScalarFieldOf (Pallas_Point ctx) = FFA FqModulus 'Auto ctx
  pointGen = pointXY
    (fromConstant (0x40000000000000000000000000000000224698fc094cf91b992d30ed00000000 :: Natural))
    (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize FqModulus (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA FpModulus 'Auto ctx
  , KnownFFA FqModulus 'Auto ctx
  ) => Scale (FFA FqModulus 'Auto ctx) (Pallas_Point ctx) where

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
  , uintBits ~ FFAUIntSize FpModulus (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA FqModulus 'Auto ctx
  , KnownFFA FpModulus 'Auto ctx
  ) => CyclicGroup (Vesta_Point ctx) where
  type ScalarFieldOf (Vesta_Point ctx) = FFA FpModulus 'Auto ctx
  pointGen = pointXY
    (fromConstant (0x40000000000000000000000000000000224698fc094cf91b992d30ed00000000 :: Natural))
    (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , nativeBits ~ NumberOfBits a
  , uintBits ~ FFAUIntSize FpModulus (Order a)
  , KnownNat (nativeBits + uintBits)
  , KnownFFA FqModulus 'Auto ctx
  , KnownFFA FpModulus 'Auto ctx
  ) => Scale (FFA FpModulus 'Auto ctx) (Vesta_Point ctx) where

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
