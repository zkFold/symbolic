{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Pasta where

import           Prelude                                 (type (~), ($))
import qualified Prelude                                 as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.EllipticCurve.Class
import qualified ZkFold.Base.Algebra.EllipticCurve.Pasta as Base
import           ZkFold.Symbolic.Class                   (Symbolic (..))
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString
import           ZkFold.Symbolic.Data.Combinators        (RegisterSize (..))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA                (FFA, KnownFFA)

type PallasPoint ctx = Base.Pasta_Point (FFA Base.FpModulus (Fixed 1) ctx)

instance (Symbolic ctx, KnownFFA Base.FpModulus (Fixed 1) ctx, KnownFFA Base.FqModulus (Fixed 1) ctx) => CyclicGroup (PallasPoint ctx) where
  type ScalarFieldOf (PallasPoint ctx) = FFA Base.FqModulus (Fixed 1) ctx
  pointGen = pointXY
    (fromConstant (0x40000000000000000000000000000000224698fc094cf91b992d30ed00000000 :: Natural))
    (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , bits ~ NumberOfBits a
  , KnownFFA Base.FpModulus (Fixed 1) ctx
  , KnownFFA Base.FqModulus (Fixed 1) ctx
  ) => Scale (FFA Base.FqModulus (Fixed 1) ctx) (PallasPoint ctx) where

    scale sc x = sum $ P.zipWith (\b p -> bool @(Bool ctx) zero p (isSet bits b)) [upper, upper -! 1 .. 0] (P.iterate (\e -> e + e) x)
        where
            bits :: ByteString 255 ctx
            bits = binaryExpansion sc

            upper :: Natural
            upper = value @bits -! 1

type VestaPoint ctx = Base.Pasta_Point (FFA Base.FqModulus (Fixed 1) ctx)

instance (Symbolic ctx, KnownFFA Base.FpModulus (Fixed 1) ctx, KnownFFA Base.FqModulus (Fixed 1) ctx) => CyclicGroup (VestaPoint ctx) where
  type ScalarFieldOf (VestaPoint ctx) = FFA Base.FpModulus (Fixed 1) ctx
  pointGen = pointXY
    (fromConstant (0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000000 :: Natural))
    (fromConstant (0x02 :: Natural))

instance
  ( Symbolic ctx
  , a ~ BaseField ctx
  , bits ~ NumberOfBits a
  , KnownFFA Base.FpModulus (Fixed 1) ctx
  , KnownFFA Base.FqModulus (Fixed 1) ctx
  ) => Scale (FFA Base.FpModulus (Fixed 1) ctx) (VestaPoint ctx) where

    scale sc x = sum $ P.zipWith (\b p -> bool @(Bool ctx) zero p (isSet bits b)) [upper, upper -! 1 .. 0] (P.iterate (\e -> e + e) x)
        where
            bits :: ByteString 255 ctx
            bits = binaryExpansion sc

            upper :: Natural
            upper = value @bits -! 1