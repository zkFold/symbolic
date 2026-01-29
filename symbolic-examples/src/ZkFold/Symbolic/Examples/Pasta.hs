module ZkFold.Symbolic.Examples.Pasta (
  examplePallas_Add,
  examplePallas_Scale,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import ZkFold.Symbolic.Data.FFA

examplePallas_Add
  :: (Symbolic ctx, KnownFFA FpModulus ctx)
  => Pallas_Point ctx -> Pallas_Point ctx -> Pallas_Point ctx
examplePallas_Add = (+)

examplePallas_Scale
  :: (Symbolic ctx, KnownFFA FpModulus ctx, KnownFFA FqModulus ctx)
  => ScalarFieldOf (Pallas_Point ctx) -> Pallas_Point ctx -> Pallas_Point ctx
examplePallas_Scale = scale
