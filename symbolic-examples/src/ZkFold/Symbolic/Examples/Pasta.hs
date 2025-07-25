{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.Pasta (
  examplePallas_Add,
  examplePallas_Scale,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import ZkFold.Symbolic.Data.FFA

examplePallas_Add
  :: ( Symbolic ctx
     , KnownFFA FpModulus 'Auto ctx
     )
  => Pallas_Point ctx
  -> Pallas_Point ctx
  -> Pallas_Point ctx
examplePallas_Add = (+)

examplePallas_Scale
  :: ( Symbolic ctx
     , KnownFFA FpModulus 'Auto ctx
     , KnownFFA FqModulus 'Auto ctx
     )
  => ScalarFieldOf (Pallas_Point ctx)
  -> Pallas_Point ctx
  -> Pallas_Point ctx
examplePallas_Scale = scale
