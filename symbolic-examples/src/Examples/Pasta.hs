{-# LANGUAGE TypeOperators #-}

module Examples.Pasta (
    examplePallas_Add
  , examplePallas_Scale
  ) where

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Pasta  (FpModulus, FqModulus)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Combinators         (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import           ZkFold.Symbolic.Data.FFA

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
