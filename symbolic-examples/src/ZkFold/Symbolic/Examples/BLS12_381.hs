module ZkFold.Symbolic.Examples.BLS12_381 (
  exampleBLS12_381Scale,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Symbolic.Data.FFA

exampleBLS12_381Scale
  :: ( Symbolic ctx
     , KnownFFA BLS12_381_Base ctx
     , KnownFFA BLS12_381_Scalar ctx
     )
  => ScalarFieldOf (BLS12_381_G1_Point ctx)
  -> BLS12_381_G1_Point ctx
  -> BLS12_381_G1_Point ctx
exampleBLS12_381Scale = scale
