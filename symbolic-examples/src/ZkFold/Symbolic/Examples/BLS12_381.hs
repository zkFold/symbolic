module ZkFold.Symbolic.Examples.BLS12_381 (
  exampleBLS12_381Scale,
) where

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.V2 (Symbolic)

exampleBLS12_381Scale
  :: ( Symbolic ctx
     , KnownFFA BLS12_381_Base 'Auto ctx
     , KnownFFA BLS12_381_Scalar 'Auto ctx
     )
  => ScalarFieldOf (BLS12_381_G1_Point ctx)
  -> BLS12_381_G1_Point ctx
  -> BLS12_381_G1_Point ctx
exampleBLS12_381Scale = scale
