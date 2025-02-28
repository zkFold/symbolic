module Examples.BLS12_381 (
    exampleBLS12_381Scale
  ) where

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base)
import           ZkFold.Symbolic.Class                       (Symbolic)
import           ZkFold.Symbolic.Data.BLS12_381              (BLS12_381_G1_Point)
import           ZkFold.Symbolic.Data.Combinators            (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.FFA                    (KnownFFA)
import           ZkFold.Symbolic.Data.FieldElement

exampleBLS12_381Scale
    :: (Symbolic c, KnownFFA BLS12_381_Base Auto c)
    => FieldElement c
    -> BLS12_381_G1_Point c
    -> BLS12_381_G1_Point c
exampleBLS12_381Scale = scale
