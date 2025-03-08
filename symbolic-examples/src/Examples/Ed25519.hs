module Examples.Ed25519 (
    exampleEd25519Scale
  ) where

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.EllipticCurve.Ed25519 (Ed25519_Base)
import           ZkFold.Symbolic.Class                     (Symbolic)
import           ZkFold.Symbolic.Data.Combinators          (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Ed25519
import           ZkFold.Symbolic.Data.FFA                  (KnownFFA)
import           ZkFold.Symbolic.Data.FieldElement

exampleEd25519Scale
    :: (Symbolic c, KnownFFA Ed25519_Base Auto c)
    => FieldElement c
    -> Ed25519_Point c
    -> Ed25519_Point c
exampleEd25519Scale = scale
