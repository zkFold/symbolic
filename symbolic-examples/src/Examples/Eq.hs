module Examples.Eq (exampleEq, exampleEqVector) where

import           ZkFold.Base.Algebra.Basic.Number  (KnownNat)
import           ZkFold.Base.Data.Vector           (Vector)
import           ZkFold.Symbolic.Class             (Symbolic)
import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.Eq           (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)

exampleEq :: Symbolic c => FieldElement c -> FieldElement c -> Bool c
exampleEq x y = x == y

exampleEqVector :: (KnownNat n, Symbolic c)
    => Vector n (FieldElement c)
    -> Vector n (FieldElement c)
    -> Bool c
exampleEqVector x y = x == y
