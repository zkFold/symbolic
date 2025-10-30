{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.Eq (exampleEq, exampleEqVector) where

import GHC.Generics ((:.:) (..))
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)
import ZkFold.Symbolic.Compat (CompatData)

exampleEq
  :: Symbolic c
  => CompatData FieldElement c -> CompatData FieldElement c -> CompatData Bool c
exampleEq x y = x == y

exampleEqVector
  :: forall n c
   . Symbolic c
  => (Vector n :.: CompatData FieldElement) c
  -> (Vector n :.: CompatData FieldElement) c
  -> CompatData Bool c
exampleEqVector (Comp1 x) (Comp1 y) = x == y
