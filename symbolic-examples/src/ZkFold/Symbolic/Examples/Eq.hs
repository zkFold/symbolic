{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.Eq (exampleEq, exampleEqVector) where

import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import GHC.Generics ((:.:) (..))

exampleEq :: Symbolic c => FieldElement c -> FieldElement c -> Bool c
exampleEq x y = x == y

exampleEqVector
  :: forall n c
   . Symbolic c
  => (Vector n :.: FieldElement) c
  -> (Vector n :.: FieldElement) c
  -> Bool c
exampleEqVector (Comp1 x) (Comp1 y) = x == y
