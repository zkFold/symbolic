module Examples.MiMCHash (exampleMiMC) where

import           ZkFold.Algebra.Class
import           ZkFold.Symbolic.Algorithm.Hash.MiMC           (mimcHash2)
import           ZkFold.Symbolic.Algorithm.Hash.MiMC.Constants (mimcConstants)
import           ZkFold.Symbolic.Class                          (Symbolic (..))
import           ZkFold.Symbolic.Data.FieldElement              (FieldElement)

exampleMiMC ::
  forall c. (Symbolic c, FromConstant (BaseField c) (FieldElement c)) =>
  FieldElement c -> FieldElement c -> FieldElement c
exampleMiMC = mimcHash2 mimcConstants (zero :: BaseField c)
