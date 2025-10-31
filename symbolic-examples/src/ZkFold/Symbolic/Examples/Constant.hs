{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.Constant (
  exampleConst,
  exampleConditionalConst,
  exampleConditionalConstConst,
  exampleEqConst,
  exampleEqVectorConst,
) where

import Control.Monad.Representable.Reader (Representable (tabulate))
import Data.Function (const, ($))
import GHC.Generics ((:.:) (Comp1))
import ZkFold.Algebra.Class (FromConstant (..))
import ZkFold.Algebra.Number (KnownNat, Natural)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)

import ZkFold.Symbolic.Examples.Conditional (exampleConditional)
import ZkFold.Symbolic.Examples.Eq (exampleEq, exampleEqVector)

exampleConst :: Symbolic c => CompatData FieldElement c
exampleConst = fromConstant (5 :: Natural)

exampleConditionalConst
  :: Symbolic c
  => CompatData Bool c
  -> CompatData FieldElement c
  -> CompatData FieldElement c
exampleConditionalConst b x = exampleConditional b x exampleConst

exampleConditionalConstConst
  :: Symbolic c => CompatData Bool c -> CompatData FieldElement c
exampleConditionalConstConst b = exampleConditional b (fromConstant @Natural 3) exampleConst

exampleEqConst :: Symbolic c => CompatData FieldElement c -> CompatData Bool c
exampleEqConst = exampleEq exampleConst

exampleEqVectorConst
  :: (KnownNat n, Symbolic c)
  => (Vector n :.: CompatData FieldElement) c
  -> CompatData Bool c
exampleEqVectorConst = exampleEqVector (Comp1 $ tabulate $ const exampleConst)
