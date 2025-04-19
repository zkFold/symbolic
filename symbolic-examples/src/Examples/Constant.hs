module Examples.Constant
    ( exampleConst
    , exampleConditionalConst
    , exampleConditionalConstConst
    , exampleEqConst
    , exampleEqVectorConst) where

import           Control.Monad.Representable.Reader (Representable (tabulate))
import           Data.Function                      (const, ($))
import           Examples.Conditional               (exampleConditional)
import           Examples.Eq                        (exampleEq, exampleEqVector)

import           ZkFold.Algebra.Class               (FromConstant (..))
import           ZkFold.Algebra.Number              (KnownNat, Natural)
import           ZkFold.Data.Vector                 (Vector)
import           ZkFold.Symbolic.Class              (Symbolic)
import           ZkFold.Symbolic.Data.Bool          (Bool)
import           ZkFold.Symbolic.Data.FieldElement  (FieldElement)


exampleConst :: Symbolic c => FieldElement c
exampleConst = fromConstant (5 :: Natural)

exampleConditionalConst :: Symbolic c => Bool c -> FieldElement c -> FieldElement c
exampleConditionalConst b x = exampleConditional b x exampleConst

exampleConditionalConstConst :: Symbolic c => Bool c -> FieldElement c
exampleConditionalConstConst b = exampleConditional b (fromConstant @Natural 3) exampleConst

exampleEqConst :: Symbolic c => FieldElement c -> Bool c
exampleEqConst = exampleEq exampleConst

exampleEqVectorConst :: (KnownNat n, Symbolic c)
    => Vector n (FieldElement c)
    -> Bool c
exampleEqVectorConst = exampleEqVector (tabulate $ const exampleConst)
