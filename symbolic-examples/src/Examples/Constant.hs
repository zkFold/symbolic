module Examples.Constant
    ( exampleConst
    , exampleConditionalConst
    , exampleConditionalConstConst
    , exampleEqConst
    , exampleEqVectorConst) where

import           Data.Function                     (const, ($))
import           Examples.Conditional              (exampleConditional)
import           Examples.Eq                       (exampleEq, exampleEqVector)

import           ZkFold.Base.Algebra.Basic.Class   (FromConstant (..))
import           ZkFold.Base.Algebra.Basic.Number  (Natural, KnownNat)
import           ZkFold.Base.Data.Vector           (Vector)
import           ZkFold.Symbolic.Class             (Symbolic)
import           ZkFold.Symbolic.Data.Bool         (Bool)
import           ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Control.Monad.Representable.Reader (Representable(tabulate))


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