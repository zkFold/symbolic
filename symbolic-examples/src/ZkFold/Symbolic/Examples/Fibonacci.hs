{-# LANGUAGE TypeApplications #-}

module ZkFold.Symbolic.Examples.Fibonacci (exampleFibonacciMod) where

import Data.Foldable (foldl)
import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool, bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude (Integer)

-- | The Fibonacci index function.
-- If `x` is a Fibonacci number, returns its index (up until `nMax`).
-- Otherwise, returns `0`.
exampleFibonacciMod
  :: forall c. Symbolic c => Integer -> FieldElement c -> FieldElement c
exampleFibonacciMod nMax x =
  foldl
    (\m k -> bool @(Bool c) m (fromConstant k) (fib k one one == x))
    zero
    [1 .. nMax]
 where
  fib :: Integer -> FieldElement c -> FieldElement c -> FieldElement c
  fib 1 x1 _ = x1
  fib n x1 x2 = fib (n - 1) x2 (x1 + x2)
