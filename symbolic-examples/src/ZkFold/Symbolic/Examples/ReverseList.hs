{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.ReverseList (exampleReverseList) where

import GHC.Generics ((:.:) (..))
import ZkFold.Data.Vector (Vector, reverse)

-- | Reverses the order of elements in a vector
exampleReverseList :: (Vector n :.: t) c -> (Vector n :.: t) c
exampleReverseList (Comp1 v) = Comp1 (reverse v)
