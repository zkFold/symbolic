{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.Empty where

import Data.Function (const, ($))
import Data.Functor.Rep (Representable, tabulate)
import GHC.Generics (U1 (..), (:*:) (..), (:.:) (..))

-- TODO: Most likely temporary.
-- Remove once we get rid of Layout/Payload split in SymbolicData.

class Empty f where
  empty :: f a

instance Empty U1 where
  empty = U1

instance (Empty f, Empty g) => Empty (f :*: g) where
  empty = empty :*: empty

instance (Representable f, Empty g) => Empty (f :.: g) where
  empty = Comp1 $ tabulate (const empty)
