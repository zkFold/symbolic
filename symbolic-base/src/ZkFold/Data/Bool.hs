module ZkFold.Data.Bool where

import Data.Bool (Bool (..))
import qualified Data.Bool as Haskell
import Data.Foldable (Foldable, foldr)
import Data.Function (id, (.))

class BoolType b where
  true, false :: b
  not :: b -> b
  infixr 3 &&
  infixr 2 ||
  (&&), (||), xor :: b -> b -> b

instance BoolType Bool where
  true = True
  false = False
  not = Haskell.not
  (&&) = (Haskell.&&)
  (||) = (Haskell.||)
  xor = xor

all :: (BoolType b, Foldable t) => (x -> b) -> t x -> b
all f = foldr ((&&) . f) true

and :: (BoolType b, Foldable t) => t b -> b
and = all id

any :: (BoolType b, Foldable t) => (x -> b) -> t x -> b
any f = foldr ((||) . f) false

or :: (BoolType b, Foldable t) => t b -> b
or = any id
