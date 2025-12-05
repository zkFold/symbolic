module ZkFold.ArithmeticCircuit.Op where

import Control.Applicative (Applicative, pure, (<*>))
import Data.Functor ((<$>))
import GHC.Integer (Integer)
import GHC.Natural (Natural)

-- | A 'PrimeField' class describes operations available between 4 types:
-- * finite field itself
-- * backing integral type
-- * booleans associated with both
-- * ordering associated with integers
--
-- 'Sort' is a set of labels for differentiating between them.
data Sort = ZZp | ZZ | BB | OO

-- | A way to store type-level 'Sort' on the term-level.
data SortSing (s :: Sort) where
  ZZpSing :: SortSing ZZp
  ZZSing :: SortSing ZZ
  BBSing :: SortSing BB
  OOSing :: SortSing OO

-- | A class for communicating between term-level and type-level 'Sort's.
class KnownSort (s :: Sort) where
  knownSort :: SortSing s

instance KnownSort ZZp where
  knownSort = ZZpSing

instance KnownSort ZZ where
  knownSort = ZZSing

instance KnownSort BB where
  knownSort = BBSing

instance KnownSort OO where
  knownSort = OOSing

-- | 'Op f s' describes operations available in the 'PrimeField' class
-- where 's' is a sort of the result of an operation
-- and 'f' is a @Sort -> Type@ functor which, given a sort,
-- would return an argument to the operation of the type labeled with this sort.
data Op f (s :: Sort) where
  OpConst :: KnownSort s => Integer -> Op f s
  OpScale :: Integer -> f s -> Op f s
  OpAdd, OpMul :: f s -> f s -> Op f s
  OpNeg :: f s -> Op f s
  OpExp :: f s -> Natural -> Op f s
  OpFrom :: f ZZ -> Op f ZZp
  OpTo :: f ZZp -> Op f ZZ
  OpCompare :: f ZZ -> f ZZ -> Op f OO
  OpDiv, OpMod, OpGcd, OpBezoutL, OpBezoutR :: f ZZ -> f ZZ -> Op f ZZ
  OpInv :: f ZZp -> Op f ZZp
  OpEq :: f s -> f s -> Op f BB
  OpOr :: f BB -> f BB -> Op f BB
  OpBool :: f s -> f s -> f BB -> Op f s
  OpAppend :: f OO -> f OO -> Op f OO

-- | Replacement of a @Sort -> Type@ functor in 'Op',
-- possibly with side-effects.
traverseOp
  :: Applicative m => (forall t. f t -> m (g t)) -> Op f s -> m (Op g s)
traverseOp f = \case
  OpConst i -> pure (OpConst i)
  OpScale i x -> OpScale i <$> f x
  OpAdd x y -> OpAdd <$> f x <*> f y
  OpMul x y -> OpMul <$> f x <*> f y
  OpNeg x -> OpNeg <$> f x
  OpExp x e -> (`OpExp` e) <$> f x
  OpFrom x -> OpFrom <$> f x
  OpTo x -> OpTo <$> f x
  OpCompare x y -> OpCompare <$> f x <*> f y
  OpDiv x y -> OpDiv <$> f x <*> f y
  OpMod x y -> OpMod <$> f x <*> f y
  OpGcd x y -> OpGcd <$> f x <*> f y
  OpBezoutL x y -> OpBezoutL <$> f x <*> f y
  OpBezoutR x y -> OpBezoutR <$> f x <*> f y
  OpInv x -> OpInv <$> f x
  OpEq x y -> OpEq <$> f x <*> f y
  OpOr x y -> OpOr <$> f x <*> f y
  OpBool x y z -> OpBool <$> f x <*> f y <*> f z
  OpAppend x y -> OpAppend <$> f x <*> f y
