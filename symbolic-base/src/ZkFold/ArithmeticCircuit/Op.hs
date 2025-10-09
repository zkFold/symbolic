module ZkFold.ArithmeticCircuit.Op where

import GHC.Integer (Integer)
import GHC.Natural (Natural)
import Control.Applicative (Applicative, pure, (<*>))
import Data.Functor ((<$>))
import Data.ByteString (ByteString, cons)
import ZkFold.Data.Binary (toByteString)
import Data.Semigroup ((<>))

data Sort = ZZp | ZZ | BB | OO

data SortSing (s :: Sort) where
  ZZpSing :: SortSing ZZp
  ZZSing :: SortSing ZZ
  BBSing :: SortSing BB
  OOSing :: SortSing OO

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
  OpEq, OpNEq :: f s -> f s -> Op f BB
  OpOr :: f BB -> f BB -> Op f BB
  OpBool :: f s -> f s -> f BB -> Op f s
  OpAppend :: f OO -> f OO -> Op f OO
  OpOrder :: f ZZ -> f ZZ -> f ZZ -> f OO -> Op f ZZ

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
  OpNEq x y -> OpNEq <$> f x <*> f y
  OpOr x y -> OpOr <$> f x <*> f y
  OpBool x y z -> OpBool <$> f x <*> f y <*> f z
  OpAppend x y -> OpAppend <$> f x <*> f y
  OpOrder x y z w -> OpOrder <$> f x <*> f y <*> f z <*> f w

opToBinary :: forall f s. (forall t. f t -> ByteString) -> Op f s -> ByteString
opToBinary to = \case
  OpConst x -> 0 `cons` toByteString x
  OpScale k x -> 1 `cons` toByteString k <> to x
  OpAdd x y -> 2 `cons` to x <> to y
  OpMul x y -> 3 `cons` to x <> to y
  OpNeg x -> 4 `cons` to x
  OpExp x p -> 5 `cons` to x <> toByteString p
  OpFrom x -> 6 `cons` to x
  OpTo x -> 7 `cons` to x
  OpCompare x y -> 8 `cons` to x <> to y
  OpDiv x y -> 9 `cons` to x <> to y
  OpMod x y -> 10 `cons` to x <> to y
  OpGcd x y -> 11 `cons` to x <> to y
  OpBezoutL x y -> 12 `cons` to x <> to y
  OpBezoutR x y -> 13 `cons` to x <> to y
  OpInv x -> 14 `cons` to x
  OpEq x y -> 15 `cons` to x <> to y
  OpNEq x y -> 16 `cons` to x <> to y
  OpOr x y -> 17 `cons` to x <> to y
  OpBool x y z -> 18 `cons` to x <> to y <> to z
  OpAppend x y -> 19 `cons` to x <> to y
  OpOrder x y z w -> 20 `cons` to x <> to y <> to z <> to w
