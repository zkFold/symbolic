{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Bool (
    BoolType(..),
    Bool(..),
    all,
    all1,
    any
) where

import           Data.Eq                         (Eq (..))
import           Data.Foldable                   (Foldable (..))
import           Data.Function                   (($), (.))
import           Data.Functor                    (Functor, fmap, (<$>))
import           GHC.Generics                    (Par1 (..))
import qualified Prelude                         as Haskell
import           Text.Show                       (Show)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.HFunctor       (HFunctor)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Class      (SymbolicData)
import           ZkFold.Symbolic.Interpreter     (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit    (newAssigned)

class BoolType b where
    true  :: b

    false :: b

    not   :: b -> b

    infixr 3 &&
    (&&)  :: b -> b -> b

    infixr 2 ||
    (||)  :: b -> b -> b

    xor  :: b -> b -> b

instance BoolType Haskell.Bool where
    true  = Haskell.True

    false = Haskell.False

    not   = Haskell.not

    (&&)  = (Haskell.&&)

    (||)  = (Haskell.||)

    xor = xor

-- TODO (Issue #18): hide this constructor
newtype Bool c = Bool (c Par1)

deriving instance Eq (c Par1) => Eq (Bool c)

instance (Eq a, MultiplicativeMonoid a) => Show (Bool (Interpreter a)) where
    show (fromBool -> x) = if x == one then "True" else "False"

deriving newtype instance HFunctor c => SymbolicData c (Bool c)

instance Symbolic c => BoolType (Bool c) where
    true = Bool $ embed (Par1 one)

    false = Bool $ embed (Par1 zero)

    not (Bool b) = Bool $ fromCircuitF b $
      \(Par1 v) -> Par1 <$> newAssigned (one - ($ v))

    Bool b1 && Bool b2 = Bool $ fromCircuit2F b1 b2 $
      \(Par1 v1) (Par1 v2) -> Par1 <$> newAssigned (($ v1) * ($ v2))

    Bool b1 || Bool b2 = Bool $ fromCircuit2F b1 b2 $
      \(Par1 v1) (Par1 v2) -> Par1 <$>
          newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - x1 * x2)

    Bool b1 `xor` Bool b2 = Bool $ fromCircuit2F b1 b2 $
      \(Par1 v1) (Par1 v2) -> Par1 <$>
          newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - (one + one) * x1 * x2)

fromBool :: Bool (Interpreter a) -> a
fromBool (Bool (Interpreter (Par1 b))) = b

all :: (BoolType b, Foldable t) => (x -> b) -> t x -> b
all f = foldr ((&&) . f) true

all1 :: (BoolType b, Functor t, Foldable t) => (x -> b) -> t x -> b
all1 f = foldr1 (&&) . fmap f

any :: (BoolType b, Foldable t) => (x -> b) -> t x -> b
any f = foldr ((||) . f) false
