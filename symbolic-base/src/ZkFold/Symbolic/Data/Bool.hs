{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Bool (
    BoolType(..),
    Bool(..),
    all,
    all1,
    any,
    and,
    or
) where

import           Control.DeepSeq                   (NFData)
import           Data.Eq                           (Eq (..))
import           Data.Foldable                     (Foldable (..))
import           Data.Function                     (($), (.))
import           Data.Functor                      (Functor, fmap, (<$>))
import           GHC.Generics                      (Generic, Par1 (..))
import qualified Prelude                           as Haskell
import           Text.Show                         (Show)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.HFunctor.Classes (HEq, HNFData, HShow)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Class        (SymbolicData)
import           ZkFold.Symbolic.Interpreter       (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit      (newAssigned)

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
    deriving (Generic)

deriving instance HNFData c => NFData (Bool c)
deriving instance HEq c => Eq (Bool c)
deriving instance HShow c => Show (Bool c)

instance Symbolic c => SymbolicData (Bool c)

instance {-# OVERLAPPING #-} (Eq a, MultiplicativeMonoid a) => Show (Bool (Interpreter a)) where
    show (fromBool -> x) = if x == one then "True" else "False"

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

and :: (BoolType b, Foldable t) => t b -> b
and = all Haskell.id

or :: (BoolType b, Foldable t) => t b -> b
or = any Haskell.id

all1 :: (BoolType b, Functor t, Foldable t) => (x -> b) -> t x -> b
all1 f = foldr1 (&&) . fmap f

any :: (BoolType b, Foldable t) => (x -> b) -> t x -> b
any f = foldr ((||) . f) false
