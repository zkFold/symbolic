{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
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

import           Control.DeepSeq                                   (NFData)
import           Data.Eq                                           (Eq (..))
import           Data.Foldable                                     (Foldable (..))
import           Data.Function                                     (($), (.))
import           Data.Functor                                      (Functor, fmap, (<$>))
import           GHC.Generics                                      (Generic, Par1 (..), (:*:) (..))
import qualified Prelude                                           as Haskell
import           Text.Show                                         (Show)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Class                        (SymbolicData)
import           ZkFold.Symbolic.Interpreter                       (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit                      (MonadCircuit (..), newAssigned)
import ZkFold.Symbolic.Data.Lookup

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

deriving instance NFData (c Par1) => NFData (Bool c)
deriving instance Eq (c Par1) => Eq (Bool c)
deriving instance Show (c Par1) => Show (Bool c)

instance Symbolic c => SymbolicData (Bool c)

instance {-# OVERLAPPING #-} (Eq a, MultiplicativeMonoid a) => Show (Bool (Interpreter a)) where
    show (fromBool -> x) = if x == one then "True" else "False"

instance (Symbolic c) => BoolType (Bool c) where
    true = Bool $ embed (Par1 one)

    false = Bool $ embed (Par1 zero)

    not (Bool b) = Bool $ fromCircuitF b $
      \(Par1 v) -> Par1 <$> newAssigned (one - ($ v))

    Bool b1 && Bool b2 = Bool $ fromCircuit2F b1 b2 $
      \p1 p2 -> newBinLookup bin2Lookup (p1 :*: p2) andOp

    Bool b1 || Bool b2 = Bool $ fromCircuit2F b1 b2 $
      \p1 p2 -> newBinLookup bin2Lookup (p1 :*: p2) orOp

    Bool b1 `xor` Bool b2 = Bool $ fromCircuit2F b1 b2 $
      \p1 p2 -> newBinLookup bin2Lookup (p1 :*: p2) xorOp

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

-- boolLookup :: Arithmetic a => LookupTable a Par1
-- boolLookup = Ranges $ S.singleton (zero, one)

-- bool2Lookup :: Arithmetic a => LookupTable a (Par1 :*: Par1)
-- bool2Lookup = Product boolLookup boolLookup

-- newBinLookup :: forall f var a w m.
--   ( Traversable f, Typeable f, Representable f
--   , MonadCircuit var a w m, Binary (Rep f))
--    => LookupTable a f -> f var -> (forall x. ResidueField x => f x -> Par1 x) -> m (Par1 var)
-- newBinLookup dom vars f = do
--     let vs = fmap (at @var @w )vars
--         (Par1 v3w) = f vs
--     v3 <- unconstrained v3w
--     fId <- registerFunction f
--     lookupConstraint (vars :*: (Par1 v3)) (Plot fId dom)
--     return $ Par1 v3


-- --------------------------------------------------------------------------------

-- orOp :: (AdditiveGroup p, MultiplicativeSemigroup p) => (:*:) Par1 Par1 p -> Par1 p
-- orOp ((Par1 v1) :*: (Par1 v2)) = Par1 (v1 + v2 - v1 * v2)

-- andOp :: MultiplicativeSemigroup p => (:*:) Par1 Par1 p -> Par1 p
-- andOp ((Par1 v1) :*: (Par1 v2)) = Par1 (v1 * v2)

-- xorOp :: (AdditiveGroup p, MultiplicativeMonoid p) => (:*:) Par1 Par1 p -> Par1 p
-- xorOp ((Par1 v1) :*: (Par1 v2)) = Par1 (v1 + v2 - (one + one) * v1 * v2)
