{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Class where

import           Control.DeepSeq                  (NFData)
import           Control.Monad
import           Data.Eq                          (Eq)
import           Data.Foldable                    (Foldable)
import           Data.Function                    ((.))
import           Data.Functor                     ((<$>))
import           Data.Kind                        (Type)
import           Data.Ord                         (Ord)
import           Data.Type.Equality               (type (~))
import           GHC.Generics                     (type (:.:) (unComp1))
import           Numeric.Natural                  (Natural)
import           Prelude                          (Integer)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Control.HApplicative (HApplicative (hpair, hunit))
import           ZkFold.Base.Data.Package         (Package (pack))
import           ZkFold.Base.Data.Product         (uncurryP)
import           ZkFold.Symbolic.MonadCircuit

-- | Field of residues with decidable equality and ordering
-- is called an ``arithmetic'' field.
type Arithmetic a = ( ResidueField a, IntegralOf a ~ Integer
                    , ToConstant a, Const a ~ Natural
                    , Eq a, Ord a, NFData a)

-- | A type of mappings between functors inside a circuit.
-- @fs@ are input functors, @g@ is an output functor, @c@ is context.
--
-- A function is a mapping between functors inside a circuit if,
-- given an arbitrary builder of circuits @m@ over @c@ with arbitrary @i@ as
-- variables, it maps @f@ many inputs to @g@ many outputs using @m@.
--
-- NOTE: the property above is correct by construction for each function of a
-- suitable type, you don't have to check it yourself.
type CircuitFun (fs :: [Type -> Type]) (g :: Type -> Type) (c :: (Type -> Type) -> Type) =
  forall i m. (NFData i, MonadCircuit i (BaseField c) (WitnessField c) m) => FunBody fs g i m

type family FunBody (fs :: [Type -> Type]) (g :: Type -> Type) (i :: Type) (m :: Type -> Type) where
  FunBody '[] g i m = m (g i)
  FunBody (f ': fs) g i m = f i -> FunBody fs g i m

-- | A Symbolic DSL for performant pure computations with arithmetic circuits.
-- @c@ is a generic context in which computations are performed.
class ( HApplicative c, Package c, Arithmetic (BaseField c)
      , ResidueField (WitnessField c)) => Symbolic c where
    -- | Base algebraic field over which computations are performed.
    type BaseField c :: Type
    -- | Type of witnesses usable inside circuit construction
    type WitnessField c :: Type

    -- | Computes witnesses (exact value may depend on the input to context).
    witnessF :: Functor f => c f -> f (WitnessField c)

    -- | To perform computations in a generic context @c@ -- that is, to form a
    -- mapping between @c f@ and @c g@ for given @f@ and @g@ -- you need to
    -- provide an algorithm for turning @f@ into @g@ inside a circuit.
    fromCircuitF :: c f -> CircuitFun '[f] g c -> c g

    -- | If there is a simpler implementation of a function in pure context,
    -- you can provide it via 'sanityF' to use it in pure contexts.
    sanityF :: BaseField c ~ a => c f -> (f a -> g a) -> (c f -> c g) -> c g
    sanityF x _ f = f x

-- | Embeds the pure value(s) into generic context @c@.
embed :: (Symbolic c, Functor f) => f (BaseField c) -> c f
embed cs = fromCircuitF hunit (\_ -> return (fromConstant <$> cs))

symbolicF ::
  (Symbolic c, BaseField c ~ a) => c f ->
  (f a -> g a) -> CircuitFun '[f] g c -> c g
symbolicF x f c = sanityF x f (`fromCircuitF` c)

symbolic2F ::
  (Symbolic c, BaseField c ~ a) => c f -> c g ->
  (f a -> g a -> h a) -> CircuitFun '[f, g] h c -> c h
-- | Runs the binary function from @f@ and @g@ into @h@ in a generic context @c@.
symbolic2F x y f m = symbolicF (hpair x y) (uncurryP f) (uncurryP m)

fromCircuit2F :: Symbolic c => c f -> c g -> CircuitFun '[f, g] h c -> c h
-- | Runs the binary @'CircuitFun'@ in a generic context.
fromCircuit2F x y m = fromCircuitF (hpair x y) (uncurryP m)

symbolic3F ::
  (Symbolic c, BaseField c ~ a) => c f -> c g -> c h ->
  (f a -> g a -> h a -> k a) -> CircuitFun '[f, g, h] k c -> c k
-- | Runs the ternary function from @f@, @g@ and @h@ into @k@ in a context @c@.
symbolic3F x y z f m = symbolic2F (hpair x y) z (uncurryP f) (uncurryP m)

fromCircuit3F ::
  Symbolic c => c f -> c g -> c h -> CircuitFun '[f, g, h] k c -> c k
-- | Runs the ternary @'CircuitFun'@ in a generic context.
fromCircuit3F x y z m = fromCircuit2F (hpair x y) z (uncurryP m)

symbolicVF ::
  (Symbolic c, BaseField c ~ a, WitnessField c ~ w, Foldable f, Functor f) =>
  f (c g) -> (f (g a) -> h a) ->
  (forall i m. MonadCircuit i a w m => f (g i) -> m (h i)) -> c h
-- | Given a generic context @c@, runs the function from @f@ many @c g@'s into @c h@.
symbolicVF xs f m = symbolicF (pack xs) (f . unComp1) (m . unComp1)

fromCircuitVF ::
  (Symbolic c, BaseField c ~ a, WitnessField c ~ w, Foldable f, Functor f) =>
  f (c g) -> (forall i m. MonadCircuit i a w m => f (g i) -> m (h i)) -> c h
-- | Given a generic context @c@, runs the @'CircuitFun'@ from @f@ many @c g@'s into @c h@.
fromCircuitVF xs m = fromCircuitF (pack xs) (m . unComp1)
