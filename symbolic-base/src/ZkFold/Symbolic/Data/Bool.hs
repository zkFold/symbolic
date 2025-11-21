{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Bool (
  BoolType (..),
  Bool (..),
  Conditional (..),
  all,
  any,
  and,
  or,
  assert,
  bitwiseGE,
  bitwiseGT,
) where

import Control.DeepSeq (NFData)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Traversable (Traversable)
import GHC.Generics (Par1 (..))
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..), ifThenElse)
import ZkFold.Data.Bool
import ZkFold.Data.Eq
import ZkFold.Symbolic.Data.Vec (Vec (..))
import qualified Data.Zip as Z
import Data.Foldable (Foldable)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Class
import Data.Semialign (Semialign)

-- TODO (Issue #18): hide this constructor
newtype Bool c = Bool { fromBool :: c }
  deriving stock (Haskell.Eq)
  deriving SymbolicData via (Vec Par1)

deriving instance NFData c => NFData (Bool c)

instance {-# OVERLAPPING #-} (Haskell.Eq a, MultiplicativeMonoid a) => Show (Bool a) where
  show (fromBool -> x) = if x Haskell.== one then "True" else "False"

instance Symbolic c => FromConstant Haskell.Bool (Bool c) where
  fromConstant b = ifThenElse b true false

instance Symbolic c => Arbitrary (Bool c) where
  arbitrary = fromConstant @Haskell.Bool <$> arbitrary

instance Symbolic c => BoolType (Bool c) where
  true = Bool one

  false = Bool zero

  not (Bool _b) = Bool $ Haskell.error "TODO"
    -- fromCircuitF b $
    --  \(Par1 v) -> Par1 <$> newAssigned (one - ($ v))

  Bool _b1 && Bool _b2 = Bool $ Haskell.error "TODO"
    -- fromCircuit2F b1 b2 $
    --   \(Par1 v1) (Par1 v2) -> Par1 <$> newAssigned (($ v1) * ($ v2))

  Bool _b1 || Bool _b2 = Bool $ Haskell.error "TODO"
    -- fromCircuit2F b1 b2 $
    --   \(Par1 v1) (Par1 v2) ->
    --     Par1
    --       <$> newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - x1 * x2)

  Bool _b1 `xor` Bool _b2 = Bool $ Haskell.error "TODO"
    -- fromCircuit2F b1 b2 $
    --   \(Par1 v1) (Par1 v2) ->
    --     Par1
    --       <$> newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - (one + one) * x1 * x2)

instance Symbolic c => Conditional (Bool c) (d c) where
  bool _onFalse _onTrue (Bool _b) = Haskell.error "TODO"
    -- interpolate ((zero, onFalse) :| [(one, onTrue)]) b

instance Symbolic c => Eq (Bool c) where
  type BooleanOf (Bool c) = Bool c
  b == b' = not (b /= b')
  (/=) = xor

instance (Symbolic c, Semialign f, Traversable f) => Eq (Vec f c) where
  type BooleanOf (Vec f c) = Bool c
  Vec _x == Vec _y =
    let
      result :: f c
      result = Haskell.error "TODO"
        -- symbolic2F
        --  x
        --  y
        --  ( alignWith \case
        --      These i j -> bool zero one (i Haskell.== j)
        --      _ -> zero
        --  )
        --  ( \x' y' -> do
        --      difference <- for (align x' y') $ \case
        --        These i j -> newAssigned \w -> w i - w j
        --        _ -> pure (fromConstant @c one)
        --      (isZeros, _) <- runInvert difference
        --      return isZeros
        --  )
     in
      all Bool result

  Vec _x /= Vec _y =
    let
      result :: f c
      result = Haskell.error "TODO"
        -- symbolic2F
        --   x
        --   y
        --   ( alignWith \case
        --       These i j -> bool zero one (i Haskell./= j)
        --       _ -> one
        --   )
        --   ( \x' y' -> do
        --       difference <- for (align x' y') $ \case
        --         These i j -> newAssigned \w -> w i - w j
        --         _ -> pure (fromConstant @c one)
        --       (isZeros, _) <- runInvert difference
        --       for isZeros $ \isZ ->
        --         newAssigned (\w -> one - w isZ)
        --   )
     in
      any Bool result

assert :: (h c -> Bool c) -> h c -> h c
assert _p _x = Haskell.error "TODO"
  -- restore
  --   ( fromCircuit2F (arithmetize $ p x) (arithmetize x) \(Par1 b) i -> do
  --       constraint (\v -> v b - one)
  --       pure i
  --   , payload x
  --   )

bitwiseGE :: forall r c f. (Symbolic c, Z.Zip f, Foldable f, KnownNat r) => f c -> f c -> Bool c
-- ^ Given two lists of bits of equal length, compares them lexicographically.
bitwiseGE _xs _ys = Bool $ Haskell.error "TODO" -- symbolic2F xs ys
    -- (\us vs -> Par1 $ bool zero one (toList us Haskell.>= toList vs))
    -- $ \is js -> Par1 <$> blueprintGE @r is js

bitwiseGT :: forall r c f. (Symbolic c, Z.Zip f, Foldable f, KnownNat r) => f c -> f c -> Bool c
-- ^ Given two lists of bits of equal length, compares them lexicographically.
bitwiseGT _xs _ys = Bool $ Haskell.error "TODO"
  -- $ symbolic2F
  --   xs
  --   ys
  --   (\us vs -> Par1 $ bool zero one (toList us Haskell.> toList vs))
  -- $ \is js -> do
  --   (hasOne, hasNegOne) <- circuitDelta @r is js
  --   Par1 <$> newAssigned (\p -> p hasOne * (one - p hasNegOne))
