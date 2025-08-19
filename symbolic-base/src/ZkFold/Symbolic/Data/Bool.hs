{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.Bool (
  BoolType (..),
  Bool (..),
  Conditional (..),
  SymbolicEq,
  all,
  any,
  and,
  or,
) where

import Control.DeepSeq (NFData)
import Control.Monad (return)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy)
import Data.Traversable (Traversable, for)
import Data.Type.Equality (type (~))
import GHC.Generics (Par1 (..))
import Text.Show (Show)
import qualified Prelude as Haskell

import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..), ifThenElse)
import ZkFold.Data.Bool
import ZkFold.Data.Eq
import ZkFold.Data.HFunctor.Classes (HEq, HNFData, HShow)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class (SymbolicData, interpolate)
import ZkFold.Symbolic.Data.Combinators (runInvert)
import ZkFold.Symbolic.Data.Vec (Vec (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Symbolic.MonadCircuit (newAssigned)
import Data.Semialign (Semialign (align), alignWith)
import Data.These (These(..))
import Control.Applicative (pure)

-- TODO (Issue #18): hide this constructor
newtype Bool c = Bool (c Par1)
  deriving SymbolicData via (Vec Par1)

fromBool :: Bool (Interpreter a) -> a
fromBool (Bool (Interpreter (Par1 b))) = b

deriving instance HNFData c => NFData (Bool c)

deriving instance HEq c => Haskell.Eq (Bool c)

deriving instance HShow c => Show (Bool c)

instance {-# OVERLAPPING #-} (Haskell.Eq a, MultiplicativeMonoid a) => Show (Bool (Interpreter a)) where
  show (fromBool -> x) = if x Haskell.== one then "True" else "False"

instance Symbolic c => FromConstant Haskell.Bool (Bool c) where
  fromConstant b = ifThenElse b true false

instance Symbolic c => BoolType (Bool c) where
  true = Bool $ embed (Par1 one)

  false = Bool $ embed (Par1 zero)

  not (Bool b) = Bool $
    fromCircuitF b $
      \(Par1 v) -> Par1 <$> newAssigned (one - ($ v))

  Bool b1 && Bool b2 = Bool $
    fromCircuit2F b1 b2 $
      \(Par1 v1) (Par1 v2) -> Par1 <$> newAssigned (($ v1) * ($ v2))

  Bool b1 || Bool b2 = Bool $
    fromCircuit2F b1 b2 $
      \(Par1 v1) (Par1 v2) ->
        Par1
          <$> newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - x1 * x2)

  Bool b1 `xor` Bool b2 = Bool $
    fromCircuit2F b1 b2 $
      \(Par1 v1) (Par1 v2) ->
        Par1
          <$> newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - (one + one) * x1 * x2)

instance (Symbolic c, Semialign f, Traversable f) => Conditional (Bool c) (c f) where
  bool onFalse onTrue = runVec . bool (Vec onFalse) (Vec onTrue)

instance (Symbolic c, SymbolicData d) => Conditional (Bool c) (d c) where
  bool onFalse onTrue (Bool b) =
    interpolate ((zero, onFalse) :| [(one, onTrue)]) b

type SymbolicEq x c =
  ( SymbolicData x
  , Eq (x c)
  , BooleanOf (x c) ~ Bool c
  )

instance Symbolic c => Eq (Proxy c) where
  type BooleanOf (Proxy c) = Bool c
  _ == _ = true
  _ /= _ = false

instance Symbolic c => Eq (Bool c) where
  type BooleanOf (Bool c) = Bool c
  b == b' = not (b /= b')
  (/=) = xor

instance (Symbolic c, Semialign f, Traversable f) => Eq (c f) where
  type BooleanOf (c f) = Bool c
  x == y =
    let
      result =
        symbolic2F
          x
          y
          (alignWith \case
            These i j -> bool zero one (i Haskell.== j)
            _ -> zero
          )
          (\x' y' -> do
              difference <- for (align x' y') $ \case
                These i j -> newAssigned \w -> w i - w j
                _ -> pure (fromConstant @(BaseField c) one)
              (isZeros, _) <- runInvert difference
              return isZeros
          )
     in
      all Bool (unpacked result)

  x /= y =
    let
      result =
        symbolic2F
          x
          y
          (alignWith \case
            These i j -> bool zero one (i Haskell./= j)
            _ -> one
          )
          ( \x' y' -> do
              difference <- for (align x' y') $ \case
                These i j -> newAssigned \w -> w i - w j
                _ -> pure (fromConstant @(BaseField c) one)
              (isZeros, _) <- runInvert difference
              for isZeros $ \isZ ->
                newAssigned (\w -> one - w isZ)
          )
     in
      any Bool (unpacked result)
