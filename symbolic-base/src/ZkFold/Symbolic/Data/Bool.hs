{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.Bool (
  BoolType (..),
  Bool (..),
  Conditional (..),
  all,
  any,
  and,
  or,
) where

import Control.DeepSeq (NFData)
import Data.Eq (Eq (..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Par1 (..))
import Text.Show (Show)
import ZkFold.Algebra.Class
import ZkFold.Control.Conditional (Conditional (..))
import ZkFold.Data.Bool
import ZkFold.Data.HFunctor.Classes (HEq, HNFData, HShow)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class (Context, SymbolicData, interpolate)
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Symbolic.MonadCircuit (newAssigned)
import qualified Prelude as Haskell

-- TODO (Issue #18): hide this constructor
newtype Bool c = Bool (c Par1)
  deriving Generic

deriving instance HNFData c => NFData (Bool c)

deriving instance HEq c => Eq (Bool c)

deriving instance HShow c => Show (Bool c)

instance Symbolic c => SymbolicData (Bool c)

instance {-# OVERLAPPING #-} (Eq a, MultiplicativeMonoid a) => Show (Bool (Interpreter a)) where
  show (fromBool -> x) = if x == one then "True" else "False"

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

instance (SymbolicData d, Context d ~ c) => Conditional (Bool c) d where
  bool onFalse onTrue (Bool b) =
    interpolate ((zero, onFalse) :| [(one, onTrue)]) b

fromBool :: Bool (Interpreter a) -> a
fromBool (Bool (Interpreter (Par1 b))) = b
