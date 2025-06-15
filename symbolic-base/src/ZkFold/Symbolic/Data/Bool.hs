module ZkFold.Symbolic.Data.Bool (
    BoolType(..),
    Bool(..),
    all,
    any,
    and,
    or
) where

import           Control.DeepSeq              (NFData)
import           Data.Eq                      (Eq (..))
import           Data.Function                (($))
import           Data.Functor                 ((<$>))
import           GHC.Generics                 (Par1 (..), Generic1 (..), Generic)
import qualified Prelude                      as Haskell
import           Text.Show                    (Show)

import           ZkFold.Algebra.Class
import           ZkFold.Data.Bool
import           ZkFold.Data.HFunctor.Classes (HEq, HNFData, HShow)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Class   (SymbolicData (..), Sym (..))
import           ZkFold.Symbolic.Interpreter  (Interpreter (..))
import           ZkFold.Symbolic.MonadCircuit (newAssigned)

-- TODO (Issue #18): hide this constructor
newtype Bool c = Bool (Sym Par1 c)
    deriving (Generic, Generic1)

deriving instance HNFData c => NFData (Bool c)
deriving instance HEq c => Eq (Bool c)
deriving instance HShow c => Show (Bool c)

instance SymbolicData Bool

instance {-# OVERLAPPING #-} (Eq a, MultiplicativeMonoid a) => Show (Bool (Interpreter a)) where
    show (fromBool -> x) = if x == one then "True" else "False"

instance Symbolic c => BoolType (Bool c) where
    true = fromContext $ embed (Par1 one)

    false = fromContext $ embed (Par1 zero)

    not b = fromContext $ fromCircuitF (toContext b) $
      \(Par1 v) -> Par1 <$> newAssigned (one - ($ v))

    b1 && b2 = fromContext $ fromCircuit2F (toContext b1) (toContext b2) $
      \(Par1 v1) (Par1 v2) -> Par1 <$> newAssigned (($ v1) * ($ v2))

    b1 || b2 = fromContext $ fromCircuit2F (toContext b1) (toContext b2) $
      \(Par1 v1) (Par1 v2) -> Par1 <$>
          newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - x1 * x2)

    b1 `xor` b2 = fromContext $ fromCircuit2F (toContext b1) (toContext b2) $
      \(Par1 v1) (Par1 v2) -> Par1 <$>
          newAssigned (\x -> let x1 = x v1; x2 = x v2 in x1 + x2 - (one + one) * x1 * x2)

fromBool :: Bool (Interpreter a) -> a
fromBool (Bool (Sym (Interpreter (Par1 b)))) = b
