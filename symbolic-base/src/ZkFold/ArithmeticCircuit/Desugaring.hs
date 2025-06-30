module ZkFold.ArithmeticCircuit.Desugaring (desugarRanges) where

import Control.Monad (foldM, return)
import Control.Monad.State (execState)
import Data.Binary (Binary)
import Data.Bool (otherwise)
import Data.Either (Either (..), partitionEithers)
import Data.Eq ((/=), (==))
import Data.Function (flip, ($), (.))
import Data.List (dropWhile, head, zip)
import qualified Data.Map.Monoidal as MM
import Data.Maybe (maybe)
import qualified Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple (fst, uncurry)
import Prelude (error)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit.Context (CircuitContext, acLookup)
import ZkFold.ArithmeticCircuit.Lookup (asRange)
import ZkFold.ArithmeticCircuit.Var (toVar)
import ZkFold.Prelude (assert, length)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Combinators (expansion)
import ZkFold.Symbolic.MonadCircuit

desugarRange :: (Arithmetic a, MonadCircuit i a w m) => i -> (a, a) -> m ()
desugarRange i (a, b)
  | a /= zero = error "non-zero lower bound not supported yet"
  | b == negate one = return ()
  | otherwise = do
      let bs = binaryExpansion (toConstant b)
      is <- expansion (length bs) i
      case dropWhile ((== one) . fst) (zip bs is) of
        [] -> return ()
        ((_, k0) : ds) -> do
          z <- newAssigned (one - ($ k0))
          ge <- foldM (\j (c, k) -> newAssigned $ forceGE j c k) z ds
          constraint (($ ge) - one)
 where
  forceGE j c k
    | c == zero = ($ j) * (one - ($ k))
    | otherwise = one + ($ k) * (($ j) - one)

-- | Desugars range constraints into polynomial constraints
desugarRanges
  :: (Arithmetic a, Binary a)
  => CircuitContext a o -> CircuitContext a o
desugarRanges c =
  let (rm, tm) =
        partitionEithers
          [ maybe (Right (k, v)) (Left . (,v)) (asRange k)
          | (k, v) <- MM.assocs (acLookup c)
          ]
   in flip execState c {acLookup = MM.fromList tm} $
        traverse
          (uncurry desugarRange)
          -- TODO: @v@ should belong to either of segments
          [ (toVar v, assert (length k == 1) (length k) (head k))
          | (S.toList -> k, s) <- rm
          , [v] <- S.toList s
          ]
