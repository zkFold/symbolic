{-# LANGUAGE BlockArguments #-}

module ZkFold.ArithmeticCircuit.Desugaring (desugarRanges) where

import Control.Applicative (pure)
import Control.Monad (guard, return, zipWithM, (>>), (>>=))
import Control.Monad.State (State, execState)
import Data.Binary (Binary)
import Data.Bool (otherwise)
import Data.Either (Either (..), partitionEithers)
import Data.Foldable (foldrM)
import Data.Function (flip, ($), (.))
import Data.Functor (fmap)
import Data.List (head, unfoldr)
import qualified Data.Map.Monoidal as MM
import Data.Maybe (Maybe (..), maybe)
import qualified Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple (swap, uncurry)
import Numeric.Natural (Natural)
import Prelude (error)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit.Context
import ZkFold.ArithmeticCircuit.Var (Var, at, toVar)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq ((/=), (==))
import ZkFold.Prelude (assert, length, take)
import ZkFold.Symbolic.Class (Arithmetic)

-- | Desugars range constraints into polynomial constraints
desugarRanges
  :: forall a o
   . (Arithmetic a, Binary a)
  => Natural -> CircuitContext a o -> CircuitContext a o
desugarRanges regBound ctx =
  let (rm, tm) =
        partitionEithers
          [ maybe (Right (k, v)) (Left . (,v)) (asRange k)
          | (k, v) <- MM.assocs (acLookup ctx)
          ]
   in flip execState ctx {acLookup = MM.fromList tm} $
        traverse
          (uncurry desugarRange)
          -- TODO: @v@ should belong to either of segments
          [ (toVar v, assert (length k == 1) (length k) (head k))
          | (S.toList -> k, s) <- rm
          , [v] <- S.toList s
          ]
 where
  desugarRange :: Var a -> (a, a) -> State (CircuitContext a o) ()
  desugarRange i (a, b)
    | a /= zero = error "non-zero lower bound not supported yet"
    | b == negate one = return ()
    | regBound == toConstant b = rangeConstraint i regBound
    | otherwise = do
        let base = regBound + 1
            bs =
              unfoldr
                (\x -> guard (x /= 0) >> Just (swap $ divMod x base))
                (toConstant b)
            ws =
              unfoldr
                ( Just
                    . swap
                    . fmap fromConstant
                    . (`divMod` fromConstant base)
                )
                $ toIntegral (at i)
        is <- newRanged regBound `traverse` take (length bs) ws
        sm <-
          foldrM
            (\j s -> newAssigned \w -> w j + base `scale` w s)
            (fromConstant (zero :: a))
            is
        constraint \w -> w sm - w i
        o <-
          zipWithM (\c j -> newAssigned \w -> fromConstant c - w j) bs is
            >>= flip foldrM (fromConstant (zero :: a)) \d o -> do
              d' <-
                newRanged regBound $
                  fromConstant $
                    toIntegral (at d) `mod` fromConstant base
              isLE <- isEq d d'
              isEQ <- isEq d (fromConstant regBound)
              --  o | isLE | isEQ | result
              -- ---+------+------+-------
              -- -1 |   *  |   *  |   -1
              --  1 |   *  |   *  |    1
              --  0 |   1  |   1  |    0
              --  0 |   1  |   0  |   -1
              --  0 |   0  |   0  |    1
              --
              -- o + (1 - o^2) (1 + isLE (isEQ - 2))
              oIsZ <- newAssigned \w -> one - w o * w o
              cmp <- newAssigned \w -> one + w isLE * (w isEQ - one - one)
              addend <- newAssigned \w -> w oIsZ * w cmp
              newAssigned \w -> w o + w addend
        constraint \w -> w o * (w o + one)

  isEq :: Var a -> Var a -> State (CircuitContext a o) (Var a)
  isEq x y = do
    d <- newAssigned \w -> w x - w y
    isZ <-
      newConstrained (\w i -> w d * w i) $
        ifThenElse (at d == zero) one zero
    constraint \w -> w isZ * (one - w isZ)
    pure isZ
