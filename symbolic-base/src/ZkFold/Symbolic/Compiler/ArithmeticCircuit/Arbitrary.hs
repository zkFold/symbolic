{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Arbitrary (arbitraryContext) where

import Control.Monad (foldM, return)
import Control.Monad.State (execState)
import Data.Binary (Binary)
import Data.Function (flip, ($))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (Representable)
import Data.List ((++))
import Data.Monoid (mempty)
import Data.Traversable (Traversable)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements)
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Prelude (elementsRep, replicateA)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext (..), crown, getAllVars)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var (CircuitWitness, NewVar, Var, toVar)
import ZkFold.Symbolic.MonadCircuit

------------------------------------- Instances -------------------------------------

arbitraryContext
  :: (Arbitrary a, Arithmetic a, Binary a, Representable o, Traversable o)
  => [NewVar] -> Natural -> Gen (CircuitContext a o)
arbitraryContext vars nconstraints = do
  acts <-
    replicateA nconstraints $
      elements
        [ arbitraryPolynomialConstraint vars
        , arbitraryLookupConstraint vars
        ]
  ac <- foldM (flip ($)) mempty acts
  out <- fmap toVar <$> elementsRep (vars ++ getAllVars ac)
  return $ ac `crown` out

-- | Add a random Plonk constraint to the circuit.
-- TODO: generalize the constraint
arbitraryPolynomialConstraint
  :: forall a o
   . (Arbitrary a, Arithmetic a, Binary a)
  => [NewVar] -> CircuitContext a o -> Gen (CircuitContext a o)
arbitraryPolynomialConstraint inVars ac = do
  (qm, ql, qr, qo, qc) <- arbitrary :: Gen (a, a, a, a, a)
  let vars = inVars ++ getAllVars ac
  l <- toVar <$> elements vars
  r <- toVar <$> elements vars
  let p :: ClosedPoly (Var a) a
      p x = scale qm (x l * x r) + scale ql (x l) + scale qr (x r) + fromConstant qc
  return $ flip execState ac do
    newConstrained
      (\x o -> p x + scale qo (x o))
      (negate $ p at // fromConstant qo)

-- | Add a random range constraint to the circuit.
-- TODO: generalize the constraint
arbitraryLookupConstraint
  :: forall a o
   . (Arithmetic a, Binary a)
  => [NewVar] -> CircuitContext a o -> Gen (CircuitContext a o)
arbitraryLookupConstraint inVars ac = do
  v <- toVar <$> elements (inVars ++ getAllVars ac)
  let b = fromConstant (7 :: Natural)
  return $
    flip execState ac $
      newRanged b $
        fromIntegral $
          toIntegral (at v :: CircuitWitness a)
            `mod` toIntegral (fromConstant (b + one) :: CircuitWitness a)
