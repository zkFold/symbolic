{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Interpreter (Interpreter (..)) where

import Control.Applicative (Applicative)
import Control.DeepSeq (NFData (..), NFData1 (..))
import Control.Monad (Monad, return)
import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq (..))
import Data.Function (id, on, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Functor.Identity (Identity (..))
import Data.List (foldl')
import Data.List.Infinite (toList)
import Data.Tuple (uncurry)
import GHC.Generics (Generic, Par1 (..))
import Text.Show (Show (..))
import ZkFold.Algebra.Class
import ZkFold.Control.HApplicative
import ZkFold.Data.HFunctor
import ZkFold.Data.HFunctor.Classes (HEq (..), HNFData (..), HShow (..))
import ZkFold.Data.Package
import ZkFold.Prelude (take)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup (FunctionId (..))
import ZkFold.Symbolic.Fold
import ZkFold.Symbolic.MonadCircuit

newtype Interpreter a f = Interpreter {runInterpreter :: f a}
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

instance Eq a => HEq (Interpreter a) where
  hliftEq f = f (==) `on` runInterpreter

instance Show a => HShow (Interpreter a) where
  hliftShowsPrec f _ p = f showsPrec showList p . runInterpreter

instance (NFData a, NFData1 f) => NFData (Interpreter a f) where
  rnf = hliftRnf liftRnf

instance NFData a => HNFData (Interpreter a) where
  hliftRnf f (Interpreter x) = f rnf x

instance HFunctor (Interpreter a) where
  hmap f (Interpreter x) = Interpreter (f x)

instance HApplicative (Interpreter a) where
  hpure = Interpreter
  hliftA2 f (Interpreter x) (Interpreter y) = Interpreter (f x y)

instance Package (Interpreter a) where
  unpackWith f (Interpreter x) = Interpreter <$> f x
  packWith f g = Interpreter $ f (runInterpreter <$> g)

instance Arithmetic a => Symbolic (Interpreter a) where
  type BaseField (Interpreter a) = a
  type WitnessField (Interpreter a) = a
  witnessF (Interpreter x) = x
  fromCircuitF (Interpreter x) c = Interpreter $ runWitnesses @a (c x)
  sanityF (Interpreter x) f _ = Interpreter (f x)

instance Arithmetic a => SymbolicFold (Interpreter a) where
  sfoldl fun seed pload _ stream (Interpreter (Par1 cnt)) =
    foldl' ((. Interpreter) . uncurry fun) (seed, pload) $
      take (toConstant cnt) $
        toList stream

-- | An example implementation of a @'MonadCircuit'@ which computes witnesses
-- immediately and drops the constraints.
newtype Witnesses a x = Witnesses {runWitnesses :: x}
  deriving (Functor, Applicative, Monad) via Identity

instance Arithmetic a => Witness a a where
  at = id

instance Arithmetic a => MonadCircuit a a a (Witnesses a) where
  unconstrained = return
  constraint _ = return ()
  lookupConstraint _ _ = return ()
  registerFunction _ = return (FunctionId "")
