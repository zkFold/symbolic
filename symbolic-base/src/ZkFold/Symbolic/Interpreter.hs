{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Interpreter (Interpreter (..), Atop (..)) where

import           Control.Applicative              (Applicative)
import           Control.DeepSeq                  (NFData)
import           Control.Monad                    (Monad, return)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Eq                          (Eq)
import           Data.Function                    (($), (.), id)
import           Data.Functor                     (Functor, (<$>))
import           Data.Functor.Identity            (Identity (..))
import           Data.List                        (foldl')
import           Data.List.Infinite               (toList)
import           Data.Tuple                       (uncurry)
import           GHC.Generics                     (Generic, Par1 (..))
import           Text.Show                        (Show)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Control.HApplicative
import           ZkFold.Base.Data.HFunctor
import           ZkFold.Base.Data.Package
import           ZkFold.Prelude                   (take)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Fold
import           ZkFold.Symbolic.MonadCircuit

newtype Interpreter a f = Interpreter { runInterpreter :: f a }
    deriving (Eq, Show, Generic, NFData)
    deriving newtype (FromJSON, ToJSON)

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
  fromCircuitF (Interpreter x) c = Interpreter $ runWitnesses @a @a (c x)
  sanityF (Interpreter x) f _ = Interpreter (f x)

instance Arithmetic a => SymbolicFold (Interpreter a) where
  sfoldl fun seed pload _ stream (Interpreter (Par1 cnt)) =
    foldl' ((. Interpreter) . uncurry fun) (seed, pload)
      $ take (toConstant cnt) $ toList stream

-- | TODO: Integrate with Interpreter
newtype Atop a b f = Atop { runAtop :: f b }
  deriving (HFunctor, HApplicative) via (Interpreter b)

instance Package (Atop a b) where
  unpackWith f (Atop x) = Atop <$> f x
  packWith f g = Atop $ f (runAtop <$> g)

instance (Arithmetic a, Algebra a b, ResidueField b, NFData b) =>
    Symbolic (Atop a b) where
  type BaseField (Atop a b) = a
  type WitnessField (Atop a b) = b
  fromCircuitF (Atop f) c = Atop $ runWitnesses @a @b (c f)
  witnessF (Atop f) = f

-- | An example implementation of a @'MonadCircuit'@ which computes witnesses
-- immediately and drops the constraints.
newtype Witnesses a b x = Witnesses { runWitnesses :: x }
  deriving (Functor, Applicative, Monad) via Identity

instance ResidueField a => Witness a a where
  at = id

instance (Arithmetic a, Algebra a b, ResidueField b) =>
    MonadCircuit b a b (Witnesses a b) where
  unconstrained = return
  constraint _ = return ()
  rangeConstraint _ _ = return ()
