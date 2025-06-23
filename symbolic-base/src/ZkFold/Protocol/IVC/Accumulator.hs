{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.Accumulator where

import Control.Lens ((^.))
import Control.Lens.Combinators (makeLenses)
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (Binary)
import Data.Functor.Rep (Representable (..))
import GHC.Generics (Generic)
import ZkFold.Algebra.Class (Ring, Scale, zero)
import ZkFold.Algebra.Number (KnownNat, type (+), type (-))
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.AlgebraicMap (algebraicMap)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit (..))
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.Predicate (Predicate)
import ZkFold.Symbolic.Data.Class (LayoutData (..), LayoutFunctor, SymbolicData (..))
import Prelude hiding (length, pi)

-- Page 19, Accumulator instance
data AccumulatorInstance k i c f = AccumulatorInstance
  { _pi :: LayoutData i f -- pi ∈ M^{l_in} in the paper
  , _c :: Vector k c -- [C_i] ∈ C^k in the paper
  , _r :: Vector (k - 1) f -- [r_i] ∈ F^{k-1} in the paper
  , _e :: c -- E ∈ C in the paper
  , _mu :: f -- μ ∈ F in the paper
  }
  deriving (Eq, Functor, Generic)

makeLenses ''AccumulatorInstance

instance Functor i => Bifunctor (AccumulatorInstance k i) where
  bimap f g AccumulatorInstance {..} =
    AccumulatorInstance
      { _pi = fmap g _pi
      , _c = fmap f _c
      , _r = fmap g _r
      , _e = f _e
      , _mu = g _mu
      }

instance
  (OracleSource a f, OracleSource a c, Foldable i)
  => OracleSource a (AccumulatorInstance k i c f)
  where
  source AccumulatorInstance {..} =
    source (FoldableSource _pi, _c, _r, _e, _mu)

instance
  ( KnownNat (k - 1)
  , KnownNat k
  , LayoutFunctor i
  , SymbolicData c
  , SymbolicData f
  , Context f ~ Context c
  , Support f ~ Support c
  )
  => SymbolicData (AccumulatorInstance k i c f)

-- Page 19, Accumulator
-- @acc.x@ (accumulator instance) from the paper corresponds to _x
-- @acc.w@ (accumulator witness) from the paper corresponds to _w
data Accumulator k i c f = Accumulator
  { _x :: AccumulatorInstance k i c f
  , _w :: Vector k [f]
  }
  deriving (Functor, Generic)

makeLenses ''Accumulator

emptyAccumulator
  :: forall d k a i p c f
   . ( KnownNat (d + 1)
     , KnownNat (k - 1)
     , KnownNat k
     , Representable i
     , HomomorphicCommit [f] c
     , Ring f
     , Scale a f
     , Binary (Rep i)
     , Binary (Rep p)
     )
  => Predicate a i p -> Accumulator k i c f
emptyAccumulator phi =
  let accW = tabulate (const zero)
      aiC = fmap hcommit accW
      aiR = tabulate (const zero)
      aiMu = zero
      aiPI = tabulate (const zero)
      aiE = hcommit $ algebraicMap @d phi aiPI accW aiR aiMu
      accX = AccumulatorInstance {_pi = LayoutData aiPI, _c = aiC, _r = aiR, _e = aiE, _mu = aiMu}
   in Accumulator accX accW

emptyAccumulatorInstance
  :: forall d k a i p c f
   . ( KnownNat (d + 1)
     , KnownNat (k - 1)
     , KnownNat k
     , Representable i
     , HomomorphicCommit [f] c
     , Ring f
     , Scale a f
     , Binary (Rep i)
     , Binary (Rep p)
     )
  => Predicate a i p -> AccumulatorInstance k i c f
emptyAccumulatorInstance phi = emptyAccumulator @d phi ^. x
