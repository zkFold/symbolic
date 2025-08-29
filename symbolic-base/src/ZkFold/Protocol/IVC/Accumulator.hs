{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.Accumulator where

import Control.Lens ((^.))
import Control.Lens.Combinators (makeLenses)
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (Binary)
import qualified Data.Eq as Haskell
import Data.Foldable (Foldable)
import Data.Function (const)
import Data.Functor (Functor (..), (<$>))
import Data.Functor.Rep (Representable (..), mzipWithRep)
import GHC.Generics (Generic, Generic1)
import Prelude (type (~))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, type (+), type (-))
import ZkFold.Data.Bool (BoolType (..), and)
import ZkFold.Data.Eq (Eq (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.AlgebraicMap (algebraicMap)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit)
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.Predicate (Predicate, Compilable)
import ZkFold.Symbolic.MonadCircuit (ResidueField (..))
import ZkFold.Symbolic.Data.Class (SymbolicData, LayoutFunctor)
import Data.Semialign (Zip)

-- import Prelude hiding (length, pi)

-- Page 19, Accumulator instance
data AccumulatorInstance k i c f = AccumulatorInstance
  { _pi :: i f -- pi ∈ M^{l_in} in the paper
  , _c :: Vector k c -- [C_i] ∈ C^k in the paper
  , _r :: Vector (k - 1) f -- [r_i] ∈ F^{k-1} in the paper
  , _e :: c -- E ∈ C in the paper
  , _mu :: f -- μ ∈ F in the paper
  }
  deriving (Functor, Generic, Haskell.Eq)

data AccumulatorInstance' k i c f ctx = AccumulatorInstance'
  { _pi' :: i (f ctx)
  , _c' :: Vector k (c ctx)
  , _r' :: Vector (k - 1) (f ctx)
  , _e' :: c ctx
  , _mu' :: f ctx
  }
  deriving (Generic1)

instance
  (Zip i, LayoutFunctor i, SymbolicData c, SymbolicData f)
  => SymbolicData (AccumulatorInstance' k i c f)

instance
  FromConstant (AccumulatorInstance k i (c ctx) (f ctx))
               (AccumulatorInstance' k i c f ctx) where
  fromConstant AccumulatorInstance {..} = AccumulatorInstance'
    { _pi' = _pi
    , _c' = _c
    , _r' = _r
    , _e' = _e
    , _mu' = _mu
    }

instance ToConstant (AccumulatorInstance' k i c f ctx) where
  type Const (AccumulatorInstance' k i c f ctx) =
    AccumulatorInstance k i (c ctx) (f ctx)
  toConstant AccumulatorInstance' {..} = AccumulatorInstance
    { _pi = _pi'
    , _c = _c'
    , _r = _r'
    , _e = _e'
    , _mu = _mu'
    }

makeLenses ''AccumulatorInstance

instance (ResidueField f, Compilable i, Eq c, BooleanOf (IntegralOf f) ~ BooleanOf c) => Eq (AccumulatorInstance k i c f) where
  type BooleanOf (AccumulatorInstance k i c f) = BooleanOf (IntegralOf f)
  acc1 == acc2 =
    and (mzipWithRep (==) (toIntegral <$> _pi acc1) (toIntegral <$> _pi acc2))
      && _c acc1
      == _c acc2
      && fmap toIntegral (_r acc1)
      == fmap toIntegral (_r acc2)
      && _e acc1
      == _e acc2
      && toIntegral (_mu acc1)
      == toIntegral (_mu acc2)
  acc1 /= acc2 =
    not (acc1 == acc2)

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
     , Foldable i
     , Representable p
     , Foldable p
     , Zero c
     , Ring f
     , Scale a f
     , Binary (Rep i)
     , Binary (Rep p)
     )
  => HomomorphicCommit f c
  -> Predicate a i p
  -> Accumulator k i c f
emptyAccumulator hcommit phi =
  let accW = tabulate (const zero)
      aiC = tabulate (const zero)
      aiR = tabulate (const zero)
      aiMu = zero
      aiPI = tabulate (const zero)
      aiE = hcommit (algebraicMap @d phi aiPI accW aiR aiMu) zero
      accX = AccumulatorInstance {_pi = aiPI, _c = aiC, _r = aiR, _e = aiE, _mu = aiMu}
   in Accumulator accX accW

emptyAccumulatorInstance
  :: forall d k a i p c f
   . ( KnownNat (d + 1)
     , KnownNat (k - 1)
     , KnownNat k
     , Representable i
     , Foldable i
     , Representable p
     , Foldable p
     , Zero c
     , Ring f
     , Scale a f
     , Binary (Rep i)
     , Binary (Rep p)
     )
  => HomomorphicCommit f c
  -> Predicate a i p
  -> AccumulatorInstance k i c f
emptyAccumulatorInstance hcommit phi = emptyAccumulator @d hcommit phi ^. x
