{-# LANGUAGE TypeOperators #-}

module ZkFold.Control.Conditional where

import Data.Bool (Bool)
import qualified Data.Bool as H
import Data.Int (Int)
import Data.Ratio (Rational)
import Data.String (String)
import qualified GHC.Generics as G
import Numeric.Natural (Natural)
import Prelude (Integer)

import ZkFold.Data.Bool

-- TODO: move to ZkFold.Data.Bool

class BoolType b => Conditional b a where
  -- | Properties:
  --
  --   [On true] @bool onFalse onTrue 'true' == onTrue@
  --
  --   [On false] @bool onFalse onTrue 'false' == onFalse@
  bool :: a -> a -> b -> a
  default bool :: (G.Generic a, GConditional b (G.Rep a)) => a -> a -> b -> a
  bool f t b = G.to (gbool (G.from f) (G.from t) b)

ifThenElse :: Conditional b a => b -> a -> a -> a
ifThenElse b x y = bool y x b

instance Conditional Bool Bool where bool = H.bool

instance Conditional Bool String where bool = H.bool

instance Conditional Bool Natural where bool = H.bool

instance Conditional Bool Integer where bool = H.bool

instance Conditional Bool Int where bool = H.bool

instance Conditional Bool Rational where bool = H.bool

instance
  ( Conditional b x0
  , Conditional b x1
  )
  => Conditional b (x0, x1)

instance
  ( Conditional b x0
  , Conditional b x1
  , Conditional b x2
  )
  => Conditional b (x0, x1, x2)

instance
  ( Conditional b x0
  , Conditional b x1
  , Conditional b x2
  , Conditional b x3
  )
  => Conditional b (x0, x1, x2, x3)

class BoolType b => GConditional b u where
  gbool :: u x -> u x -> b -> u x

instance (BoolType b, GConditional b u, GConditional b v) => GConditional b (u G.:*: v) where
  gbool (f0 G.:*: f1) (t0 G.:*: t1) b = gbool f0 t0 b G.:*: gbool f1 t1 b

instance GConditional b v => GConditional b (G.M1 i c v) where
  gbool (G.M1 f) (G.M1 t) b = G.M1 (gbool f t b)

instance Conditional b x => GConditional b (G.Rec0 x) where
  gbool (G.K1 f) (G.K1 t) b = G.K1 (bool f t b)
