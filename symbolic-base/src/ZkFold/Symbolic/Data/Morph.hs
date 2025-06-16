{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Data.Morph where

import           Data.Type.Equality    (type (~))

import           ZkFold.Symbolic.Class (Symbolic (BaseField))
import           ZkFold.Symbolic.Fold  (SymbolicFold)

-- | A type @u@ is a 'Replica' of type @t@ in context @c@ iff:
-- * it is 'SymbolicOutput';
-- * its context is @c@;
-- * its representation (layout and payload) match those of @t@.
type Replica c t u = u ~ t

-- | A function is a "morph" of a function of type @input -> output@
-- /to/ context @c@ iff it is a function of type @i -> o@
-- for some 'Replica's @i@, @o@ of @input@ and @output@, correspondingly,
-- in context @c@.
data MorphTo c input output =
  forall x y. (Replica c input x, Replica c output y) => Morph (x c -> y c)

-- | One can apply a morph of a function @input -> output@ in context @c@
-- if they provide 'Replica's of @input@ and @output@ in context @c@.
(@) ::
  (Replica c input x, Replica c output y) =>
  MorphTo c input output -> x c -> y c
Morph f @ x = f x
  -- let y = f . fromContext $ const (arithmetize x Proxy, payload x Proxy)
  --  in restore $ const (arithmetize y Proxy, payload y Proxy)

-- | A function is a "morph" of a function of type @input -> output@
-- /from/ context @ctx@ iff, for each context @c@ over same base field,
-- it is a morph /to/ @c@.
--
-- Thus, if a function is 'MorphFrom', it can be
-- * safely passed through parametricity boundary set by 'sfoldl';
-- * applied there in an anonymous context to an argument.
type MorphFrom ctx input output =
  forall c.
  (SymbolicFold c, BaseField c ~ BaseField ctx) =>
  MorphTo c input output
