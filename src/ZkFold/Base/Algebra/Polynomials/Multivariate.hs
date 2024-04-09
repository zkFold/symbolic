{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Base.Algebra.Polynomials.Multivariate
  ( module ZkFold.Base.Algebra.Polynomials.Multivariate.Polynomial,
    module ZkFold.Base.Algebra.Polynomials.Multivariate.Monomial,
    module ZkFold.Base.Algebra.Polynomials.Multivariate.Set,
    module ZkFold.Base.Algebra.Polynomials.Multivariate.Substitution,
    SomeMonomial,
    SomePolynomial,
    monomial,
    polynomial,
    evalPolynomial,
    evalPolynomial',
    substitutePolynomial,
    var,
    variables,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Map (Map, keys, singleton, toList)
import Data.Maybe (fromJust)
import Numeric.Natural (Natural)
import ZkFold.Base.Algebra.Basic.Class
import ZkFold.Base.Algebra.Basic.Scale (Self (..))
import ZkFold.Base.Algebra.Polynomials.Multivariate.Monomial
import ZkFold.Base.Algebra.Polynomials.Multivariate.Polynomial
import ZkFold.Base.Algebra.Polynomials.Multivariate.Set
import ZkFold.Base.Algebra.Polynomials.Multivariate.Substitution
import Prelude hiding (Num (..), length, product, replicate, sum, (!!), (^))

-- | Most general type for a multivariate monomial
type SomeMonomial = M Natural Natural (Map Natural Natural)

-- | Most general type for a multivariate polynomial
type SomePolynomial c = P c Natural Natural (Map Natural Natural) [(c, M Natural Natural (Map Natural Natural))]

-- | Monomial constructor
monomial :: (Monomial i j) => Map i j -> M i j (Map i j)
monomial = M . fromJust . toMonomial

-- | Polynomial constructor
polynomial :: (Polynomial c i j) => [(c, M i j (Map i j))] -> P c i j (Map i j) [(c, M i j (Map i j))]
polynomial = sum . map (\m -> P [m]) . fromJust . toPolynomial

-- | @'var' i@ is a polynomial \(p(x) = x_i\)
var :: (Polynomial c i j) => i -> P c i j (Map i j) [(c, M i j (Map i j))]
var x = polynomial [(one, monomial (singleton x one))]

evalMonomial :: forall i j m b. (FromMonomial i j m, Exponent b j) => (i -> b) -> M i j m -> b
evalMonomial f (M m) = product (map (\(i, j) -> f i ^ j) (toList $ fromMonomial @i @j m))

evalPolynomial ::
  forall c i j m p b.
  (FromMonomial i j m, FromPolynomial c i j m p, Algebra b c, Exponent b j) =>
  (i -> b) ->
  P c i j m p ->
  b
evalPolynomial f (P p) = sum $ map (\(c, m) -> scale c (evalMonomial f m)) (fromPolynomial @c @i @j @m @p p)

evalPolynomial' ::
  forall c i j m p.
  (FromMonomial i j m, FromPolynomial c i j m p, BinaryExpansion j) =>
  (i -> c) ->
  P c i j m p ->
  c
evalPolynomial' f = getSelf . evalPolynomial (Self . f)

substitutePolynomial ::
  forall c i i' j j' m m' p p'.
  (BinaryExpansion j, FromMonomial i j m, FromPolynomial c i j m p, FromPolynomial c i' j' m' p', m' ~ Map i' j', p' ~ [(c, M i' j' m')]) =>
  (i -> P c i' j' m' p') ->
  P c i j m p ->
  P c i' j' m' p'
substitutePolynomial = evalPolynomial

variables :: forall c i j m p. (FromMonomial i j m, FromPolynomial c i j m p) => P c i j m p -> [i]
variables (P p) = nubOrd $ concatMap (\(_, M m) -> keys (fromMonomial @i @j @m m)) $ fromPolynomial @c @i @j @m @p p
