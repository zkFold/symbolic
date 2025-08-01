{-# LANGUAGE TypeOperators #-}

{- HLINT ignore "Redundant ^." -}

module ZkFold.Protocol.IVC.AccumulatorScheme where

import Control.Lens ((^.))
import Data.Binary (Binary)
import Data.Constraint (withDict)
import Data.Constraint.Nat (plusMinusInverse1)
import Data.Foldable (Foldable)
import Data.Functor.Rep (Representable (..), mzipWithRep)
import Data.Zip (Zip (..))
import Prelude (fmap, ($), (<$>), type (~))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (ScalarFieldOf)
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate (polyVecLinear)
import ZkFold.Algebra.Polynomial.Univariate.Simple (SimplePoly, toVector)
import ZkFold.Data.Vector (Vector, init, mapWithIx, tail)
import ZkFold.Protocol.IVC.Accumulator
import ZkFold.Protocol.IVC.AlgebraicMap (algebraicMap)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit (..))
import ZkFold.Protocol.IVC.FiatShamir (transcript)
import ZkFold.Protocol.IVC.NARK (NARKInstanceProof (..), NARKProof (..))
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.Predicate (Predicate)
import ZkFold.Symbolic.Data.Class (LayoutData (LayoutData), layoutData)

-- | Accumulator scheme for V_NARK as described in Chapter 3.4 of the Protostar paper
data AccumulatorScheme d k i c f = AccumulatorScheme
  { prover
      :: Accumulator k i c f -- accumulator
      -> NARKInstanceProof k i c f -- instance-proof pair (pi, π)
      -> (Accumulator k i c f, Vector (d - 1) c) -- updated accumulator and accumulation proof
  , verifier
      :: i f -- Public input
      -> Vector k c -- NARK proof π.x
      -> AccumulatorInstance k i c f -- accumulator instance acc.x
      -> Vector (d - 1) c -- accumulation proof E_j
      -> AccumulatorInstance k i c f -- updated accumulator instance acc'.x
  , decider
      :: Accumulator k i c f -- final accumulator
      -> (Vector k c, c) -- returns zeros if the final accumulator is valid
  }

accumulatorScheme
  :: forall d c k a i p f
   . ( KnownNat (d - 1)
     , KnownNat (d + 1)
     , Representable i
     , Foldable i
     , Representable p
     , Foldable p
     , OracleSource f f
     , OracleSource f c
     , HomomorphicCommit c
     , Field f
     , Scale a f
     , Scale a (SimplePoly f (d + 1))
     , Scale f c
     , Binary (Rep i)
     , Binary (Rep p)
     , f ~ ScalarFieldOf c
     )
  => Hasher
  -> Predicate a i p
  -> AccumulatorScheme d k i c f
accumulatorScheme hash phi =
  let
    prover acc (NARKInstanceProof pubi (NARKProof pi_x pi_w)) =
      let
        r_0 :: f
        r_0 = oracle hash (FoldableSource pubi)

        -- Fig. 3, step 1
        r_i :: Vector (k - 1) f
        r_i = transcript hash r_0 pi_x

        -- Fig. 3, step 2

        -- X + mu as a univariate polynomial
        polyMu :: SimplePoly f (d + 1)
        polyMu = polyVecLinear one (acc ^. x ^. mu)

        -- X * pi + pi' as a list of univariate polynomials
        polyPi :: i (SimplePoly f (d + 1))
        polyPi = mzipWithRep polyVecLinear pubi (layoutData $ acc ^. x ^. pi)

        -- X * mi + mi'
        polyW :: Vector k [SimplePoly f (d + 1)]
        polyW = zipWith (zipWith polyVecLinear) pi_w (acc ^. w)

        -- X * ri + ri'
        polyR :: Vector (k - 1) (SimplePoly f (d + 1))
        polyR = zipWith (P.flip polyVecLinear) (acc ^. x ^. r) r_i

        -- The @l x d+1@ matrix of coefficients
        -- as a vector of @l@ univariate degree-@d@ polynomials
        e_uni :: [Vector (d + 1) f]
        e_uni = toVector <$> algebraicMap @d phi polyPi polyW polyR polyMu

        -- e_all are coefficients of degree-j homogenous polynomials
        -- where j is from the range [0, d]
        e_all :: Vector (d + 1) [f]
        e_all = tabulate (\i -> fmap (`index` i) e_uni)

        -- e_j are coefficients of degree-j homogenous polynomials
        -- where j is from the range [1, d - 1]
        e_j :: Vector (d - 1) [f]
        e_j = withDict (plusMinusInverse1 @1 @d) $ tail $ init e_all

        -- Fig. 3, step 3
        pf = hcommit <$> e_j

        -- Fig. 3, step 4
        alpha :: f
        alpha = oracle hash (acc ^. x, FoldableSource pubi, pi_x, pf)

        -- Fig. 3, steps 5, 6
        mu' = alpha + acc ^. x ^. mu
        pi'' = mzipWithRep (+) (fmap (* alpha) pubi) (layoutData $ acc ^. x ^. pi)
        ri'' = scale alpha r_i + acc ^. x ^. r
        ci'' = fmap (scale alpha) pi_x + acc ^. x ^. c
        m_i'' = zipWith (+) (scale alpha pi_w) (acc ^. w)

        -- Fig. 3, step 7
        eCapital' = acc ^. x ^. e + sum (mapWithIx (\i a -> scale (alpha ^ (i + 1)) a) pf)
       in
        (Accumulator (AccumulatorInstance (LayoutData pi'') ci'' ri'' eCapital' mu') m_i'', pf)

    verifier pubi pi_x acc pf =
      let
        r_0 :: f
        r_0 = oracle hash (FoldableSource pubi)

        -- Fig. 4, step 1
        r_i :: Vector (k - 1) f
        r_i = transcript hash r_0 pi_x

        -- Fig. 4, step 2
        alpha :: f
        alpha = oracle hash (acc, FoldableSource pubi, pi_x, pf)

        -- Fig. 4, steps 3-4
        mu' = alpha + acc ^. mu
        pi'' = mzipWithRep (+) (fmap (* alpha) pubi) (layoutData $ acc ^. pi)
        ri'' = zipWith (+) (scale alpha r_i) (acc ^. r)
        ci'' = zipWith (+) (fmap (scale alpha) pi_x) (acc ^. c)

        -- Fig 4, step 5
        e' = acc ^. e + sum (mapWithIx (\i a -> scale (alpha ^ (i + 1)) a) pf)
       in
        AccumulatorInstance {_pi = LayoutData pi'', _c = ci'', _r = ri'', _e = e', _mu = mu'}

    decider acc =
      let
        -- Fig. 5, step 1
        commitsDiff = zipWith (\cm m_acc -> cm - hcommit m_acc) (acc ^. x ^. c) (acc ^. w)

        -- Fig. 5, step 2
        err :: [f]
        err = algebraicMap @d phi (layoutData $ acc ^. x ^. pi) (acc ^. w) (acc ^. x ^. r) (acc ^. x ^. mu)

        -- Fig. 5, step 3
        eDiff = (acc ^. x ^. e) - hcommit err
       in
        (commitsDiff, eDiff)
   in
    AccumulatorScheme prover verifier decider
