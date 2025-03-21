{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ^." #-}

module ZkFold.Base.Protocol.IVC.AccumulatorScheme where

import           Control.Lens                               ((^.))
import           Data.Constraint                            (withDict)
import           Data.Constraint.Nat                        (plusMinusInverse1)
import           Data.Functor.Rep                           (Representable (..))
import           Data.Zip                                   (Zip (..))
import           GHC.IsList                                 (IsList (..))
import           Prelude                                    (fmap, ($), (.), (<$>))
import qualified Prelude                                    as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import qualified ZkFold.Base.Algebra.Polynomials.Univariate as PU
import           ZkFold.Base.Data.Vector                    (Vector, init, mapWithIx, tail, unsafeToVector)
import           ZkFold.Base.Protocol.IVC.Accumulator
import           ZkFold.Base.Protocol.IVC.AlgebraicMap      (algebraicMap)
import           ZkFold.Base.Protocol.IVC.Commit            (HomomorphicCommit (..))
import           ZkFold.Base.Protocol.IVC.FiatShamir        (transcript)
import           ZkFold.Base.Protocol.IVC.NARK              (NARKInstanceProof (..), NARKProof (..))
import           ZkFold.Base.Protocol.IVC.Oracle            (HashAlgorithm, RandomOracle (..))
import           ZkFold.Base.Protocol.IVC.Predicate         (Predicate)

-- | Accumulator scheme for V_NARK as described in Chapter 3.4 of the Protostar paper
data AccumulatorScheme d k i c f = AccumulatorScheme
  {
    prover   ::
               Accumulator k i c f                          -- accumulator
            -> NARKInstanceProof k i c f                    -- instance-proof pair (pi, π)
            -> (Accumulator k i c f, Vector (d-1) (c f))    -- updated accumulator and accumulation proof

  , verifier :: i f                                         -- Public input
            -> Vector k (c f)                               -- NARK proof π.x
            -> AccumulatorInstance k i c f                  -- accumulator instance acc.x
            -> Vector (d-1) (c f)                           -- accumulation proof E_j
            -> AccumulatorInstance k i c f                  -- updated accumulator instance acc'.x

  , decider  ::
               Accumulator k i c f                          -- final accumulator
            -> (Vector k (c f), c f)                        -- returns zeros if the final accumulator is valid
  }

accumulatorScheme :: forall algo d k a i p c f .
    ( KnownNat (d-1)
    , KnownNat (d+1)
    , Representable i
    , Zip i
    , HashAlgorithm algo f
    , RandomOracle algo f f
    , RandomOracle algo (i f) f
    , RandomOracle algo (c f) f
    , HomomorphicCommit [f] (c f)
    , Field f
    , Scale a f
    , Scale a (PU.PolyVec f (d+1))
    , Scale f (c f)
    )
    => Predicate a i p
    -> AccumulatorScheme d k i c f
accumulatorScheme phi =
  let
      prover acc (NARKInstanceProof pubi (NARKProof pi_x pi_w)) =
        let
            r_0 :: f
            r_0 = oracle @algo pubi

            -- Fig. 3, step 1
            r_i :: Vector (k-1) f
            r_i = transcript @algo r_0 pi_x

            -- Fig. 3, step 2

            -- X + mu as a univariate polynomial
            polyMu :: PU.PolyVec f (d+1)
            polyMu = PU.polyVecLinear one (acc^.x^.mu)

            -- X * pi + pi' as a list of univariate polynomials
            polyPi :: i (PU.PolyVec f (d+1))
            polyPi = zipWith PU.polyVecLinear pubi (acc^.x^.pi)

            -- X * mi + mi'
            polyW :: Vector k [PU.PolyVec f (d+1)]
            polyW = zipWith (zipWith PU.polyVecLinear) pi_w (acc^.w)

            -- X * ri + ri'
            polyR :: Vector (k-1) (PU.PolyVec f (d+1))
            polyR = zipWith (P.flip PU.polyVecLinear) (acc^.x^.r) r_i

            -- The @l x d+1@ matrix of coefficients as a vector of @l@ univariate degree-@d@ polynomials
            --
            e_uni :: [Vector (d+1) f]
            e_uni = unsafeToVector . toList <$> algebraicMap @d phi polyPi polyW polyR polyMu

            -- e_all are coefficients of degree-j homogenous polynomials where j is from the range [0, d]
            e_all :: Vector (d+1) [f]
            e_all = tabulate (\i -> fmap (`index` i) e_uni)

            -- e_j are coefficients of degree-j homogenous polynomials where j is from the range [1, d - 1]
            e_j :: Vector (d-1) [f]
            e_j = withDict (plusMinusInverse1 @1 @d) $ tail $ init e_all

            -- Fig. 3, step 3
            pf = hcommit <$> e_j

            -- Fig. 3, step 4
            alpha :: f
            alpha = oracle @algo (acc^.x, pubi, pi_x, pf)

            -- Fig. 3, steps 5, 6
            mu'   = alpha + acc^.x^.mu
            pi''  = zipWith (+) (fmap (* alpha) pubi) (acc^.x^.pi)
            ri''  = scale alpha r_i  + acc^.x^.r
            ci''  = scale alpha pi_x + acc^.x^.c
            m_i'' = zipWith (+) (scale alpha pi_w) (acc^.w)

            -- Fig. 3, step 7
            eCapital' = acc^.x^.e + sum (mapWithIx (\i a -> scale (alpha ^ (i+1)) a) pf)
        in
            (Accumulator (AccumulatorInstance pi'' ci'' ri'' eCapital' mu') m_i'', pf)

      verifier pubi pi_x acc pf =
        let
            r_0 :: f
            r_0 = oracle @algo pubi

            -- Fig. 4, step 1
            r_i :: Vector (k-1) f
            r_i = transcript @algo r_0 pi_x

            -- Fig. 4, step 2
            alpha :: f
            alpha = oracle @algo (acc, pubi, pi_x, pf)

            -- Fig. 4, steps 3-4
            mu'  = alpha + acc^.mu
            pi'' = zipWith (+) (fmap (* alpha) pubi) (acc^.pi)
            ri'' = zipWith (+) (scale alpha r_i)     (acc^.r)
            ci'' = zipWith (+) (scale alpha pi_x)    (acc^.c)

            -- Fig 4, step 5
            e' = acc^.e + sum (mapWithIx (\i a -> scale (alpha ^ (i+1)) a) pf)
        in
            AccumulatorInstance { _pi = pi'', _c = ci'', _r = ri'', _e = e', _mu = mu' }

      decider acc =
        let
            -- Fig. 5, step 1
            commitsDiff = zipWith (\cm m_acc -> cm - hcommit m_acc) (acc^.x^.c) (acc^.w)

            -- Fig. 5, step 2
            err :: [f]
            err = algebraicMap @d phi (acc^.x^.pi) (acc^.w) (acc^.x^.r) (acc^.x^.mu)


            -- Fig. 5, step 3
            eDiff = (acc^.x^.e) - hcommit err
        in
            (commitsDiff, eDiff)

  in
      AccumulatorScheme prover verifier decider
