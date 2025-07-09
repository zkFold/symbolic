{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.CommitOpen where

import Data.Zip (zipWith)
import ZkFold.Algebra.Class (AdditiveGroup (..))
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (ScalarFieldOf))
import ZkFold.Algebra.Number (Natural, type (-))
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit (hcommit))
import ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))
import Prelude hiding (Num (..), length, pi, tail, zipWith, (&&))

-- type CommitOpen k i p c m o f =
--   SpecialSoundProtocol k i p (m, c) (Vector k c, o) f

data CommitOpen k i p c m o f = CommitOpen
  { input
      :: i f
      -- \^ previous public input
      -> p f
      -- \^ witness
      -> i f
  -- ^ public input
  , prover
      :: i f
      -- \^ previous public input
      -> p f
      -- \^ witness
      -> f
      -- \^ current random challenge
      -> Natural
      -- \^ round number (starting from 1)
      -> (m, c)
  -- ^ prover message
  , verifier
      :: i f
      -- \^ public input
      -> Vector k (m, c)
      -- \^ prover messages
      -> Vector (k - 1) f
      -- \^ random challenges
      -> (Vector k c, o)
  -- ^ verifier output
  }

commitOpen
  :: (HomomorphicCommit c, m ~ [ScalarFieldOf c])
  => SpecialSoundProtocol k i p m o a
  -> CommitOpen k i p c m o a
commitOpen SpecialSoundProtocol {..} =
  CommitOpen
    { input = input
    , prover = \pi0 w r i ->
        let m = prover pi0 w r i
         in (m, hcommit m)
    , verifier = \pi pms rs ->
        let ms = fmap fst pms
            cs = fmap snd pms
         in ( zipWith (-) (fmap hcommit ms) cs
            , verifier pi ms rs
            )
    }
