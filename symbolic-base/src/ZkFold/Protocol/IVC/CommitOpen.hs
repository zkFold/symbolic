{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.CommitOpen where

import Data.Zip (zipWith)
import Prelude hiding (Num (..), length, pi, tail, zipWith, (&&))

import ZkFold.Algebra.Class (AdditiveGroup (..))
import ZkFold.Algebra.Number (Natural, type (-))
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit)
import ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))

data CommitOpen k i p c f = CommitOpen
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
      -> ([f], c)
  -- ^ prover message
  , verifier
      :: i f
      -- \^ public input
      -> Vector k ([f], c)
      -- \^ prover messages
      -> Vector (k - 1) f
      -- \^ random challenges
      -> (Vector k c, [f])
  -- ^ verifier output
  }

commitOpen
  :: AdditiveGroup c
  => HomomorphicCommit f c
  -> SpecialSoundProtocol k i p f
  -> CommitOpen k i p c f
commitOpen hcommit SpecialSoundProtocol {..} =
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
