{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.CommitOpen where

import Data.Zip (zipWith)
import ZkFold.Algebra.Class (AdditiveGroup (..))
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (ScalarFieldOf))
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit (hcommit))
import ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))
import Prelude hiding (Num (..), length, pi, tail, zipWith, (&&))

type CommitOpen k i p c m o f =
  SpecialSoundProtocol k i p (m, c) (Vector k c, o) f

commitOpen
  :: (HomomorphicCommit c, m ~ [ScalarFieldOf c])
  => SpecialSoundProtocol k i p m o a
  -> CommitOpen k i p c m o a
commitOpen SpecialSoundProtocol {..} =
  SpecialSoundProtocol
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
