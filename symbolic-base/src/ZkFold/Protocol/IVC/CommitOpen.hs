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
  :: (Functor i, Functor p, HomomorphicCommit c, m ~ [ScalarFieldOf c])
  => (a -> f)
  -> (f -> a)
  -> SpecialSoundProtocol k i p m o a
  -> CommitOpen k i p c m o f
commitOpen af fa SpecialSoundProtocol {..} =
  SpecialSoundProtocol
    { input = \i p -> af <$> input (fa <$> i) (fa <$> p)
    , prover = \pi0 w r i ->
        let m = prover (fmap fa pi0) (fmap fa w) (fa r) i
         in (m, hcommit m)
    , verifier = \pi pms rs ->
        let ms = fmap fst pms
            cs = fmap snd pms
         in ( zipWith (-) (fmap hcommit ms) cs
            , verifier (fmap fa pi) ms (fmap fa rs)
            )
    }
