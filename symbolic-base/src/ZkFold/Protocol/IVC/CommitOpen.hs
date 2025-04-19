module ZkFold.Protocol.IVC.CommitOpen where

import           Data.Zip                         (zipWith)
import           Prelude                          hiding (Num (..), length, pi, tail, zipWith, (&&))

import           ZkFold.Algebra.Class             (AdditiveGroup (..))
import           ZkFold.Data.Vector               (Vector)
import           ZkFold.Protocol.IVC.Commit       (HomomorphicCommit (hcommit))
import           ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))

type CommitOpen k i p c m o f = SpecialSoundProtocol k i p (m, c f) (Vector k (c f), o) f

commitOpen :: HomomorphicCommit m (c f) => SpecialSoundProtocol k i p m o f -> CommitOpen k i p c m o f
commitOpen SpecialSoundProtocol {..} =
    let
        prover' pi0 w r i =
            let m = prover pi0 w r i
            in (m, hcommit m)

        verifier' pi pms rs =
            let ms = fmap fst pms
                cs = fmap snd pms
            in (zipWith (-) (fmap hcommit ms) cs, verifier pi ms rs)
    in
        SpecialSoundProtocol input prover' verifier'
