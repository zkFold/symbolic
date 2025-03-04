{-# LANGUAGE TypeOperators #-}

module ZkFold.Base.Protocol.IVC.CommitOpen where

import           Data.Zip                              (zipWith)
import           Prelude                               hiding (Num (..), length, pi, tail, zipWith, (&&))

import           ZkFold.Base.Algebra.Basic.Class       (AdditiveGroup (..), Algebra, FiniteField)
import           ZkFold.Base.Algebra.Basic.Number      (Natural, type (-))
import           ZkFold.Base.Data.Vector               (Vector)
import           ZkFold.Base.Protocol.IVC.Commit       (HomomorphicCommit (hcommit))
import           ZkFold.Base.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))
import           ZkFold.Symbolic.MonadCircuit          (ResidueField)
import ZkFold.Symbolic.Class (Symbolic(..))

data CommitOpen k a i p c = CommitOpen
    {
      input :: forall ctx .
        (Symbolic ctx, BaseField ctx ~ a) =>
        ctx i -> ctx p -> ctx i

    , prover :: forall f .
        (ResidueField f, Algebra a f) =>
        (HomomorphicCommit [f] (c f)) =>
        i f -> p f -> f -> Natural -> ([f], c f)

    , verifier :: forall f .
        (FiniteField f, Algebra a f) =>
        (HomomorphicCommit [f] (c f)) =>
        i f -> Vector k ([f], c f) -> Vector (k-1) f -> ([f], Vector k (c f))
    }

commitOpen :: SpecialSoundProtocol k a i p -> CommitOpen k a i p c
commitOpen SpecialSoundProtocol {..} = CommitOpen
  { input = input
  , prover = \pi0 w r i ->
      let m = prover pi0 w r i
       in (m, hcommit m)
  , verifier = \pi pms rs ->
      let ms = fmap fst pms
          cs = fmap snd pms
      in (verifier pi ms rs, zipWith (-) (fmap hcommit ms) cs)
  }
