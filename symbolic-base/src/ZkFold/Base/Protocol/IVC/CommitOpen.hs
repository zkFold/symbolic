{-# LANGUAGE TypeOperators #-}

module ZkFold.Base.Protocol.IVC.CommitOpen where

import           Data.Zip                              (zipWith)
import           Prelude                               hiding (Num (..), length, pi, tail, zipWith, (&&))

import           ZkFold.Base.Algebra.Basic.Class       (AdditiveGroup (..), Scale, FromConstant)
import           ZkFold.Base.Algebra.Basic.Number      (Natural, type (-))
import           ZkFold.Base.Data.Vector               (Vector)
import           ZkFold.Base.Protocol.IVC.Commit       (HomomorphicCommit (hcommit))
import           ZkFold.Base.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))
import           ZkFold.Symbolic.Class                 (Symbolic (..))
import           ZkFold.Symbolic.Data.FieldElement     (FieldElement)
import           ZkFold.Symbolic.Data.FieldElementW    (FieldElementW)

data CommitOpen k a i p c = CommitOpen
    {
        input :: forall ctx . (Symbolic ctx, BaseField ctx ~ a)
            => i (FieldElementW ctx)
            -> p (FieldElementW ctx)
            -> i (FieldElementW ctx)

      , prover :: forall ctx . (Symbolic ctx, BaseField ctx ~ a, FromConstant a (FieldElementW ctx), Scale a (FieldElementW ctx), HomomorphicCommit [FieldElementW ctx] (c (FieldElementW ctx)))
            => i (FieldElementW ctx)
            -> p (FieldElementW ctx)
            -> FieldElementW ctx
            -> Natural
            -> ([FieldElementW ctx], c (FieldElementW ctx))

      , verifier :: forall ctx . (Symbolic ctx, BaseField ctx ~ a, HomomorphicCommit [FieldElement ctx] (c (FieldElement ctx)))
            => i (FieldElement ctx)
            -> Vector k ([FieldElement ctx], c (FieldElement ctx))
            -> Vector (k-1) (FieldElement ctx)
            -> ([FieldElement ctx], Vector k (c (FieldElement ctx)))
    }

commitOpen :: forall k a i p c . SpecialSoundProtocol k a i p -> CommitOpen k a i p c
commitOpen SpecialSoundProtocol {..} =
    let
        prover' ::  forall ctx . (Symbolic ctx, BaseField ctx ~ a, FromConstant a (FieldElementW ctx), Scale a (FieldElementW ctx), HomomorphicCommit [FieldElementW ctx] (c (FieldElementW ctx)))
            => i (FieldElementW ctx)
            -> p (FieldElementW ctx)
            -> FieldElementW ctx
            -> Natural
            -> ([FieldElementW ctx], c (FieldElementW ctx))
        prover' pi0 w r i =
            let m = prover pi0 w r i
            in (m, hcommit m)

        verifier' :: forall ctx . (Symbolic ctx, BaseField ctx ~ a, HomomorphicCommit [FieldElement ctx] (c (FieldElement ctx)))
            => i (FieldElement ctx)
            -> Vector k ([FieldElement ctx], c (FieldElement ctx))
            -> Vector (k-1) (FieldElement ctx)
            -> ([FieldElement ctx], Vector k (c (FieldElement ctx)))
        verifier' pi pms rs =
            let ms = fmap fst pms
                cs = fmap snd pms
            in (verifier pi ms rs, zipWith (-) (fmap hcommit ms) cs)
    in
        CommitOpen input prover' verifier'
