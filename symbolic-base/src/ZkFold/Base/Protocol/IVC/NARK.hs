{-# LANGUAGE TypeOperators #-}

module ZkFold.Base.Protocol.IVC.NARK where

import           Data.Zip                            (unzip)
import           GHC.Generics
import           Prelude                             hiding (head, length, pi, unzip)

import           ZkFold.Base.Algebra.Basic.Class     (FromConstant, Scale)
import           ZkFold.Base.Data.Vector             (Vector)
import           ZkFold.Base.Protocol.IVC.Commit     (HomomorphicCommit)
import           ZkFold.Base.Protocol.IVC.FiatShamir (FiatShamir (..))
import           ZkFold.Symbolic.Class               (Symbolic (..))
import           ZkFold.Symbolic.Data.FieldElementW  (FieldElementW)

-- Page 18, section 3.4, The accumulation predicate
--
data NARKProof k c f
    = NARKProof
        { narkCommits :: Vector k (c f) -- Commits [C_i] ∈  C^k
        , narkWitness :: Vector k [f]   -- prover messages in the special-sound protocol [m_i]
        }
    deriving (Generic)

narkProof :: forall k a i p c ctx . (Symbolic ctx, BaseField ctx ~ a, FromConstant a (FieldElementW ctx), Scale a (FieldElementW ctx), HomomorphicCommit [FieldElementW ctx] (c (FieldElementW ctx)))
    => FiatShamir k a i p c
    -> i (FieldElementW ctx)
    -> p (FieldElementW ctx)
    -> NARKProof k c (FieldElementW ctx)
narkProof FiatShamir {..} pi0 w =
    let (narkWitness, narkCommits) = unzip $ prover pi0 w
    in NARKProof {..}

data NARKInstanceProof k i c f = NARKInstanceProof (i f) (NARKProof k c f)
    deriving (Generic)

narkInstanceProof :: forall k a i p c ctx . (Symbolic ctx, BaseField ctx ~ a, FromConstant a (FieldElementW ctx), Scale a (FieldElementW ctx), HomomorphicCommit [FieldElementW ctx] (c (FieldElementW ctx)))
    => FiatShamir k a i p c
    -> i (FieldElementW ctx)
    -> p (FieldElementW ctx)
    -> NARKInstanceProof k i c (FieldElementW ctx)
narkInstanceProof fs@FiatShamir {..} pi0 w = NARKInstanceProof (input pi0 w) (narkProof fs pi0 w)
