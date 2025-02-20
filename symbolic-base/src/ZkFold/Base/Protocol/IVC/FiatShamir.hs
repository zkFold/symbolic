{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Base.Protocol.IVC.FiatShamir where

import           Data.Constraint                     (withDict)
import           Data.Constraint.Nat                 (plusMinusInverse1)
import           Prelude                             hiding (Bool (..), Eq (..), init, length, pi, scanl, unzip)

import           ZkFold.Base.Algebra.Basic.Class     (Ring, FromConstant, Scale)
import           ZkFold.Base.Algebra.Basic.Number    (KnownNat, type (-))
import           ZkFold.Base.Data.Vector             (Vector, init, scanl, unfold)
import           ZkFold.Base.Protocol.IVC.Commit     (HomomorphicCommit)
import           ZkFold.Base.Protocol.IVC.CommitOpen
import           ZkFold.Base.Protocol.IVC.Oracle     (HashAlgorithm, oracle)
import           ZkFold.Symbolic.Class               (Symbolic (..))
import           ZkFold.Symbolic.Data.FieldElement   (FieldElement)
import           ZkFold.Symbolic.Data.FieldElementW  (FieldElementW)

data FiatShamir k a i p c = FiatShamir
    {
        input :: forall ctx . (Symbolic ctx, BaseField ctx ~ a)
            => i (FieldElementW ctx)
            -> p (FieldElementW ctx)
            -> i (FieldElementW ctx)

      , prover  :: forall ctx . (Symbolic ctx, BaseField ctx ~ a, FromConstant a (FieldElementW ctx), Scale a (FieldElementW ctx), HomomorphicCommit [FieldElementW ctx] (c (FieldElementW ctx)))
            => i (FieldElementW ctx)
            -> p (FieldElementW ctx)
            -> Vector k ([FieldElementW ctx], c (FieldElementW ctx))

      , verifier :: forall ctx . (Symbolic ctx, BaseField ctx ~ a, HomomorphicCommit [FieldElement ctx] (c (FieldElement ctx)))
            => i (FieldElement ctx)
            -> Vector k ([FieldElement ctx], c (FieldElement ctx))
            -> ([FieldElement ctx], Vector k (c (FieldElement ctx)))
    }

-- The transcript of the Fiat-Shamired protocol (ignoring the last round)
transcript :: forall algo k c f .
    ( HashAlgorithm algo
    , Ring f
    , Foldable c
    ) => f -> Vector k (c f) -> Vector (k-1) f
transcript r0 cs = withDict (plusMinusInverse1 @1 @k) $ init $ init $ scanl (curry (oracle @algo)) r0 cs

fiatShamir :: forall algo k a i p c .
    ( HashAlgorithm algo
    , KnownNat k
    , Foldable i
    , Foldable c
    ) => CommitOpen k a i p c -> FiatShamir k a i p c
fiatShamir CommitOpen {..} =
    let
        prover' :: forall ctx . (Symbolic ctx, BaseField ctx ~ a, FromConstant a (FieldElementW ctx), Scale a (FieldElementW ctx), HomomorphicCommit [FieldElementW ctx] (c (FieldElementW ctx)))
            => i (FieldElementW ctx)
            -> p (FieldElementW ctx)
            -> Vector k ([FieldElementW ctx], c (FieldElementW ctx))
        prover' pi0 w =
            let r0 = oracle @algo pi0
                f (r, k) =
                    let (m', c') = prover pi0 w r k
                    in ((m', c'), (oracle @algo (r, c'), k + 1))
            in unfold f (r0, 1)

        verifier' :: forall ctx . (Symbolic ctx, BaseField ctx ~ a, HomomorphicCommit [FieldElement ctx] (c (FieldElement ctx)))
            => i (FieldElement ctx)
            -> Vector k ([FieldElement ctx], c (FieldElement ctx))
            -> ([FieldElement ctx], Vector k (c (FieldElement ctx)))
        verifier' pi pms =
            let r0 = oracle @algo pi
                rs = transcript @algo r0 $ fmap snd pms
            in verifier pi pms rs
    in
        FiatShamir input prover' verifier'
