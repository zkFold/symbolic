{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Protocol.IVC.FiatShamir where

import           Data.Constraint                  (withDict)
import           Data.Constraint.Nat              (plusMinusInverse1)
import           Prelude                          hiding (Bool (..), Eq (..), init, length, pi, scanl, unzip)

import           ZkFold.Algebra.Number            (KnownNat, type (-))
import           ZkFold.Data.Vector               (Vector, init, item, scanl, unfold)
import           ZkFold.Protocol.IVC.CommitOpen
import           ZkFold.Protocol.IVC.Oracle       (HashAlgorithm, RandomOracle (..))
import           ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))

type FiatShamir k i p c m o f = SpecialSoundProtocol 1 i p (Vector k (m, c f)) (Vector k (c f), o) f

-- The transcript of the Fiat-Shamired protocol (ignoring the last round)
transcript :: forall algo k c f .
    ( HashAlgorithm algo f
    , RandomOracle algo f f
    , RandomOracle algo (c f) f
    ) => f -> Vector k (c f) -> Vector (k-1) f
transcript r0 cs = withDict (plusMinusInverse1 @1 @k) $ init $ init $ scanl (curry (oracle @algo)) r0 cs

fiatShamir :: forall algo k i p c m o f .
    ( KnownNat k
    , HashAlgorithm algo f
    , RandomOracle algo f f
    , RandomOracle algo (i f) f
    , RandomOracle algo (c f) f
    ) => CommitOpen k i p c m o f -> FiatShamir k i p c m o f
fiatShamir SpecialSoundProtocol {..} =
    let
        prover' pi0 w _ _ =
            let r0 = oracle @algo pi0
                f (r, k) =
                    let (m', c') = prover pi0 w r k
                    in ((m', c'), (oracle @algo (r, c'), k + 1))
            in unfold f (r0, 1)

        verifier' pi pms' _ =
            let pms = item pms'
                r0 = oracle @algo pi :: f
                rs = transcript @algo r0 $ fmap snd pms
            in verifier pi pms rs
    in
        SpecialSoundProtocol input prover' verifier'
