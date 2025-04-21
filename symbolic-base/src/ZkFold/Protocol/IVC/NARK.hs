{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Protocol.IVC.NARK where

import           Control.DeepSeq                  (NFData (..))
import           Data.Zip                         (unzip)
import           GHC.Generics
import           Prelude                          hiding (head, length, pi, unzip)

import           ZkFold.Algebra.Class             (Ring, zero)
import           ZkFold.Data.Vector               (Vector)
import           ZkFold.Protocol.IVC.FiatShamir   (FiatShamir)
import           ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))

-- Page 18, section 3.4, The accumulation predicate
--
data NARKProof k c f
    = NARKProof
        { narkCommits :: Vector k (c f) -- Commits [C_i] ∈  C^k
        , narkWitness :: Vector k [f]   -- prover messages in the special-sound protocol [m_i]
        }
    deriving (Show, Generic, NFData)

narkProof :: Ring f
    => FiatShamir k i p c [f] o f
    -> i f
    -> p f
    -> NARKProof k c f
narkProof a pi0 w =
    let (narkWitness, narkCommits) = unzip $ prover a pi0 w zero 0
    in NARKProof {..}

data NARKInstanceProof k i c f = NARKInstanceProof (i f) (NARKProof k c f)
    deriving (Show, Generic, NFData)

narkInstanceProof :: Ring f
    => FiatShamir k i p c [f] o f
    -> i f
    -> p f
    -> NARKInstanceProof k i c f
narkInstanceProof a pi0 w = NARKInstanceProof (input a pi0 w) (narkProof a pi0 w)
