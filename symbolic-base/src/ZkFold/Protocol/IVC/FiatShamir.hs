{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.FiatShamir where

import Data.Constraint (withDict)
import Data.Constraint.Nat (plusMinusInverse1)
import ZkFold.Algebra.Class (Ring)
import ZkFold.Algebra.Number (KnownNat, type (-))
import ZkFold.Data.Vector (Vector, init, item, scanl, unfold)
import ZkFold.Protocol.IVC.CommitOpen
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.SpecialSound (SpecialSoundProtocol (..))
import Prelude hiding (Bool (..), Eq (..), init, length, pi, scanl, unzip)

type FiatShamir k i p c m o f =
  SpecialSoundProtocol 1 i p (Vector k (m, c)) (Vector k c, o) f

-- The transcript of the Fiat-Shamired protocol (ignoring the last round)
transcript
  :: forall k c f
   . (Ring f, OracleSource f f, OracleSource f c)
  => Hasher -> f -> Vector k c -> Vector (k - 1) f
transcript hash r0 cs =
  withDict (plusMinusInverse1 @1 @k) $
    init $
      init $
        scanl (curry (oracle hash)) r0 cs

fiatShamir
  :: forall k i p c m o f
   . (KnownNat k, Ring f, OracleSource f f, OracleSource f c, Foldable i)
  => Hasher -> CommitOpen k i p c m o f -> FiatShamir k i p c m o f
fiatShamir hash SpecialSoundProtocol {..} =
  let
    prover' pi0 w _ _ =
      let r0 = oracle hash (FoldableSource pi0)
          f (r, k) =
            let (m', c') = prover pi0 w r k
             in ((m', c'), (oracle hash (r, c'), k + 1))
       in unfold f (r0, 1)

    verifier' pi pms' _ =
      let pms = item pms'
          r0 = oracle hash (FoldableSource pi)
          rs = transcript hash r0 $ fmap snd pms
       in verifier pi pms rs
   in
    SpecialSoundProtocol input prover' verifier'
