{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Base.Protocol.IVC.SpecialSound where

import           Data.Function                         (($))
import           Data.Map.Strict                       (elems)
import           GHC.Generics                          ((:*:) (..))
import           Prelude                               (type (~), fmap)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.Vector               (Vector)
import           ZkFold.Base.Protocol.IVC.AlgebraicMap
import           ZkFold.Base.Protocol.IVC.Predicate    (Predicate (..), PredicateFunctionAssumptions)
import           ZkFold.Symbolic.Class                 (Symbolic (..))
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Data.Class            (LayoutFunctor)
import           ZkFold.Symbolic.Data.FieldElement     (FieldElement (..))
import           ZkFold.Symbolic.Data.FieldElementW    (FieldElementW (..), unconstrainFieldElement, constrainFieldElement)

{-- | Section 3.1

The protocol Πsps has 3 essential parameters k, d, l ∈ N, meaning that Πsps is a (2k − 1)-
move protocol with verifier degree d and output length l (i.e. the verifier checks l degree
d algebraic equations). In each round i (1 ≤ i ≤ k), the prover Psps(pi, w, [mj , rj], j=1 to i-1)
generates the next message mi on input the public input pi, the witness w, and the current
transcript [mj , rj], j=1 to i-1, and sends mi to the verifier; the verifier replies with a random
challenge ri ∈ F. After the final message mk, the verifier computes the algebraic map Vsps
and checks that the output is a zero vector of length l.

--}

data SpecialSoundProtocol k a i p = SpecialSoundProtocol
  {
    input :: forall c. (Symbolic c, BaseField c ~ a)
      => i (FieldElementW c)            -- ^ previous public input
      -> p (FieldElementW c)            -- ^ witness
      -> i (FieldElementW c)            -- ^ public input

  , prover :: forall c. (Symbolic c, BaseField c ~ a, FromConstant a (FieldElementW c), Scale a (FieldElementW c))
      => i (FieldElementW c)            -- ^ previous public input
      -> p (FieldElementW c)            -- ^ witness
      -> FieldElementW c                -- ^ current random challenge
      -> Natural                        -- ^ round number (starting from 1)
      -> [FieldElementW c]              -- ^ prover message

  , verifier :: forall c. (Symbolic c, BaseField c ~ a)
      => i (FieldElement c)             -- ^ public input
      -> Vector k [FieldElement c]      -- ^ prover messages
      -> Vector (k-1) (FieldElement c)  -- ^ random challenges
      -> [FieldElement c]               -- ^ verifier output
  }

specialSoundProtocol :: forall d a i p .
    ( KnownNat (d+1)
    , LayoutFunctor i
    , LayoutFunctor p
    ) => Predicate a i p -> SpecialSoundProtocol 1 a i p
specialSoundProtocol phi@Predicate {..} =
  let
      input :: forall c. (Symbolic c, BaseField c ~ a) => i (FieldElementW c) -> p (FieldElementW c) -> i (FieldElementW c)
      input x u =
        let
            x' = fmap constrainFieldElement x
            u' = fmap constrainFieldElement u
        in
          fmap unconstrainFieldElement (predicateFunction x' u')

      prover :: forall c. (Symbolic c, BaseField c ~ a, FromConstant a (FieldElementW c), Scale a (FieldElementW c))
        => i (FieldElementW c) -> p (FieldElementW c) -> FieldElementW c -> Natural -> [FieldElementW c]
      prover pi0 w _ _ = elems $ witnessGenerator' predicateCircuit (pi0 :*: w) (input pi0 w)

      verifier :: forall f . PredicateFunctionAssumptions a f => i f -> Vector 1 [f] -> Vector 0 f -> [f]
      verifier pi pm ts = algebraicMap @d phi pi pm ts one
  in
      SpecialSoundProtocol input prover verifier
