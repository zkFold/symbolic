{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Base.Protocol.IVC.SpecialSound where

import           Data.Function                         (($))
import           Data.Map.Strict                       (elems)
import           Data.Type.Equality                    (type (~))
import           GHC.Generics                          ((:*:) (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.Vector               (Vector)
import           ZkFold.Base.Protocol.IVC.AlgebraicMap
import           ZkFold.Base.Protocol.IVC.Predicate    (Predicate (..))
import           ZkFold.Symbolic.Class                 (Symbolic (..))
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Data.Class            (LayoutFunctor)
import           ZkFold.Symbolic.MonadCircuit          (ResidueField)
import Prelude (error)

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
      => c i  -- ^ previous public input
      -> c p  -- ^ witness
      -> c i  -- ^ public input

  , prover :: forall f. (ResidueField f, Algebra a f)
      => i f      -- ^ previous public input
      -> p f      -- ^ witness
      -> f        -- ^ current random challenge
      -> Natural  -- ^ round number (starting from 1)
      -> [f]      -- ^ prover message

  , verifier :: forall f. (FiniteField f, Algebra a f)
      => i f             -- ^ public input
      -> Vector k [f]    -- ^ prover messages
      -> Vector (k-1) f  -- ^ random challenges
      -> [f]             -- ^ verifier output
  }

specialSoundProtocol :: forall d a i p .
    ( KnownNat (d+1)
    , LayoutFunctor i
    , LayoutFunctor p
    ) => Predicate a i p -> SpecialSoundProtocol 1 a i p
specialSoundProtocol phi@Predicate {..} = SpecialSoundProtocol
    { input = predicateFunction
    , prover = \pi0 w _ _ -> elems $
        witnessGenerator' predicateCircuit (pi0 :*: w)
          (error "input should not be accessed")
    , verifier = \pi pm ts -> algebraicMap @d phi pi pm ts one
    }
