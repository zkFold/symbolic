{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module ZkFold.Protocol.IVC.Internal where

import           Control.DeepSeq                                    (NFData)
import           Control.Lens                                       ((^.))
import           Control.Lens.Combinators                           (makeLenses)
import           Data.Binary                                        (Binary)
import           Data.Function                                      (const, ($), (.))
import           Data.Functor.Rep                                   (Representable (..))
import           Data.List                                          (map)
import           Data.Type.Equality                                 (type (~))
import           Data.Zip                                           (Zip (..), unzip)
import           GHC.Generics                                       (Generic)
import           Text.Show                                          (Show)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number                              (KnownNat, type (+), type (-))
import           ZkFold.Algebra.Polynomial.Univariate.Simple        (SimplePoly)
import           ZkFold.Data.Vector                                 (Vector, singleton)
import           ZkFold.Protocol.IVC.Accumulator                    hiding (pi)
import qualified ZkFold.Protocol.IVC.AccumulatorScheme              as Acc
import           ZkFold.Protocol.IVC.AccumulatorScheme              (AccumulatorScheme, accumulatorScheme)
import           ZkFold.Protocol.IVC.CommitOpen
import           ZkFold.Protocol.IVC.FiatShamir
import           ZkFold.Protocol.IVC.NARK                           (NARKInstanceProof (..), NARKProof (..))
import           ZkFold.Protocol.IVC.Oracle
import           ZkFold.Protocol.IVC.Predicate                      (Predicate (..), StepFunction, predicate)
import           ZkFold.Protocol.IVC.RecursiveFunction
import           ZkFold.Protocol.IVC.SpecialSound                   (SpecialSoundProtocol (..), specialSoundProtocol,
                                                                     specialSoundProtocol')
import           ZkFold.Symbolic.Class                              (Arithmetic)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit         (ArithmeticCircuit (ArithmeticCircuit), exec1)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext)
import           ZkFold.Symbolic.Data.Class                         (LayoutFunctor)
import           ZkFold.Symbolic.Data.FieldElement                  (FieldElement (..))

-- | The recursion circuit satisfiability proof.
data IVCProof k c f
    = IVCProof
    { _proofX :: Vector k (c f)
    -- ^ The commitment to the witness of the recursion circuit satisfiability proof.
    , _proofW :: Vector k [f]
    -- ^ The witness of the recursion circuit satisfiability proof.
    } deriving (GHC.Generics.Generic, Show, NFData)

makeLenses ''IVCProof

noIVCProof :: forall k c f .
    ( KnownNat k
    , AdditiveMonoid (c f)
    , AdditiveMonoid f
    ) => IVCProof k c f
noIVCProof = IVCProof (tabulate $ const zero) (tabulate $ const zero)

-- | The current result of recursion together with the first iteration flag,
-- the corresponding accumulator, and the recursion circuit satisfiability proof.
data IVCResult k i c f
    = IVCResult
    { _z     :: i f
    , _acc   :: Accumulator k (RecursiveI i) c f
    , _proof :: IVCProof k c f
    } deriving (GHC.Generics.Generic, Show, NFData)

makeLenses ''IVCResult

type IVCAssumptions d k a i p c f =
    ( KnownNat (d + 1)
    , KnownNat (d - 1)
    , k ~ 1
    , Arithmetic a
    , Binary a
    , LayoutFunctor i
    , LayoutFunctor p
    , LayoutFunctor c
    , Field f
    , Scale a f
    , Scale a (SimplePoly f (d + 1))
    , FieldAssumptions c f
    , FieldAssumptions c (FieldElement (CircuitContext a))
    )

-- | Create the first IVC result
--
-- It differs from the rest of the iterations as we don't have anything accumulated just yet.
ivcSetup ::
    forall d k a i p c . IVCAssumptions d k a i p c a =>
    Hasher (FieldElement (CircuitContext a)) ->
    StepFunction a i p -> i a -> p a -> IVCResult k i c a
ivcSetup hash f z0 witness =
    let
        p :: Predicate a i p
        p = predicate f

        z1 :: i a
        z1 = predicateEval p z0 witness

        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = recursivePredicate $ recursiveFunction hash f
    in
        IVCResult z1 (emptyAccumulator @d pRec) noIVCProof

ivcProve ::
    forall d k a i p c . IVCAssumptions d k a i p c a =>
    Hasher (FieldElement (CircuitContext a)) ->
    StepFunction a i p -> IVCResult k i c a -> p a -> IVCResult k i c a
ivcProve hash f res witness =
    let
        p :: Predicate a i p
        p = predicate f

        z' :: i a
        z' = predicateEval p (res^.z) witness

        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = recursivePredicate $ recursiveFunction hash f

        hash' :: Hasher a
        hash' = execFE . hash . map fromConstant
            where execFE (FieldElement ctx) = exec1 (ArithmeticCircuit ctx)

        input :: RecursiveI i a
        input = RecursiveI (res^.z) (oracle hash' $ res^.acc^.x)

        messages :: Vector k [a]
        messages = res^.proof^.proofW

        commits :: Vector k (c a)
        commits = res^.proof^.proofX

        narkIP :: NARKInstanceProof k (RecursiveI i) c a
        narkIP = NARKInstanceProof input (NARKProof commits messages)

        accScheme :: AccumulatorScheme d k (RecursiveI i) c a
        accScheme = accumulatorScheme @d hash' pRec

        (acc', pf) = Acc.prover accScheme (res^.acc) narkIP

        payload :: RecursiveP d k i p c a
        payload = RecursiveP witness commits (res^.acc^.x) one pf

        protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) c [a] [a] a
        protocol = fiatShamir hash' $ commitOpen $ specialSoundProtocol @d pRec

        (messages', commits') = unzip $ prover protocol input payload zero 0

        ivcProof :: IVCProof k c a
        ivcProof = IVCProof commits' messages'
    in
        IVCResult z' acc' ivcProof

ivcVerify ::
    forall d k a i p c f . IVCAssumptions d k a i p c f =>
    Hasher (FieldElement (CircuitContext a)) ->
    Hasher f ->
    StepFunction a i p -> IVCResult k i c f ->
    ((Vector k (c f), [f]), (Vector k (c f), c f))
ivcVerify hashFE hashF f res =
    let
        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = recursivePredicate $ recursiveFunction hashFE f

        input :: RecursiveI i f
        input = RecursiveI (res^.z) (oracle hashF $ res^.acc^.x)

        messages :: Vector k [f]
        messages = res^.proof^.proofW

        commits :: Vector k (c f)
        commits = res^.proof^.proofX

        accScheme :: AccumulatorScheme d k (RecursiveI i) c f
        accScheme = accumulatorScheme @d hashF pRec

        protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) c [f] [f] f
        protocol = fiatShamir hashF $ commitOpen $ specialSoundProtocol' @d pRec
    in
        ( verifier protocol input (singleton $ zip messages commits) zero
        , Acc.decider accScheme (res^.acc)
        )
