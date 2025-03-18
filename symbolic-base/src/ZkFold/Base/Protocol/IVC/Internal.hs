{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module ZkFold.Base.Protocol.IVC.Internal where

import           Control.DeepSeq                            (NFData)
import           Control.Lens                               ((^.))
import           Control.Lens.Combinators                   (makeLenses)
import           Data.Functor.Rep                           (Representable (..))
import           Data.Type.Equality                         (type (~))
import           Data.Zip                                   (Zip (..), unzip)
import           GHC.Generics                               (Generic, U1, (:*:))
import           Prelude                                    (Show, const, ($))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number           (KnownNat, type (+))
import           ZkFold.Base.Algebra.Polynomials.Univariate (PolyVec)
import           ZkFold.Base.Data.Vector                    (Vector, singleton)
import           ZkFold.Base.Protocol.IVC.Accumulator       hiding (pi)
import qualified ZkFold.Base.Protocol.IVC.AccumulatorScheme as Acc
import           ZkFold.Base.Protocol.IVC.AccumulatorScheme (AccumulatorScheme, accumulatorScheme)
import           ZkFold.Base.Protocol.IVC.Commit            (HomomorphicCommit)
import           ZkFold.Base.Protocol.IVC.CommitOpen
import           ZkFold.Base.Protocol.IVC.FiatShamir
import           ZkFold.Base.Protocol.IVC.NARK              (NARKInstanceProof (..), NARKProof (..))
import           ZkFold.Base.Protocol.IVC.Oracle
import           ZkFold.Base.Protocol.IVC.Predicate         (Predicate (..), predicate)
import           ZkFold.Base.Protocol.IVC.RecursiveFunction
import           ZkFold.Base.Protocol.IVC.SpecialSound      (SpecialSoundProtocol (..), specialSoundProtocol,
                                                             specialSoundProtocol')
import           ZkFold.Base.Protocol.IVC.StepFunction      (StepFunction)
import           ZkFold.Symbolic.Compiler                   (ArithmeticCircuit)
import           ZkFold.Symbolic.Data.FieldElement          (FieldElement)
import           ZkFold.Symbolic.Interpreter                (Interpreter)

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

type IVCAssumptions ctx0 ctx1 algo d k a i p c f =
    ( RecursivePredicateAssumptions algo d k a i p c
    , KnownNat (d+1)
    , k ~ 1
    , Zip i
    , Field f
    , HashAlgorithm algo f
    , RandomOracle algo f f
    , RandomOracle algo (i f) f
    , RandomOracle algo (c f) f
    , HomomorphicCommit [f] (c f)
    , Scale a f
    , Scale a (PolyVec f (d+1))
    , Scale f (c f)
    , ctx0 ~ Interpreter a
    , RecursiveFunctionAssumptions algo d a i c (FieldElement ctx0) ctx0
    , ctx1 ~ ArithmeticCircuit a (RecursiveI i :*: RecursiveP d k i p c) U1
    , RecursiveFunctionAssumptions algo d a i c (FieldElement ctx1) ctx1
    )

-- | Create the first IVC result
--
-- It differs from the rest of the iterations as we don't have anything accumulated just yet.
ivcSetup :: forall ctx0 ctx1 algo d k a i p c . IVCAssumptions ctx0 ctx1 algo d k a i p c a
    => StepFunction a i p
    -> i a
    -> p a
    -> IVCResult k i c a
ivcSetup f z0 witness =
    let
        p :: Predicate a i p
        p = predicate f

        z1 :: i a
        z1 = predicateEval p z0 witness

        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = recursivePredicate @algo $ recursiveFunction @algo f
    in
        IVCResult z1 (emptyAccumulator @d pRec) noIVCProof

ivcProve :: forall ctx0 ctx1 algo d k a i p c . IVCAssumptions ctx0 ctx1 algo d k a i p c a
    => StepFunction a i p
    -> IVCResult k i c a
    -> p a
    -> IVCResult k i c a
ivcProve f res witness =
    let
        p :: Predicate a i p
        p = predicate f

        z' :: i a
        z' = predicateEval p (res^.z) witness

        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = recursivePredicate @algo $ recursiveFunction @algo f

        input :: RecursiveI i a
        input = RecursiveI (res^.z) (oracle @algo $ res^.acc^.x)

        messages :: Vector k [a]
        messages = res^.proof^.proofW

        commits :: Vector k (c a)
        commits = res^.proof^.proofX

        narkIP :: NARKInstanceProof k (RecursiveI i) c a
        narkIP = NARKInstanceProof input (NARKProof commits messages)

        accScheme :: AccumulatorScheme d k (RecursiveI i) c a
        accScheme = accumulatorScheme @algo @d pRec

        (acc', pf) = Acc.prover accScheme (res^.acc) narkIP

        payload :: RecursiveP d k i p c a
        payload = RecursiveP witness commits (res^.acc^.x) one pf

        protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) c [a] [a] a
        protocol = fiatShamir @algo $ commitOpen $ specialSoundProtocol @d pRec

        (messages', commits') = unzip $ prover protocol input payload zero 0

        ivcProof :: IVCProof k c a
        ivcProof = IVCProof commits' messages'
    in
        IVCResult z' acc' ivcProof

ivcVerify :: forall ctx0 ctx1 algo d k a i p c f . IVCAssumptions ctx0 ctx1 algo d k a i p c f
    => StepFunction a i p
    -> IVCResult k i c f
    -> ((Vector k (c f), [f]), (Vector k (c f), c f))
ivcVerify f res =
    let
        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = recursivePredicate @algo $ recursiveFunction @algo f

        input :: RecursiveI i f
        input = RecursiveI (res^.z) (oracle @algo $ res^.acc^.x)

        messages :: Vector k [f]
        messages = res^.proof^.proofW

        commits :: Vector k (c f)
        commits = res^.proof^.proofX

        accScheme :: AccumulatorScheme d k (RecursiveI i) c f
        accScheme = accumulatorScheme @algo @d pRec

        protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) c [f] [f] f
        protocol = fiatShamir @algo $ commitOpen $ specialSoundProtocol' @d pRec
    in
        ( verifier protocol input (singleton $ zip messages commits) zero
        , Acc.decider accScheme (res^.acc)
        )
