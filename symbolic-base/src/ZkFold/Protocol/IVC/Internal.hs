{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- HLINT ignore "Redundant ^." -}

module ZkFold.Protocol.IVC.Internal where

import Control.Lens ((^.))
import Control.Lens.Combinators (makeLenses)
import Data.Bifunctor (bimap)
import Data.Binary (Binary)
import Data.Function (($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Type.Equality (type (~))
import Data.Zip (Zip (..), unzip)
import GHC.Generics (Generic, Par1 (..), type (:*:) (..))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, type (+), type (-))
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.Accumulator hiding (pi)
import ZkFold.Protocol.IVC.AccumulatorScheme (AccumulatorScheme, accumulatorScheme, (.+))
import qualified ZkFold.Protocol.IVC.AccumulatorScheme as Acc
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit)
import ZkFold.Protocol.IVC.CommitOpen (commitOpen)
import ZkFold.Protocol.IVC.FiatShamir
import ZkFold.Protocol.IVC.NARK (NARKInstanceProof (..), NARKProof (..))
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.Predicate (Predicate (..), StepFunction, predicate, Compilable)
import ZkFold.Protocol.IVC.RecursiveFunction
import ZkFold.Protocol.IVC.SpecialSound (
  specialSoundProtocolA,
  specialSoundProtocolC,
 )
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.Class (Layout, Payload, SymbolicData, arithmetize, payload)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import ZkFold.Symbolic.Data.Vec (Vec(..))
import ZkFold.ArithmeticCircuit.Context (CircuitContext)

-- | The recursion circuit satisfiability proof.
data IVCProof k c f
  = IVCProof
  { _proofX :: Vector k c
  -- ^ The commitment to the witness of the recursion circuit satisfiability proof.
  , _proofW :: Vector k [f]
  -- ^ The witness of the recursion circuit satisfiability proof.
  }
  deriving (Functor, Generic)

makeLenses ''IVCProof

noIVCProof :: (KnownNat k, Zero c, Zero f) => IVCProof k c f
noIVCProof = IVCProof zero zero

-- | The current result of recursion together with the first iteration flag,
-- the corresponding accumulator, and the recursion circuit satisfiability proof.
data IVCResult k i c f
  = IVCResult
  { _z :: i f
  , _acc :: Accumulator k (RecursiveI i) c f
  , _proof :: IVCProof k c f
  }
  deriving (Functor, Generic)

makeLenses ''IVCResult

-- | Create the first IVC result
--
-- It differs from the rest of the iterations as we don't have anything accumulated just yet.
ivcSetup
  :: forall d pt k a i p c
   . ( KnownNat (d + 1)
     , KnownNat (d - 1)
     , k ~ 1
     , Compilable i, Zip i
     , Compilable p
     , Zero c
     , Arithmetic a
     , Binary a
     , IsRecursivePoint pt a
     )
  => Hasher
  -> HomomorphicCommit a c
  -> HomomorphicCommit (FieldElement (CircuitContext a)) (pt (CircuitContext a))
  -> StepFunction a i p
  -> i a
  -> p a
  -> IVCResult k i c a
ivcSetup hash hcommit1 hcommit2 f z0 witness =
  let
    p :: Predicate a i p
    p = predicate f

    z1 :: i a
    z1 = predicateEval p z0 witness

    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p pt (Order a))
    pRec = recursivePredicate @pt $ recursiveFunction @pt hash hcommit2 f
   in
    IVCResult z1 (emptyAccumulator @d hcommit1 pRec) noIVCProof

ivcProve
  :: forall d pt k a i p c
   . ( KnownNat (d + 1)
     , KnownNat (d - 1)
     , k ~ 1
     , Zip i, Compilable i
     , Compilable p
     , Arithmetic a
     , Binary a
     , IsRecursivePoint pt a
     , Scale a c
     , OracleSource a c
     , AdditiveGroup c
     )
  => Hasher
  -> HomomorphicCommit a c
  -> HomomorphicCommit (FieldElement (CircuitContext a)) (pt (CircuitContext a))
  -> (c -> pt (Interpreter a))
  -> StepFunction a i p
  -> IVCResult k i c a
  -> p a
  -> IVCResult k i c a
ivcProve hash hcommit1 hcommit2 ptData f res witness =
  let
    p :: Predicate a i p
    p = predicate f

    z' :: i a
    z' = predicateEval p (res ^. z) witness

    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p pt (Order a))
    pRec = recursivePredicate @pt $ recursiveFunction @pt hash hcommit2 f

    input :: RecursiveI i a
    input =
      fmap fromConstant (res ^. z)
        :*: Par1 (oracle hash $ res ^. acc ^. x)

    messages :: Vector k [a]
    messages = fmap fromConstant <$> res ^. proof ^. proofW

    commits :: Vector k c
    commits = res ^. proof ^. proofX

    narkIP :: NARKInstanceProof k (RecursiveI i) c a
    narkIP = NARKInstanceProof input (NARKProof commits messages)

    accScheme :: AccumulatorScheme d k (RecursiveI i) c c a
    accScheme = accumulatorScheme hash hcommit1 pRec

    (acc', pf) = Acc.prover accScheme (res ^. acc) narkIP

    value
      :: SymbolicData x
      => x (Interpreter a) -> (Layout x (Order a) :*: Payload x (Order a)) a
    value v = runInterpreter (arithmetize v) :*: payload v

    recursivePayload :: RecursiveP d k i p pt (Order a) a
    recursivePayload =
      value
        RecursivePayload
          { recPayload = Vec $ Interpreter witness
          , recProof = ptData <$> commits
          , recAccInst = fromConstant $ bimap ptData
              (fromConstant @_ @(FieldElement (Interpreter a)))
              (res ^. acc ^. x)
          , recFlag = true
          , recCommits = ptData . (zero .+) <$> pf
          }

    protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p pt (Order a)) c a
    protocol =
      fiatShamir hash $
        commitOpen hcommit1 $
          specialSoundProtocolA @d pRec

    (messages', commits') =
      unzip $
        prover protocol input recursivePayload zero 0

    ivcProof :: IVCProof k c a
    ivcProof = IVCProof commits' messages'
   in
    IVCResult z' acc' ivcProof

ivcVerify
  :: forall d k a i p c pt f
   . ( KnownNat (d + 1)
     , KnownNat (d - 1)
     , k ~ 1
     , Zip i, Compilable i
     , Compilable p
     , IsRecursivePoint pt a
     , Arithmetic a
     , Binary a
     , AdditiveGroup c
     , OracleSource f c
     , Scale f c
     , f ~ FieldElement (CircuitContext a)
     )
  => Hasher
  -> HomomorphicCommit f c
  -> HomomorphicCommit f (pt (CircuitContext a))
  -> StepFunction a i p
  -> IVCResult k i c f
  -> ((Vector k c, [f]), (Vector k c, c))
ivcVerify hash hcommitC hcommitPt f res =
  let
    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p pt (Order a))
    pRec = recursivePredicate @pt $ recursiveFunction @pt hash hcommitPt f

    input :: RecursiveI i f
    input = (res ^. z) :*: Par1 (oracle hash $ res ^. acc ^. x)

    messages :: Vector k [f]
    messages = res ^. proof ^. proofW

    commits :: Vector k c
    commits = res ^. proof ^. proofX

    accScheme :: AccumulatorScheme d k (RecursiveI i) c c f
    accScheme = accumulatorScheme @d hash hcommitC pRec

    protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p pt (Order a)) c f
    protocol = fiatShamir hash $ commitOpen hcommitC $ specialSoundProtocolC @d pRec
   in
    ( verifier protocol input (zip messages commits) zero
    , Acc.decider accScheme (res ^. acc)
    )
