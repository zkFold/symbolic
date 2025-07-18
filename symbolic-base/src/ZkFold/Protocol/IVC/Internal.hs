{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- HLINT ignore "Redundant ^." -}

module ZkFold.Protocol.IVC.Internal where

import Control.Lens ((^.))
import Control.Lens.Combinators (makeLenses)
import Data.Bifunctor (bimap, first)
import Data.Function (($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Type.Equality (type (~))
import Data.Zip (Zip (..), unzip)
import GHC.Generics (Generic, Par1 (..), type (:*:) (..))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (ScalarFieldOf))
import ZkFold.Algebra.Number (KnownNat, type (+), type (-))
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.Accumulator hiding (pi)
import ZkFold.Protocol.IVC.AccumulatorScheme (AccumulatorScheme, accumulatorScheme)
import qualified ZkFold.Protocol.IVC.AccumulatorScheme as Acc
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit)
import ZkFold.Protocol.IVC.CommitOpen (commitOpen)
import ZkFold.Protocol.IVC.FiatShamir
import ZkFold.Protocol.IVC.NARK (NARKInstanceProof (..), NARKProof (..))
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.Predicate (Predicate (..), StepFunction, predicate)
import ZkFold.Protocol.IVC.RecursiveFunction
import ZkFold.Protocol.IVC.SpecialSound (
  specialSoundProtocolA,
  specialSoundProtocolC,
 )
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.Class (Context, Layout, LayoutFunctor, SymbolicData, arithmetize)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Interpreter (Interpreter (..))

-- | The recursion circuit satisfiability proof.
data IVCProof k c f
  = IVCProof
  { _proofX :: Vector k (DataSource c)
  -- ^ The commitment to the witness of the recursion circuit satisfiability proof.
  , _proofW :: Vector k [f]
  -- ^ The witness of the recursion circuit satisfiability proof.
  }
  deriving (Functor, Generic)

makeLenses ''IVCProof

noIVCProof :: (KnownNat k, AdditiveMonoid c, AdditiveMonoid f) => IVCProof k c f
noIVCProof = IVCProof zero zero

-- | The current result of recursion together with the first iteration flag,
-- the corresponding accumulator, and the recursion circuit satisfiability proof.
data IVCResult k i c f
  = IVCResult
  { _z :: i f
  , _acc :: Accumulator k (RecursiveI i) (DataSource c) f
  , _proof :: IVCProof k c f
  }
  deriving (Functor, Generic)

makeLenses ''IVCResult

-- | Create the first IVC result
--
-- It differs from the rest of the iterations as we don't have anything accumulated just yet.
ivcSetup
  :: forall d cc k a i p c
   . ( KnownNat (d + 1)
     , KnownNat (d - 1)
     , k ~ 1
     , LayoutFunctor i
     , LayoutFunctor p
     , FieldAssumptions a cc
     , HomomorphicCommit c
     , a ~ ScalarFieldOf c
     )
  => Hasher
  -> StepFunction a i p
  -> i a
  -> p a
  -> IVCResult k i c a
ivcSetup hash f z0 witness =
  let
    p :: Predicate a i p
    p = predicate f

    z1 :: i a
    z1 = predicateEval p z0 witness

    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p cc)
    pRec = recursivePredicate @cc $ recursiveFunction @cc hash f
   in
    IVCResult z1 (emptyAccumulator @d pRec) noIVCProof

ivcProve
  :: forall d cc k a i p c f
   . ( KnownNat (d + 1)
     , KnownNat (d - 1)
     , k ~ 1
     , LayoutFunctor i
     , LayoutFunctor p
     , FieldAssumptions a cc
     , Layout cc ~ f
     , SymbolicData c
     , Context c ~ Interpreter a
     , Layout c ~ f
     , Scale a c
     , OracleSource a (DataSource c)
     , HomomorphicCommit c
     , a ~ ScalarFieldOf c
     )
  => Hasher
  -> StepFunction a i p
  -> IVCResult k i c a
  -> p a
  -> IVCResult k i c a
ivcProve hash f res witness =
  let
    p :: Predicate a i p
    p = predicate f

    z' :: i a
    z' = predicateEval p (res ^. z) witness

    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p cc)
    pRec = recursivePredicate @cc $ recursiveFunction @cc hash f

    value
      :: (SymbolicData x, Context x ~ Interpreter a) => x -> Layout x a
    value = runInterpreter . arithmetize

    input :: RecursiveI i a
    input =
      fmap fromConstant (res ^. z)
        :*: Par1 (oracle hash $ res ^. acc ^. x)

    messages :: Vector k [a]
    messages = fmap fromConstant <$> res ^. proof ^. proofW

    commits :: Vector k (DataSource c)
    commits = res ^. proof ^. proofX

    narkIP :: NARKInstanceProof k (RecursiveI i) (DataSource c) a
    narkIP = NARKInstanceProof input (NARKProof commits messages)

    accScheme :: AccumulatorScheme d k (RecursiveI i) (DataSource c) a
    accScheme = accumulatorScheme hash pRec

    (acc', pf) = Acc.prover accScheme (fromConstant <$> res ^. acc) narkIP

    payload :: RecursiveP d k i p cc a
    payload =
      fmap fromConstant $
        value $
          RecursivePayload
            (Interpreter witness)
            (DataSource <$> commits)
            (bimap DataSource fromConstant $ res ^. acc ^. x)
            true
            (DataSource <$> pf)

    protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) (DataSource c) a
    protocol =
      fiatShamir hash $
        commitOpen $
          specialSoundProtocolA @d pRec

    (messages', commits') = unzip $ prover protocol input payload zero 0

    ivcProof :: IVCProof k c a
    ivcProof = IVCProof commits' messages'
   in
    IVCResult z' acc' ivcProof

ivcVerify
  :: forall d k a i p c f
   . ( KnownNat (d + 1)
     , KnownNat (d - 1)
     , k ~ 1
     , LayoutFunctor i
     , LayoutFunctor p
     , FieldAssumptions a c
     , f ~ FieldElement (CircuitContext a)
     )
  => Hasher
  -> StepFunction a i p
  -> IVCResult k i c f
  -> ((Vector k c, [f]), (Vector k c, c))
ivcVerify hash f res =
  let
    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
    pRec = recursivePredicate @c $ recursiveFunction @c hash f

    input :: RecursiveI i f
    input = (res ^. z) :*: Par1 (oracle hash $ res ^. acc ^. x)

    messages :: Vector k [f]
    messages = res ^. proof ^. proofW

    commits :: Vector k (DataSource c)
    commits = res ^. proof ^. proofX

    accScheme :: AccumulatorScheme d k (RecursiveI i) (DataSource c) f
    accScheme = accumulatorScheme @d hash pRec

    protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) (DataSource c) f
    protocol = fiatShamir hash $ commitOpen $ specialSoundProtocolC @d pRec
   in
    ( first (fmap dataSource) $ verifier protocol input (zip messages commits) zero
    , bimap (fmap dataSource) dataSource $ Acc.decider accScheme (res ^. acc)
    )
