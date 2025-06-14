{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

{- HLINT ignore "Redundant ^." -}

module ZkFold.Protocol.IVC.Internal where

import           Control.Lens                                       ((^.))
import           Control.Lens.Combinators                           (makeLenses)
import           Data.Bifunctor                                     (bimap, first)
import           Data.Function                                      (id, ($), (.))
import           Data.Functor                                       (Functor, fmap, (<$>))
import           Data.Type.Equality                                 (type (~))
import           Data.Zip                                           (Zip (..), unzip)
import           GHC.Generics                                       (Generic, Par1 (..), type (:*:) (..))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number                              (KnownNat, type (+), type (-))
import           ZkFold.Data.Vector                                 (Vector, singleton)
import           ZkFold.Protocol.IVC.Accumulator                    hiding (pi)
import qualified ZkFold.Protocol.IVC.AccumulatorScheme              as Acc
import           ZkFold.Protocol.IVC.AccumulatorScheme              (AccumulatorScheme, accumulatorScheme)
import           ZkFold.Protocol.IVC.Commit                         (HomomorphicCommit)
import           ZkFold.Protocol.IVC.CommitOpen
import           ZkFold.Protocol.IVC.FiatShamir
import           ZkFold.Protocol.IVC.NARK                           (NARKInstanceProof (..), NARKProof (..))
import           ZkFold.Protocol.IVC.Oracle
import           ZkFold.Protocol.IVC.Predicate                      (Predicate (..), StepFunction, predicate)
import           ZkFold.Protocol.IVC.RecursiveFunction
import           ZkFold.Protocol.IVC.SpecialSound                   (SpecialSoundProtocol (..), specialSoundProtocol,
                                                                     specialSoundProtocol')
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext)
import           ZkFold.Symbolic.Data.Bool                          (true)
import           ZkFold.Symbolic.Data.Class                         (Context, Layout, LayoutFunctor, SymbolicOutput)
import           ZkFold.Symbolic.Data.FieldElement                  (FieldElement (..))
import           ZkFold.Symbolic.Interpreter                        (Interpreter (..))

-- | The recursion circuit satisfiability proof.
data IVCProof k c f
    = IVCProof
    { _proofX :: Vector k (DataSource c)
    -- ^ The commitment to the witness of the recursion circuit satisfiability proof.
    , _proofW :: Vector k [f]
    -- ^ The witness of the recursion circuit satisfiability proof.
    } deriving (Generic, Functor)

makeLenses ''IVCProof

noIVCProof :: (KnownNat k, AdditiveMonoid c, AdditiveMonoid f) => IVCProof k c f
noIVCProof = IVCProof zero zero

-- | The current result of recursion together with the first iteration flag,
-- the corresponding accumulator, and the recursion circuit satisfiability proof.
data IVCResult k i c f
    = IVCResult
    { _z     :: i f
    , _acc   :: Accumulator k (RecursiveI i) (DataSource c) f
    , _proof :: IVCProof k c f
    } deriving (Generic, Functor)

makeLenses ''IVCResult

-- | Create the first IVC result
--
-- It differs from the rest of the iterations as we don't have anything accumulated just yet.
ivcSetup ::
    forall d cc k a i p c .
    ( KnownNat (d + 1)
    , KnownNat (d - 1)
    , k ~ 1
    , LayoutFunctor i
    , LayoutFunctor p
    , FieldAssumptions a cc
    , HomomorphicCommit [a] c
    ) => Hasher -> StepFunction a i p -> i a -> p a -> IVCResult k i c a
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

ivcProve ::
    forall d cc k a i p c f fe.
    ( KnownNat (d + 1)
    , KnownNat (d - 1)
    , k ~ 1
    , LayoutFunctor i
    , LayoutFunctor p
    , FieldAssumptions a cc
    , Layout cc ~ f
    , SymbolicOutput c
    , Context c ~ Interpreter a
    , Layout c ~ f
    , fe ~ FieldElement (Interpreter a)
    , Scale fe c
    , HomomorphicCommit [fe] c
    ) =>
    Hasher -> StepFunction a i p -> IVCResult k i c a -> p a ->
    IVCResult k i c a
ivcProve hash f res witness =
    let
        p :: Predicate a i p
        p = predicate f

        z' :: i a
        z' = predicateEval p (res^.z) witness

        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p cc)
        pRec = recursivePredicate @cc $ recursiveFunction @cc hash f

        value ::
            (SymbolicOutput x, Context x ~ Interpreter a) => x -> Layout x a
        value = runInterpreter . arithmetize0

        input :: RecursiveI i fe
        input = fmap fromConstant (res^.z) :*:
            Par1 (oracle hash $ bimap DataSource (fromConstant :: a -> fe) $ res^.acc^.x)

        messages :: Vector k [fe]
        messages = fmap fromConstant <$> res^.proof^.proofW

        commits :: Vector k (DataSource c)
        commits = res^.proof^.proofX

        narkIP :: NARKInstanceProof k (RecursiveI i) (DataSource c) fe
        narkIP = NARKInstanceProof input (NARKProof commits messages)

        accScheme :: AccumulatorScheme d k (RecursiveI i) (DataSource c) fe
        accScheme = accumulatorScheme hash pRec

        (acc', pf) = Acc.prover accScheme (fromConstant <$> res^.acc) narkIP

        payload :: RecursiveP d k i p cc fe
        payload = fmap fromConstant $ value $ RecursivePayload
            (Interpreter witness)
            (DataSource <$> commits)
            (bimap DataSource fromConstant $ res^.acc^.x)
            true
            (DataSource <$> pf)

        protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) (DataSource c) [fe] [a] fe
        protocol = fiatShamir hash $ commitOpen fromConstant toConstant $
            specialSoundProtocol @d fromConstant toConstant pRec

        (messages', commits') = unzip $ prover protocol input payload zero 0

        ivcProof :: IVCProof k c a
        ivcProof = IVCProof commits' (fmap toConstant <$> messages')
    in
        IVCResult z' (toConstant <$> acc') ivcProof

ivcVerify ::
    forall d k a i p c f.
    ( KnownNat (d + 1)
    , KnownNat (d - 1)
    , k ~ 1
    , LayoutFunctor i
    , LayoutFunctor p
    , FieldAssumptions a c
    , f ~ FieldElement (CircuitContext a)
    ) =>
    Hasher -> StepFunction a i p -> IVCResult k i c f ->
    ((Vector k c, [f]), (Vector k c, c))
ivcVerify hash f res =
    let
        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = recursivePredicate @c $ recursiveFunction @c hash f

        input :: RecursiveI i f
        input = (res^.z) :*: Par1 (oracle hash $ res^.acc^.x)

        messages :: Vector k [f]
        messages = res^.proof^.proofW

        commits :: Vector k (DataSource c)
        commits = res^.proof^.proofX

        accScheme :: AccumulatorScheme d k (RecursiveI i) (DataSource c) f
        accScheme = accumulatorScheme @d hash pRec

        protocol :: FiatShamir k (RecursiveI i) (RecursiveP d k i p c) (DataSource c) [f] [f] f
        protocol = fiatShamir hash $ commitOpen id id $ specialSoundProtocol' @d pRec
    in
        ( first (fmap dataSource) $ verifier protocol input (singleton $ zip messages commits) zero
        , bimap (fmap dataSource) dataSource $ Acc.decider accScheme (res^.acc)
        )
