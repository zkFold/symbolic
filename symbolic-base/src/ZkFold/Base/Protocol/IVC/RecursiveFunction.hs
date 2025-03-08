{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module ZkFold.Base.Protocol.IVC.RecursiveFunction where

import           Control.DeepSeq                            (NFData, NFData1)
import           Data.Binary                                (Binary)
import           Data.Distributive                          (Distributive (..))
import           Data.Functor.Rep                           (Representable (..), collectRep, distributeRep)
import           Data.These                                 (These (..))
import           Data.Zip                                   (Semialign (..), Zip (..))
import           GHC.Generics                               (Generic, Generic1, U1 (..), (:*:) (..))
import           Prelude                                    (Foldable, Functor, Show, Traversable, fmap, type (~), ($),
                                                             (.), (<$>))

import           ZkFold.Base.Algebra.Basic.Class            (Scale, zero)
import           ZkFold.Base.Algebra.Basic.Number           (KnownNat, type (+), type (-))
import           ZkFold.Base.Algebra.Polynomials.Univariate (PolyVec)
import           ZkFold.Base.Data.ByteString                (Binary1)
import           ZkFold.Base.Data.Orphans                   ()
import           ZkFold.Base.Data.Package                   (packed, unpacked)
import           ZkFold.Base.Data.Vector                    (Vector)
import           ZkFold.Base.Protocol.IVC.Accumulator       hiding (pi, x)
import           ZkFold.Base.Protocol.IVC.AccumulatorScheme (AccumulatorScheme (..), accumulatorScheme)
import           ZkFold.Base.Protocol.IVC.Commit            (HomomorphicCommit)
import           ZkFold.Base.Protocol.IVC.Oracle
import           ZkFold.Base.Protocol.IVC.Predicate         (Predicate (..), PredicateAssumptions, PredicateCircuit,
                                                             predicate)
import           ZkFold.Base.Protocol.IVC.StepFunction      (StepFunction, StepFunctionAssumptions)
import           ZkFold.Symbolic.Compiler                   (ArithmeticCircuit, compileWith, guessOutput, hlmap)
import           ZkFold.Symbolic.Data.Bool                  (Bool (..))
import           ZkFold.Symbolic.Data.Class                 (LayoutFunctor, SymbolicData (..))
import           ZkFold.Symbolic.Data.Conditional           (bool)
import           ZkFold.Symbolic.Data.FieldElement          (FieldElement (FieldElement), fromFieldElement)
import           ZkFold.Symbolic.Data.Input                 (SymbolicInput)
import           ZkFold.Symbolic.Interpreter                (Interpreter (..))

-- | Public input to the recursive function
data RecursiveI i f = RecursiveI (i f) f
    deriving (Generic, Generic1, Show, Binary, NFData, NFData1, Functor, Foldable, Traversable)

instance Semialign i => Semialign (RecursiveI i) where
    alignWith f (RecursiveI x h) (RecursiveI y h') = RecursiveI (alignWith f x y) (f (These h h'))

instance Zip i => Zip (RecursiveI i) where
    zipWith f (RecursiveI x h) (RecursiveI y h') = RecursiveI (zipWith f x y) (f h h')

instance Representable i => Distributive (RecursiveI i) where
    distribute = distributeRep
    collect = collectRep

instance Representable i => Representable (RecursiveI i)

instance (HashAlgorithm algo f, RandomOracle algo f f, RandomOracle algo (i f) f) => RandomOracle algo (RecursiveI i f) f

instance (SymbolicData f, SymbolicData (i f), Context f ~ Context (i f), Support f ~ Support (i f)) => SymbolicData (RecursiveI i f)

instance (SymbolicInput f, SymbolicInput (i f), Context f ~ Context (i f)) => SymbolicInput (RecursiveI i f)

-- | Payload to the recursive function
data RecursiveP d k i p c f = RecursiveP (p f) (Vector k (c f)) (AccumulatorInstance k (RecursiveI i) c f) f (Vector (d-1) (c f))
    deriving (Generic, Generic1, NFData, NFData1, Functor, Foldable, Traversable)

instance (KnownNat (d - 1), KnownNat k, KnownNat (k - 1), Binary1 i, Binary1 p, Binary1 c, Binary f) => Binary (RecursiveP d k i p c f)

instance (KnownNat (d-1), KnownNat (k-1), KnownNat k, Representable i, Representable p, Representable c) => Distributive (RecursiveP d k i p c) where
    distribute = distributeRep
    collect = collectRep

instance (KnownNat (d-1), KnownNat (k-1), KnownNat k, Representable i, Representable p, Representable c) => Representable (RecursiveP d k i p c)

type RecursiveFunctionAssumptions algo d a i c f ctx =
    ( StepFunctionAssumptions a f ctx
    , HashAlgorithm algo f
    , RandomOracle algo f f
    , RandomOracle algo (i f) f
    , RandomOracle algo (c f) f
    , HomomorphicCommit [f] (c f)
    , Scale a f
    , Scale a (PolyVec f (d+1))
    , Scale f (c f)
    )

type RecursiveFunction algo d k a i p c = forall f ctx . RecursiveFunctionAssumptions algo d a i c f ctx
    => RecursiveI i f -> RecursiveP d k i p c f -> RecursiveI i f

-- | Transform a step function into a recursive function
recursiveFunction :: forall algo d k a i p c .
    ( PredicateAssumptions a i p
    , LayoutFunctor c
    , KnownNat (d-1)
    , KnownNat (d+1)
    , KnownNat (k-1)
    , KnownNat k
    , Zip i
    ) => StepFunction a i p -> RecursiveFunction algo d k a i p c
recursiveFunction func =
    let
        -- A helper function to derive the accumulator scheme
        func' :: forall ctx f . StepFunctionAssumptions a f ctx => RecursiveI i f -> RecursiveP d k i p c f -> RecursiveI i f
        func' (RecursiveI x h) (RecursiveP u _ _ _ _) = RecursiveI (func x u) h

        -- A helper predicate to derive the accumulator scheme
        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = predicate func'

        funcRecursive :: forall ctx f . RecursiveFunctionAssumptions algo d a i c f ctx
            => RecursiveI i f
            -> RecursiveP d k i p c f
            -> RecursiveI i f
        funcRecursive z@(RecursiveI x _) (RecursiveP u piX accX flag pf) =
            let
                accScheme :: AccumulatorScheme d k (RecursiveI i) c f
                accScheme = accumulatorScheme @algo pRec

                x' :: i f
                x' = func x u

                accX' :: AccumulatorInstance k (RecursiveI i) c f
                accX' = verifier accScheme z piX accX pf

                h :: f
                h = bool zero (oracle @algo accX') $ Bool $ fromFieldElement flag
            in
                RecursiveI x' h

    in funcRecursive

--------------------------------------------------------------------------------

type RecursivePredicateAssumptions algo d k a i p c =
    ( KnownNat (d-1)
    , KnownNat (k-1)
    , KnownNat k
    , PredicateAssumptions a i p
    , LayoutFunctor c
    )

recursivePredicate :: forall algo d k a i p c ctx0 ctx1 .
    ( RecursivePredicateAssumptions algo d k a i p c
    , ctx0 ~ Interpreter a
    , RecursiveFunctionAssumptions algo d a i c (FieldElement ctx0) ctx0
    , ctx1 ~ ArithmeticCircuit a (RecursiveI i :*: RecursiveP d k i p c) U1
    , RecursiveFunctionAssumptions algo d a i c (FieldElement ctx1) ctx1
    ) => RecursiveFunction algo d k a i p c -> Predicate a (RecursiveI i) (RecursiveP d k i p c)
recursivePredicate func =
    let
        func' :: forall f ctx . RecursiveFunctionAssumptions algo d a i c f ctx
            => ctx (RecursiveI i) -> ctx (RecursiveP d k i p c) -> ctx (RecursiveI i)
        func' x' u' =
            let
                x = FieldElement <$> unpacked x'
                u = FieldElement <$> unpacked u'
            in
                packed . fmap fromFieldElement $ func x u

        predicateEval :: (RecursiveI i) a -> (RecursiveP d k i p c) a -> (RecursiveI i) a
        predicateEval z u = runInterpreter $ func' (Interpreter z) (Interpreter u)

        predicateCircuit :: PredicateCircuit a (RecursiveI i) (RecursiveP d k i p c)
        predicateCircuit =
            hlmap (U1 :*:) $
            compileWith @a guessOutput (\(i :*: p) U1 -> (U1 :*: U1 :*: U1, i :*: p :*: U1)) func'
    in Predicate {..}
