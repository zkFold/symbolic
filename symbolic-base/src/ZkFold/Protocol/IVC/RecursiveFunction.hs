{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module ZkFold.Protocol.IVC.RecursiveFunction where

import           Control.DeepSeq                                    (NFData, NFData1)
import           Data.Binary                                        (Binary)
import           Data.Distributive                                  (Distributive (..))
import           Data.Functor.Rep                                   (Representable (..), collectRep, distributeRep)
import           GHC.Generics                                       (Generic, Generic1, Par1 (..), type (:.:) (..))
import           Prelude                                            (Foldable, Functor, Show, Traversable, fmap,
                                                                     type (~), ($), (<$>))

import           ZkFold.Algebra.Class                               (Scale, zero)
import           ZkFold.Algebra.Number                              (KnownNat, type (+), type (-))
import           ZkFold.Control.HApplicative                        (HApplicative (hliftA2))
import           ZkFold.Data.ByteString                             (Binary1)
import           ZkFold.Data.HFunctor                               (hmap)
import           ZkFold.Data.Orphans                                ()
import           ZkFold.Data.Package                                (unpacked)
import           ZkFold.Data.Vector                                 (Vector)
import           ZkFold.Protocol.IVC.Accumulator                    hiding (pi, x)
import           ZkFold.Protocol.IVC.AccumulatorScheme              (AccumulatorScheme (..), accumulatorScheme)
import           ZkFold.Protocol.IVC.Commit                         (HomomorphicCommit)
import           ZkFold.Protocol.IVC.Oracle
import           ZkFold.Protocol.IVC.Predicate                      (Predicate (..), StepFunction, predicate)
import           ZkFold.Symbolic.Class                              (Arithmetic)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext)
import           ZkFold.Symbolic.Data.Bool                          (Bool (..))
import           ZkFold.Symbolic.Data.Class                         (LayoutFunctor, SymbolicData (..))
import           ZkFold.Symbolic.Data.Conditional                   (bool)
import           ZkFold.Symbolic.Data.FieldElement                  (FieldElement (FieldElement), fromFieldElement)
import           ZkFold.Symbolic.Data.Input                         (SymbolicInput)

-- | Public input to the recursive function
data RecursiveI i f = RecursiveI (i f) f
    deriving (Generic, Generic1, Show, Binary, NFData, NFData1, Functor, Foldable, Traversable)

instance Representable i => Distributive (RecursiveI i) where
    distribute = distributeRep
    collect = collectRep

instance Representable i => Representable (RecursiveI i)

instance (OracleSource a f, Foldable i) => OracleSource a (RecursiveI i f) where
    source (RecursiveI x h) = source (FoldableSource x, h)

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

type RecursiveFunction algo d k a i p c =
    StepFunction a (RecursiveI i) (RecursiveP d k i p c)

type FieldAssumptions algo c f =
    ( HashAlgorithm algo f
    , OracleSource f (c f)
    , HomomorphicCommit [f] (c f)
    , Scale f (c f)
    )

-- | Transform a step function into a recursive function
recursiveFunction :: forall algo d k a i p c .
    ( Arithmetic a
    , Binary a
    , LayoutFunctor i
    , LayoutFunctor p
    , LayoutFunctor c
    , KnownNat (d-1)
    , KnownNat (d+1)
    , KnownNat (k-1)
    , KnownNat k
    , FieldAssumptions algo c (FieldElement (CircuitContext a))
    ) => StepFunction a i p -> RecursiveFunction algo d k a i p c
recursiveFunction func =
    let
        -- A helper function to derive the accumulator scheme
        func' ::
            CircuitContext a (RecursiveI i) ->
            CircuitContext a (RecursiveP d k i p c) ->
            CircuitContext a (RecursiveI i)
        func' i p =
            let x = hmap (\(RecursiveI x' _) -> x') i
                h = hmap (\(RecursiveI _ h') -> Par1 h') i
                u = hmap (\(RecursiveP u' _ _ _ _) -> u') p
                y = func x u
             in hliftA2 (\y' (Par1 h') -> RecursiveI y' h') y h

        -- A helper predicate to derive the accumulator scheme
        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = predicate func'

     in \i p ->
        let z = FieldElement <$> unpacked i
            x = hmap (\(RecursiveI x0 _) -> x0) i
            u = hmap (\(RecursiveP u0 _ _ _ _) -> u0) p
            piX = unComp1 $ fmap FieldElement $ unpacked $
                hmap (\(RecursiveP _ pi _ _ _) -> Comp1 pi) p
            accX = fmap FieldElement $ unpacked $
                hmap (\(RecursiveP _ _ a _ _) -> a) p
            flag = Bool $ hmap (\(RecursiveP _ _ _ f _) -> Par1 f) p
            pf = unComp1 $ fmap FieldElement $ unpacked $
                hmap (\(RecursiveP _ _ _ _ p') -> Comp1 p') p
            accScheme = accumulatorScheme @algo pRec
            x' = func x u
            accX' = verifier accScheme z piX accX pf
            h = bool zero (oracle @algo accX') flag
         in hliftA2 (\x0 (Par1 h0) -> RecursiveI x0 h0) x' (fromFieldElement h)

--------------------------------------------------------------------------------

recursivePredicate :: forall algo d k a i p c .
    ( KnownNat k
    , KnownNat (k - 1)
    , KnownNat (d - 1)
    , Arithmetic a
    , Binary a
    , LayoutFunctor i
    , LayoutFunctor p
    , LayoutFunctor c
    ) => RecursiveFunction algo d k a i p c -> Predicate a (RecursiveI i) (RecursiveP d k i p c)
recursivePredicate = predicate
