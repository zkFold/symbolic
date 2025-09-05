{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.IVC.RecursiveFunction where

import Data.Binary (Binary (..))
import Data.Function (($), (.))
import Data.Semialign (Zip)
import GHC.Generics (Generic, Generic1, Par1, (:*:) (..))
import Prelude ((<$>), type (~))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, type (+), type (-))
import ZkFold.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Data.Empty (empty)
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Orphans ()
import ZkFold.Data.Package (unpacked)
import ZkFold.Data.Product (fstP, sndP)
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.Accumulator hiding (pi, x)
import ZkFold.Protocol.IVC.AccumulatorScheme (AccumulatorScheme (..), accumulatorScheme)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit)
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.Predicate (Compilable, Predicate (..), StepFunction, predicate)
import ZkFold.Symbolic.Class (Arithmetic, witnessF)
import ZkFold.Symbolic.Data.Bool (Bool, bool)
import ZkFold.Symbolic.Data.Class (LayoutFunctor, RepData, SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Vec (Vec (..), runVec)

-- | Public input to the recursive function
type RecursiveI i = i :*: Par1

-- | Payload to the recursive function
data RecursivePayload d k i p c ctx = RecursivePayload
  { recPayload :: Vec p ctx
  , recProof :: Vector k (c ctx)
  , recAccInst :: AccumulatorInstance' k (RecursiveI i) c FieldElement ctx
  , recFlag :: Bool ctx
  , recCommits :: Vector (d - 1) (c ctx)
  }
  deriving (Generic, Generic1)

instance
  ( SymbolicData c
  , LayoutFunctor p
  , LayoutFunctor i
  , Zip i
  , KnownNat k
  , KnownNat (k - 1)
  , KnownNat (d - 1)
  )
  => SymbolicData (RecursivePayload d k i p c)

type RecursiveP d k i p c n =
  Layout (RecursivePayload d k i p c) n
    :*: Payload (RecursivePayload d k i p c) n

type RecursiveFunction d k a i p c =
  StepFunction a (RecursiveI i) (RecursiveP d k i p c (Order a))

instance OracleSource (FieldElement ctx) (FieldElement ctx) where
  source = (: [])

type IsRecursivePoint c a =
  ( SymbolicData c
  , RepData c (CircuitContext a)
  , Compilable (Payload c (Order a))
  , Scale (FieldElement (CircuitContext a)) (c (CircuitContext a))
  , AdditiveGroup (c (CircuitContext a))
  , OracleSource (FieldElement (CircuitContext a)) (c (CircuitContext a))
  )

-- | Transform a step function into a recursive function
recursiveFunction
  :: forall c d k a i p ctx
   . ( Compilable i
     , Zip i
     , Compilable p
     , KnownNat (d - 1)
     , KnownNat (d + 1)
     , KnownNat (k - 1)
     , KnownNat k
     , Arithmetic a
     , Binary a
     , IsRecursivePoint c a
     , ctx ~ CircuitContext a
     )
  => Hasher
  -> HomomorphicCommit (FieldElement ctx) (c ctx)
  -> StepFunction a i p
  -> RecursiveFunction d k a i p c
recursiveFunction hash hcommit func =
  let
    restoreP
      :: Vec (RecursiveP d k i p c (Order a)) ctx
      -> RecursivePayload d k i p c ctx
    restoreP (runVec -> lp) = restore (hmap fstP lp, witnessF $ hmap sndP lp)

    -- A helper function to derive the accumulator scheme
    func' :: RecursiveFunction d k a i p c
    func'
      (restore . (,empty) . runVec -> (x :*: (h :: FieldElement ctx)))
      ( restoreP ->
          ( RecursivePayload u _ _ _ _
              :: RecursivePayload d k i p c ctx
            )
        ) = Vec $ arithmetize (func x u :*: h)

    -- A helper predicate to derive the accumulator scheme
    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c (Order a))
    pRec = predicate func'
   in
    \(runVec -> i)
     ( restoreP ->
         ( RecursivePayload u piX (toConstant -> accX) flag pf
             :: RecursivePayload d k i p c ctx
           )
       ) ->
        let z = FieldElement <$> unpacked i
            x :*: (_ :: FieldElement ctx) = restore (i, empty)
            accX' = verifier (accumulatorScheme hash hcommit pRec) z piX accX pf
            h = bool zero (oracle hash accX') flag
         in Vec $ arithmetize (func x u :*: (h :: FieldElement (CircuitContext a)))

--------------------------------------------------------------------------------

recursivePredicate
  :: forall c d k a i p
   . ( KnownNat k
     , KnownNat (k - 1)
     , KnownNat (d - 1)
     , Arithmetic a
     , Binary a
     , Compilable i
     , Compilable p
     , SymbolicData c
     , RepData c (CircuitContext a)
     , Compilable (Payload c (Order a))
     )
  => RecursiveFunction d k a i p c
  -> Predicate a (RecursiveI i) (RecursiveP d k i p c (Order a))
recursivePredicate = predicate
