{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.IVC.RecursiveFunction where

import Data.Binary (Binary (..))
import Data.Foldable (toList)
import Data.Function (($), (.))
import GHC.Generics (Generic, Par1, (:*:))
import Prelude ((<$>), type (~))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
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
import ZkFold.Protocol.IVC.Predicate (Predicate (..), StepFunction, predicate)
import ZkFold.Symbolic.Class (Arithmetic, Symbolic (BaseField, witnessF))
import ZkFold.Symbolic.Data.Bool (Bool, bool)
import ZkFold.Symbolic.Data.Class (LayoutFunctor, SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..), fieldElements)

-- | Public input to the recursive function
type RecursiveI i = i :*: Par1

newtype DataSource c = DataSource {dataSource :: c}
  deriving newtype
    ( AdditiveGroup
    , AdditiveSemigroup
    , CyclicGroup
    , SymbolicData
    , Zero
    )

deriving newtype instance AdditiveMonoid c => AdditiveMonoid (DataSource c)

deriving instance {-# INCOHERENT #-} Scale a c => Scale a (DataSource c)

instance
  (SymbolicData c, Context c ~ ctx)
  => OracleSource (FieldElement ctx) (DataSource c)
  where
  source = toList . fieldElements . arithmetize

-- | Payload to the recursive function
data RecursivePayload d k i p c = RecursivePayload
  { recPayload :: Context c p
  , recProof :: Vector k c
  , recAccInst :: AccumulatorInstance k (RecursiveI i) c (FieldElement (Context c))
  , recFlag :: Bool (Context c)
  , recCommits :: Vector (d - 1) c
  }
  deriving Generic

instance
  ( SymbolicData c
  , LayoutFunctor p
  , LayoutFunctor i
  , KnownNat k
  , KnownNat (k - 1)
  , KnownNat (d - 1)
  )
  => SymbolicData (RecursivePayload d k i p c)

type RecursiveP d k i p c =
  Layout (RecursivePayload d k i p c)
    :*: Payload (RecursivePayload d k i p c)

type RecursiveFunction d k a i p c =
  StepFunction a (RecursiveI i) (RecursiveP d k i p c)

instance OracleSource (FieldElement ctx) (FieldElement ctx) where
  source = (: [])

type IsRecursivePoint c =
  ( SymbolicData c
  , Context c ~ CircuitContext (BaseField (Context c))
  , LayoutFunctor (Payload c)
  , Scale (FieldElement (Context c)) c
  , AdditiveGroup c
  , OracleSource (FieldElement (Context c)) c
  )

-- | Transform a step function into a recursive function
recursiveFunction
  :: forall c d k a i p
   . ( LayoutFunctor i
     , LayoutFunctor p
     , KnownNat (d - 1)
     , KnownNat (d + 1)
     , KnownNat (k - 1)
     , KnownNat k
     , Binary a
     , IsRecursivePoint c
     , BaseField (Context c) ~ a
     )
  => Hasher
  -> HomomorphicCommit (FieldElement (Context c)) c
  -> StepFunction a i p
  -> RecursiveFunction d k a i p c
recursiveFunction hash hcommit func =
  let
    restoreP :: Context c (RecursiveP d k i p c) -> RecursivePayload d k i p c
    restoreP lp = restore (hmap fstP lp, witnessF $ hmap sndP lp)

    -- A helper function to derive the accumulator scheme
    func' :: RecursiveFunction d k a i p c
    func'
      (restore . (,empty) -> (x, h :: FieldElement (Context c)))
      ( restoreP ->
          ( RecursivePayload u _ _ _ _
              :: RecursivePayload d k i p c
            )
        ) =
        arithmetize (func x u, h)

    -- A helper predicate to derive the accumulator scheme
    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
    pRec = predicate func'
   in
    \i
     ( restoreP ->
         ( RecursivePayload u piX accX flag pf
             :: RecursivePayload d k i p c
           )
       ) ->
        let z = FieldElement <$> unpacked i
            (x, _ :: FieldElement (Context c)) = restore (i, empty)
            accX' = verifier (accumulatorScheme hash hcommit pRec) z piX accX pf
            h = bool zero (oracle hash accX') flag
         in arithmetize (func x u, h :: FieldElement (CircuitContext a))

--------------------------------------------------------------------------------

recursivePredicate
  :: forall c d k a i p
   . ( KnownNat k
     , KnownNat (k - 1)
     , KnownNat (d - 1)
     , Arithmetic a
     , Binary a
     , LayoutFunctor i
     , LayoutFunctor p
     , SymbolicData c
     , LayoutFunctor (Payload c)
     )
  => RecursiveFunction d k a i p c
  -> Predicate a (RecursiveI i) (RecursiveP d k i p c)
recursivePredicate = predicate
