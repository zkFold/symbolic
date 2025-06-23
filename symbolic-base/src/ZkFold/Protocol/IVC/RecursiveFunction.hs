{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.IVC.RecursiveFunction where

import Data.Binary (Binary (..))
import Data.Foldable (toList)
import Data.Function (const, (.))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Par1, (:*:))
import Prelude (($), (<$>), type (~))

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (KnownNat, type (+), type (-))
import ZkFold.Data.Empty (Empty, empty)
import ZkFold.Data.Orphans ()
import ZkFold.Data.Package (unpacked)
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.IVC.Accumulator hiding (pi, x)
import ZkFold.Protocol.IVC.AccumulatorScheme (AccumulatorScheme (..), accumulatorScheme)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit)
import ZkFold.Protocol.IVC.Oracle
import ZkFold.Protocol.IVC.Predicate (Predicate (..), StepFunction, predicate)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext)
import ZkFold.Symbolic.Data.Bool (Bool (..))
import ZkFold.Symbolic.Data.Class (LayoutFunctor, SymbolicData (..), SymbolicOutput)
import ZkFold.Symbolic.Data.Conditional (bool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..), fieldElements)

-- | Public input to the recursive function
type RecursiveI i = i :*: Par1

newtype DataSource c = DataSource {dataSource :: c}
  deriving newtype
    ( AdditiveGroup
    , AdditiveMonoid
    , AdditiveSemigroup
    , HomomorphicCommit h
    , Scale a
    , SymbolicData
    )

arithmetize0 :: SymbolicOutput x => x -> Context x (Layout x)
arithmetize0 x = arithmetize x Proxy

instance
  (Context c ~ ctx, SymbolicOutput c)
  => OracleSource (FieldElement ctx) (DataSource c)
  where
  source = toList . fieldElements . arithmetize0

-- | Payload to the recursive function
data RecursivePayload d k i p c = RecursivePayload
  { recPayload :: Context c p
  , recProof :: Vector k (DataSource c)
  , recAccInst
      :: AccumulatorInstance
           k
           (RecursiveI i)
           (DataSource c)
           (FieldElement (Context c))
  , recFlag :: Bool (Context c)
  , recCommits :: Vector (d - 1) (DataSource c)
  }
  deriving Generic

instance
  ( KnownNat (d - 1)
  , KnownNat (k - 1)
  , KnownNat k
  , LayoutFunctor i
  , LayoutFunctor p
  , SymbolicOutput c
  )
  => SymbolicData (RecursivePayload d k i p c)

type RecursiveP d k i p c = Layout (RecursivePayload d k i p c)

type RecursiveFunction d k a i p c =
  StepFunction a (RecursiveI i) (RecursiveP d k i p c)

type FieldAssumptions a c =
  ( Arithmetic a
  , Binary a
  , SymbolicOutput c
  , Context c ~ CircuitContext a
  , Empty (Payload c)
  , Scale (FieldElement (Context c)) c
  , HomomorphicCommit [FieldElement (Context c)] c
  )

instance OracleSource (FieldElement ctx) (FieldElement ctx) where
  source = (: [])

-- | Transform a step function into a recursive function
recursiveFunction
  :: forall c d k a i p
   . ( FieldAssumptions a c
     , KnownNat (d + 1)
     , KnownNat (d - 1)
     , KnownNat (k - 1)
     , KnownNat k
     , LayoutFunctor i
     , LayoutFunctor p
     )
  => Hasher -> StepFunction a i p -> RecursiveFunction d k a i p c
recursiveFunction hash func =
  let
    restore0
      :: (Empty (Payload x), SymbolicData x) => Context x (Layout x) -> x
    restore0 l = restore $ const (l, empty)

    -- A helper function to derive the accumulator scheme
    func' :: RecursiveFunction d k a i p c
    func'
      (restore0 -> (x, h :: FieldElement (Context c)))
      ( restore0 ->
          ( RecursivePayload u _ _ _ _
              :: RecursivePayload d k i p c
            )
        ) =
        arithmetize0 (func x u, h)

    -- A helper predicate to derive the accumulator scheme
    pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
    pRec = predicate func'
   in
    \i
     ( restore0 ->
         ( RecursivePayload u piX accX flag pf
             :: RecursivePayload d k i p c
           )
       ) ->
        let z = FieldElement <$> unpacked i
            (x, _ :: FieldElement (Context c)) = restore0 i
            accX' = verifier (accumulatorScheme hash pRec) z piX accX pf
            h = bool zero (oracle hash accX') flag
         in arithmetize0 (func x u, h :: FieldElement (CircuitContext a))

--------------------------------------------------------------------------------

recursivePredicate
  :: forall c d k a i p
   . ( Arithmetic a
     , Binary a
     , KnownNat (d - 1)
     , KnownNat (k - 1)
     , KnownNat k
     , LayoutFunctor i
     , LayoutFunctor p
     , SymbolicOutput c
     )
  => RecursiveFunction d k a i p c
  -> Predicate a (RecursiveI i) (RecursiveP d k i p c)
recursivePredicate = predicate
