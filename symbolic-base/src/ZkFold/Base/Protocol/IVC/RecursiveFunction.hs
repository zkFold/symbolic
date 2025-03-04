{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module ZkFold.Base.Protocol.IVC.RecursiveFunction where

import           Control.DeepSeq                            (NFData, NFData1)
import           Data.Distributive                          (Distributive (..))
import           Data.Function                              ((.))
import           Data.Functor                               ((<$>))
import           Data.Functor.Rep                           (Representable (..), collectRep, distributeRep)
import           Data.These                                 (These (..))
import           Data.Zip                                   (Semialign (..), Zip (..))
import           GHC.Generics                               (Generic, Generic1, Par1 (..), type (:.:) (..))
import           Prelude                                    (Foldable, Functor, Show, Traversable, fmap, type (~), ($))

import           ZkFold.Base.Algebra.Basic.Class            (FromConstant (..), zero)
import           ZkFold.Base.Algebra.Basic.Number           (KnownNat, type (+), type (-))
import           ZkFold.Base.Control.HApplicative           (HApplicative (..))
import           ZkFold.Base.Data.ByteString                (Binary, Binary1)
import           ZkFold.Base.Data.HFunctor                  (hmap)
import           ZkFold.Base.Data.Orphans                   ()
import           ZkFold.Base.Data.Package                   (Package (..), packed, unpacked)
import           ZkFold.Base.Data.Vector                    (Vector)
import           ZkFold.Base.Protocol.IVC.Accumulator       hiding (pi, x)
import           ZkFold.Base.Protocol.IVC.AccumulatorScheme (AccumulatorScheme (..), accumulatorScheme)
import           ZkFold.Base.Protocol.IVC.Oracle
import           ZkFold.Base.Protocol.IVC.Predicate         (Predicate (..), PredicateFunction, predicate)
import           ZkFold.Symbolic.Class                      (Arithmetic, Symbolic (..))
import           ZkFold.Symbolic.Data.Bool                  (Bool (..))
import           ZkFold.Symbolic.Data.Class                 (LayoutFunctor, SymbolicData (..))
import           ZkFold.Symbolic.Data.Conditional           (Conditional (..), ifThenElse)
import           ZkFold.Symbolic.Data.FieldElement          (FieldElement (..))
import           ZkFold.Symbolic.Data.Input                 (SymbolicInput)

-- | Public input to the recursive function
data RecursiveI i f = RecursiveI
    { recursiveIInput :: i f
    , recursiveIHash  :: f
    }
    deriving (Generic, Generic1, Show, Binary, NFData, NFData1, Functor, Foldable, Traversable)

instance Semialign i => Semialign (RecursiveI i) where
    alignWith f (RecursiveI x h) (RecursiveI y h') = RecursiveI (alignWith f x y) (f (These h h'))

instance Zip i => Zip (RecursiveI i) where
    zipWith f (RecursiveI x h) (RecursiveI y h') = RecursiveI (zipWith f x y) (f h h')

instance Representable i => Distributive (RecursiveI i) where
    distribute = distributeRep
    collect = collectRep

instance Representable i => Representable (RecursiveI i)

instance (SymbolicData f, SymbolicData (i f), Context f ~ Context (i f), Support f ~ Support (i f)) => SymbolicData (RecursiveI i f)

instance (SymbolicInput f, SymbolicInput (i f), Context f ~ Context (i f)) => SymbolicInput (RecursiveI i f)

instance {-# INCOHERENT #-} (Functor i, FromConstant a (FieldElement ctx)) => FromConstant (RecursiveI i a) (RecursiveI i (FieldElement ctx)) where
    fromConstant (RecursiveI x h) = RecursiveI (fmap fromConstant x) (fromConstant h)

instance (Symbolic ctx, LayoutFunctor i) => Conditional (Bool ctx) (RecursiveI i (FieldElement ctx)) where
    bool x y b =
        let
            x' = packed $ fmap fromFieldElement x
            y' = packed $ fmap fromFieldElement y
        in fmap FieldElement $ unpacked $ bool x' y' b

-- | Payload to the recursive function
data RecursiveP d k i p c f = RecursiveP
  { recursivePPayload     :: p f
  , recursivePCommitments :: Vector k (c f)
  , recursivePAccInstance :: AccumulatorInstance k (RecursiveI i) c f
  , recursivePFlag        :: f
  , recursivePAccProof    :: Vector (d-1) (c f)
  }
  deriving (Generic, Generic1, NFData1, Functor, Foldable, Traversable)

instance (KnownNat (d - 1), KnownNat k, KnownNat (k - 1), Binary1 i, Binary1 p, Binary1 c, Binary f) => Binary (RecursiveP d k i p c f)

instance (KnownNat (d-1), KnownNat (k-1), KnownNat k, Representable i, Representable p, Representable c) => Distributive (RecursiveP d k i p c) where
    distribute = distributeRep
    collect = collectRep

instance (KnownNat (d-1), KnownNat (k-1), KnownNat k, Representable i, Representable p, Representable c) => Representable (RecursiveP d k i p c)

--------------------------------------------------------------------------------

type RecursiveFunctionAssumptions algo a d k i p c ctx =
    ( HashAlgorithm algo
    , Arithmetic a
    , Binary a
    , KnownNat (d-1)
    , KnownNat (d+1)
    , k ~ 1 -- TODO: This should be generalized once we support multi-round special-sound protocols
    , LayoutFunctor i
    , Zip i
    , LayoutFunctor p
    , LayoutFunctor c
    , c ~ Par1
    , Symbolic ctx
    , BaseField ctx ~ a
    )

-- | Transform a step function into a recursive function
recursiveFunction :: forall algo a d k i p c ctx . RecursiveFunctionAssumptions algo a d k i p c ctx
    => PredicateFunction a i p
    -> ctx (RecursiveI i)
    -> ctx (RecursiveP d k i p c)
    -> ctx (RecursiveI i)
recursiveFunction func z p =
    let
        pRec :: Predicate a (RecursiveI i) (RecursiveP d k i p c)
        pRec = predicate (recursiveFunction @algo @a func)

        accScheme :: AccumulatorScheme d k a (RecursiveI i) c
        accScheme = accumulatorScheme @algo pRec

        x, x' :: ctx i
        x  = hmap recursiveIInput z
        x' = func x (hmap recursivePPayload p)

        piX :: Vector k (c (FieldElement ctx))
        piX = unComp1 $ FieldElement
          <$> unpackWith (Comp1 . fmap Par1 . recursivePCommitments) p

        accX :: AccumulatorInstance k (RecursiveI i) c (FieldElement ctx)
        accX = FieldElement <$> unpackWith (fmap Par1 . recursivePAccInstance) p

        pf :: Vector (d-1) (c (FieldElement ctx))
        pf = unComp1 $ FieldElement
          <$> unpackWith (Comp1 . fmap Par1 . recursivePAccProof) p

        accX' :: AccumulatorInstance k (RecursiveI i) c (FieldElement ctx)
        accX' = verifier accScheme
          (FieldElement <$> unpacked z) piX accX pf

        h, zr :: ctx Par1
        FieldElement h = oracle @algo accX'
        FieldElement zr = zero

        newI, oldI :: ctx (RecursiveI i)
        newI = hliftA2 (\inp (Par1 hsh) -> RecursiveI inp hsh) x' h
        oldI = hliftA2 (\old (Par1 zro) -> RecursiveI old zro) x zr

        flag :: Bool ctx
        flag = Bool $ hmap (Par1 . recursivePFlag) p
    in ifThenElse flag newI oldI
