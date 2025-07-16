{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.ArithmeticCircuit.Experimental where

import Control.Applicative              (Applicative, pure)
import Control.DeepSeq                  (NFData (..), NFData1, rwhnf)
import Control.Monad                    (Monad)
import Data.ByteString                  (ByteString)
import Data.Function                    (($), (.))
import Data.Functor                     (Functor, fmap)
import Data.Functor.Identity            (Identity (..))
import Data.Map                         (Map)
import Data.Map                         qualified as M
import Data.Map.Monoidal                (MonoidalMap)
import Data.Map.Monoidal                qualified as MM
import Data.Set                         (Set)
import GHC.Generics                     (Generic)
import Prelude                          (error)
import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit.Context (LookupFunction)
import ZkFold.ArithmeticCircuit.Lookup  (LookupType)
import ZkFold.ArithmeticCircuit.Witness (WitnessF)
import ZkFold.Control.HApplicative      (HApplicative (..))
import ZkFold.Data.HFunctor             (HFunctor (..))
import ZkFold.Data.HFunctor.Classes     (HNFData (..))
import ZkFold.Data.Package              (Package (..))
import ZkFold.Symbolic.Class            (Arithmetic, Symbolic (..))
import ZkFold.Symbolic.MonadCircuit     (MonadCircuit (..), Witness (..))

newtype Polynomial a = MkPolynomial
    { runPolynomial :: forall b. Algebra a b => (ByteString -> b) -> b }

instance NFData (Polynomial a) where
    rnf = rwhnf

data Elem a = MkElem
    { elChildren :: Set (Elem a)
    , elHash     :: ByteString
    , elPolyCon  :: Map ByteString (Polynomial a)
    , elLkpFuns  :: Map ByteString (LookupFunction a)
    , elLookups  :: MonoidalMap (LookupType a) (Set [ByteString])
    , elWitness  :: WitnessF a ByteString
    }
    deriving Generic

instance NFData a => NFData (Elem a)

instance FromConstant c (WitnessF a (Elem a)) => FromConstant c (Elem a) where
    fromConstant (fromConstant -> (witness :: WitnessF a (Elem a))) = MkElem
        { elChildren = error "TODO: find children"
        , elHash = error "TODO: find hash"
        , elPolyCon = M.empty
        , elLkpFuns = M.empty
        , elLookups = MM.empty
        , ..
        }
      where
        elWitness = fmap elHash witness

newtype AC a f = MkAC { runAC :: f (Elem a) }

deriving instance (NFData a, NFData1 f) => NFData (AC a f)

instance NFData a => HNFData (AC a) where
    hliftRnf liftRnf = liftRnf rnf . runAC

instance HFunctor (AC a) where
    hmap f = MkAC . f . runAC

instance HApplicative (AC a) where
    hpure = MkAC
    hliftA2 f (MkAC a) (MkAC b) = MkAC (f a b)

instance Package (AC a) where
    unpackWith f = fmap MkAC . f . runAC
    packWith f = MkAC . f . fmap runAC

instance Arithmetic a => Symbolic (AC a) where
    type BaseField (AC a) = a
    type WitnessField (AC a) = WitnessF a (Elem a)
    witnessF = fmap at . runAC
    fromCircuitF (MkAC es) cf = MkAC $ commit (cf es)

newtype ACM a b = MkACM { runACM :: b }
    deriving (Functor, Applicative, Monad) via Identity

commit :: ACM a (f (Elem a)) -> f (Elem a)
commit = error "TODO: write constraints into elems"

instance Arithmetic a => Witness (Elem a) (WitnessF a (Elem a)) where
    at = pure

instance Arithmetic a => MonadCircuit (Elem a) a (WitnessF a (Elem a)) (ACM a) where
    unconstrained = MkACM . fromConstant
    constraint _ = error "TODO: write down constraint"
    registerFunction _ = error "TODO: write down function"
    lookupConstraint _ = error "TODO: write down lookup constraint"
