{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-} -- Avoid reduction overflow error caused by NumberOfRegisters

module ZkFold.Symbolic.Cardano.Types.Output (
    module ZkFold.Symbolic.Cardano.Types.Output.Datum,
    Output(..),
    Liability(..)
) where

import           GHC.Generics                               (Generic)
import           Prelude                                    hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude                                    as Haskell

import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor.Classes          (HEq)
import           ZkFold.Symbolic.Cardano.Types.Address      (Address)
import           ZkFold.Symbolic.Cardano.Types.Basic
import           ZkFold.Symbolic.Cardano.Types.Output.Datum
import           ZkFold.Symbolic.Cardano.Types.Value        (SingleAsset, Value)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators           (KnownRegisters, RegisterSize (..))
import           ZkFold.Symbolic.Data.Conditional           (Conditional)
import           ZkFold.Symbolic.Data.Eq                    (Eq)
import           ZkFold.Symbolic.Data.Input                 (SymbolicInput (..))

data Liability context
    = Liability
        { lLiability :: SingleAsset context -- Liability in native tokens
        , lBabel     :: SingleAsset context -- Offer in any other tokens
        }

deriving instance Generic (Liability context)
deriving instance HEq context => Haskell.Eq (Liability context)
deriving instance (Symbolic context, KnownRegisters context 64 Auto) => SymbolicData (Liability context)

-- TODO: derive this automatically
instance
    ( Symbolic context
    , KnownRegisters context 64 Auto
    ) => SymbolicInput (Liability context) where
    isValid Liability{..} = isValid (lLiability, lBabel)

data Output tokens datum context = Output {
        txoAddress   :: Address context,
        txoTokens    :: Value tokens context,
        txoDatumHash :: DatumHash context
    }

deriving instance Generic (Output tokens datum context)
deriving instance HEq context => Haskell.Eq (Output tokens datum context)
deriving instance (KnownNat tokens, Symbolic context, KnownRegisters context 64 Auto) => SymbolicData (Output tokens datum context)

instance
    ( Symbolic context
    , KnownNat tokens
    , KnownRegisters context 64 Auto
    ) => SymbolicInput (Output tokens datum context) where
    isValid (Output a t d) = isValid (a, t,  d)

instance
    ( Symbolic context
    , KnownNat tokens
    , KnownRegisters context 64 Auto
    ) => Conditional (Bool context) (Output tokens datum context)

instance
    ( Symbolic context
    , KnownNat tokens
    , KnownRegisters context 64 Auto
    ) => Eq (Output tokens datum context)
