{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -freduction-depth=0 #-} -- Avoid reduction overflow error caused by NumberOfRegisters

module ZkFold.Symbolic.Cardano.Types.Input where

import           GHC.Generics                            (Generic)
import           Prelude                                 hiding (Bool, Eq, length, splitAt, (*), (+))
import qualified Prelude                                 as Haskell

import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Symbolic.Cardano.Types.Address   (Address)
import           ZkFold.Symbolic.Cardano.Types.Output    (DatumHash, Output, txoAddress, txoDatumHash, txoTokens)
import           ZkFold.Symbolic.Cardano.Types.OutputRef (OutputRef)
import           ZkFold.Symbolic.Cardano.Types.Value     (Value)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators        (KnownRegisters, RegisterSize (..))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Input              (SymbolicInput, isValid)

data Input tokens datum context = Input  {
        txiOutputRef :: OutputRef context,
        txiOutput    :: Output tokens datum context
    }
    deriving (Generic)

instance
    ( Symbolic context
    , KnownNat tokens
    , KnownRegisters context 32 Auto
    , KnownRegisters context 64 Auto
    ) => Conditional (Bool context) (Input tokens datum context)

instance
    ( Symbolic context
    , KnownNat tokens
    , KnownRegisters context 32 Auto
    , KnownRegisters context 64 Auto
    ) => Eq (Input tokens datum context)

deriving instance HEq context => Haskell.Eq (Input tokens datum context)

instance
  ( Symbolic context, KnownNat tokens
  , KnownRegisters context 32 Auto
  , KnownRegisters context 64 Auto
  ) => SymbolicData (Input tokens datum context)

instance
  ( Symbolic context
  , KnownNat tokens
  , KnownRegisters context 32 Auto
  , KnownRegisters context 64 Auto
  ) => SymbolicInput (Input tokens datum context)

txiAddress :: Input tokens datum context -> Address context
txiAddress (Input _ txo) = txoAddress txo

txiTokens :: Input tokens datum context -> Value tokens context
txiTokens (Input _ txo) = txoTokens txo

txiDatumHash :: Input tokens datum context -> DatumHash context
txiDatumHash (Input _ txo) = txoDatumHash txo
