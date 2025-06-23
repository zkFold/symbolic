-- Avoid reduction overflow error caused by NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}

module ZkFold.Symbolic.Cardano.Types.Output.Datum where

import Data.Constraint (withDict)
import Data.Constraint.Nat (gcdZero)
import ZkFold.Symbolic.Algorithm.Hash.Blake2b (blake2b_256)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.ByteString (emptyByteString)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Cardano.Types.Basic

type DatumHash context = ByteString 256 context

emptyDatumHash :: forall context. Symbolic context => DatumHash context
emptyDatumHash = withDict (gcdZero @8) $ blake2b_256 @0 $ emptyByteString @context
