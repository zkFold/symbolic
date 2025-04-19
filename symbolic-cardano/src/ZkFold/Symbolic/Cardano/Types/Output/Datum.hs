{-# OPTIONS_GHC -freduction-depth=0 #-} -- Avoid reduction overflow error caused by NumberOfRegisters

module ZkFold.Symbolic.Cardano.Types.Output.Datum where

import           Data.Constraint                        (withDict)
import           Data.Constraint.Nat                    (gcdZero)
import           Prelude                                hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Algorithm.Hash.Blake2b (blake2b_256)
import           ZkFold.Symbolic.Cardano.Types.Basic
import           ZkFold.Symbolic.Class                  (Symbolic)
import           ZkFold.Symbolic.Data.ByteString        (emptyByteString)

type DatumHash context = ByteString 256 context

emptyDatumHash :: forall context . Symbolic context => DatumHash context
emptyDatumHash = withDict (gcdZero @8) $ blake2b_256 @0 $ emptyByteString @context
