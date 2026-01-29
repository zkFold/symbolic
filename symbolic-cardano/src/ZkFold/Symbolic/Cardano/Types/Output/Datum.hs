module ZkFold.Symbolic.Cardano.Types.Output.Datum where

import Data.Constraint (withDict)
import Data.Constraint.Nat (gcdZero)
import ZkFold.Symbolic.Algorithm.Hash.Blake2b (blake2b_256)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.ByteString (emptyByteString)
import ZkFold.Symbolic.Data.UInt (KnownUInt)
import Prelude hiding (Bool, Eq, length, splitAt, (*), (+))

import ZkFold.Symbolic.Cardano.Types.Basic

type DatumHash context = ByteString 256 context

emptyDatumHash :: forall c. (Symbolic c, KnownUInt 64 c) => DatumHash c
emptyDatumHash = withDict (gcdZero @8) $ blake2b_256 @0 $ emptyByteString
