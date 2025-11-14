module ZkFold.Symbolic.Cardano.Contracts.SponsoredTx (sponsoredTx) where

import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Symbolic.Algorithm.Hash.MiMC
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (BoolType (..))
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.Maybe
import ZkFold.Symbolic.V2 (Symbolic)
import Prelude (($))

import ZkFold.Symbolic.Cardano.Types

-- | Works almost like Babel fees but there's no reward for covering the liability.
sponsoredTx
  :: forall context i1 ri1 i2 ri2
   . Symbolic context
  => KnownRegisters context 32 'Auto
  => KnownRegisters context 64 'Auto
  => Transaction i1 ri1 2 1 0 () context
  -> Transaction i2 ri2 1 1 0 () context
  -> CompatData Bool context
sponsoredTx tx1 tx2 = noExchange && consumesLiability && consumesOutput
 where
  Liability {..} = txLiability tx1

  noExchange :: CompatData Bool context
  noExchange = amount lLiability /= zero && amount lBabel == zero -- There is a liability but no reward. Another party is expected to cover it
  tx1Hash :: ByteString 256 context
  tx1Hash = resize @(ByteString (NumberOfBits context) context) $ from (hashV2 @context tx1)

  consumesLiability :: CompatData Bool context
  consumesLiability =
    isJust $ find (\Input {..} -> outputRefId txiOutputRef == tx1Hash && outputRefIndex txiOutputRef == zero) $ txInputs tx2

  consumesOutput :: CompatData Bool context
  consumesOutput =
    isJust $ find (\Input {..} -> outputRefId txiOutputRef == tx1Hash && outputRefIndex txiOutputRef == one) $ txInputs tx2
