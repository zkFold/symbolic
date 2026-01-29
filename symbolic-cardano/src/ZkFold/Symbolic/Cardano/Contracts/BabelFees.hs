module ZkFold.Symbolic.Cardano.Contracts.BabelFees (babelFees) where

import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Symbolic.Algorithm.Hash.MiMC
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (BoolType (..))
import ZkFold.Symbolic.Data.ByteString (feToBSbe, resize)
import ZkFold.Symbolic.Data.Maybe
import ZkFold.Symbolic.Data.UInt (KnownUInt)
import Prelude (($))

import ZkFold.Symbolic.Cardano.Types

-- | The original paper: https://arxiv.org/pdf/2106.01161
-- It introduces a babel fee output holding a liability and a reward for covering it.
-- This contract checks that the second transaction consumes the liability and pays back the transaction fee
babelFees
  :: forall context i1 ri1 i2 ri2
   . (Symbolic context, KnownUInt 32 context, KnownUInt 64 context)
  => Transaction i1 ri1 2 1 0 () context
  -> Transaction i2 ri2 1 1 0 () context
  -> Bool context
babelFees tx1 tx2 = consumesLiability && consumesOutput
 where
  tx1Hash :: ByteString 256 context
  tx1Hash = resize $ feToBSbe $ hash @context tx1

  consumesLiability :: Bool context
  consumesLiability =
    isJust $ find (\Input {..} -> outputRefId txiOutputRef == tx1Hash && outputRefIndex txiOutputRef == zero) $ txInputs tx2

  consumesOutput :: Bool context
  consumesOutput =
    isJust $ find (\Input {..} -> outputRefId txiOutputRef == tx1Hash && outputRefIndex txiOutputRef == one) $ txInputs tx2
