module ZkFold.Symbolic.Cardano.Wrapper where

import ZkFold.Symbolic.Data.Bool (BoolType (..))
import ZkFold.Symbolic.Data.Combinators (RegisterSize (..))
import ZkFold.Symbolic.Data.Eq (Eq ((==)))
import Prelude hiding (Bool, Eq (..), length, splitAt, (&&), (*), (+))

import ZkFold.Symbolic.Cardano.Types

type TxHash context = UInt 64 Auto context

-- TODO: implement transaction hashing
hashFunction :: Transaction inputs rinputs outputs tokens mint datum context -> TxHash context
hashFunction = undefined -- from . blake2b_224 . serialiseData . toBuiltinData

type Contract tx redeemer context = tx context -> redeemer context -> Bool context

-- | Wrap the contract, exposing the transaction hash as the single public input.
symbolicContractWrapper
  :: forall inputs rinputs outputs tokens mint datum redeemer context
   . Eq (TxHash context)
  => Contract (Transaction inputs rinputs outputs tokens mint datum) redeemer context
  -> TxHash context
  -> Transaction inputs rinputs outputs tokens mint datum context
  -> redeemer context
  -> Bool context
symbolicContractWrapper contract hash tx witness =
  let
    -- Сhecking the supplied transaction hash
    conditionCorrectHash = hashFunction tx == hash

    -- Run the contract on the transaction and the redeemer
    conditionContractSucceeds = contract tx witness
   in
    conditionCorrectHash && conditionContractSucceeds
