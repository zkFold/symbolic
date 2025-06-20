module ZkFold.Symbolic.Ledger.Types.Circuit (
  Circuit,
) where

import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude hiding (Bool, Eq, Ord, length, splitAt, (*), (+))

-- TODO: Give proper definition of Circuit. For now it is just a FieldElement to help with instances and avoiding extra class constraints.

{- | Set of commitments to the constraints of the circuit.

'Circuit' is used to represent the smart contract that locks funds at a particular address.
-}
type Circuit context = FieldElement context
