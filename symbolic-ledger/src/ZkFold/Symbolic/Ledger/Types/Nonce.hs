module ZkFold.Symbolic.Ledger.Types.Nonce (
  Nonce,
) where

import ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- | Denotes the number of transactions sent by an address.
type Nonce context = FieldElement context
