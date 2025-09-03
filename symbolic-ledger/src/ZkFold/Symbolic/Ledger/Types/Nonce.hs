module ZkFold.Symbolic.Ledger.Types.Nonce (
  Nonce,
  KnownRegistersNonce,
) where

import ZkFold.Symbolic.Data.Combinators (KnownRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.UInt (UInt)

-- TODO: Document on why 256 bits is fine.

-- | Denotes the number of transactions sent by an address.
type Nonce context = UInt 256 Auto context

type KnownRegistersNonce context = KnownRegisters context 256 Auto