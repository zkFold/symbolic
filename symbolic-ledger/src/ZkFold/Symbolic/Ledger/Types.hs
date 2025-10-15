module ZkFold.Symbolic.Ledger.Types (
  EdDSABaseField,
  EdDSAScalarField,
  EdDSAPoint,
  module ZkFold.Symbolic.Ledger.Types.Address,
  module ZkFold.Symbolic.Ledger.Types.Hash,
  module ZkFold.Symbolic.Ledger.Types.State,
  module ZkFold.Symbolic.Ledger.Types.Transaction,
  module ZkFold.Symbolic.Ledger.Types.Value,
  SignatureTransaction,
  SignatureTransactionBatch,
  SignatureState,
) where

import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (NumberOfBits)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup)
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Data.MerkleTree (MerkleTreeSize)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Combinators (GetRegisterSize, RegisterSize (..))
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)

import ZkFold.Symbolic.Ledger.Types.Address
import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Symbolic.Ledger.Types.Orphans ()
import ZkFold.Symbolic.Ledger.Types.State
import ZkFold.Symbolic.Ledger.Types.Transaction
import ZkFold.Symbolic.Ledger.Types.Value

type EdDSABaseField = FFA Jubjub_Base 'Auto

type EdDSAScalarField = FFA Jubjub_Scalar 'Auto

type EdDSAPoint = Jubjub_Point

type SignatureTransaction ud i o a context =
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , KnownNat i
  , KnownNat o
  , KnownNat a
  , KnownNat (ud - 1)
  , KnownNat (MerkleTreeSize ud)
  , KnownFFA Jubjub_Base 'Auto context
  , CyclicGroup (Jubjub_Point context)
  , KnownFFA Jubjub_Scalar 'Auto context
  , KnownNat
      (GetRegisterSize (BaseField context) (NumberOfBits (BaseField context)) 'Auto)
  )

type SignatureTransactionBatch ud i o a t context =
  ( SignatureTransaction ud i o a context
  , KnownNat t
  )

type SignatureState bi bo ud a context =
  ( Symbolic context
  , KnownRegistersAssetQuantity context
  , KnownNat bi
  , KnownNat bo
  )
