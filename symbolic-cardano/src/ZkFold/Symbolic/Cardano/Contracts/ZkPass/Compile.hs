{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImportQualifiedPost #-}

module ZkFold.Symbolic.Cardano.Contracts.ZkPass.Compile (
  
) where
  
import GHC.Generics (Generic, Generic1, Par1 (..), U1 (..), (:*:) (..), (:.:) (..))
import Data.Type.Equality (type (~))
import Data.Word (Word8)
import GHC.Generics (Generic, Generic1, Par1 (..), U1 (..), (:*:) (..), (:.:) (..))
import GHC.Natural (Natural, naturalToInteger)
import GHC.TypeNats (KnownNat, type (+), type (^))
import ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class      hiding (Point)
import           ZkFold.Symbolic.Data.EllipticCurve.Point (Point (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (
  BLS12_381_G1_CompressedPoint,
  BLS12_381_G1_JacobianPoint,
  BLS12_381_G2_JacobianPoint, BLS12_381_Scalar,
 )
import ZkFold.Algebra.EllipticCurve.Class (Compressible (..), CyclicGroup (..), Weierstrass) 
import ZkFold.Algebra.Field (Zp, fromZp)
import ZkFold.Algebra.Number qualified as Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit
import ZkFold.ArithmeticCircuit.Node qualified as C
import ZkFold.Data.Binary (toByteString)
import ZkFold.Data.Vector (Vector)
import ZkFold.FFI.Rust.Plonkup (rustPlonkupProve)
import ZkFold.Prelude (log2ceiling)
import ZkFold.Protocol.NonInteractiveProof (
  FromTranscript (..),
  ToTranscript (..),
 )
import ZkFold.Protocol.NonInteractiveProof as NP (
  NonInteractiveProof (..),
  TrustedSetup (..),
 )
import ZkFold.Protocol.Plonkup (Plonkup (..))
import ZkFold.Protocol.Plonkup.OffChain.Cardano
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..))
import ZkFold.Protocol.Plonkup.Utils (getParams)
import ZkFold.Protocol.Plonkup.Verifier.Commitments
import ZkFold.Protocol.Plonkup.Verifier.Setup
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (..))
import ZkFold.Symbolic.Class (BaseField, Symbolic (..))
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Hash (Hash (..), preimage)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.MerkleTree (KnownMerkleTree, MerkleTree (mHash))
import ZkFold.Symbolic.Interpreter
import ZkFold.Symbolic.Cardano.Contracts.ZkPass
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Algorithm.Hash.Keccak

type ZkPassBF = Zp BLS12_381_Scalar

type ZkPassContractInput = ZKPassResult

type ZkPassContractInputLayout =
  Layout
    (ZkPassContractInput :*: U1)
    (Order ZkPassBF)

type ZkPassContractInputPayload =
  Payload
    (ZkPassContractInput :*: U1)
    (Order ZkPassBF)

type ZkPassContractCompiledInput = 
  ZkPassContractInputLayout :*: ZkPassContractInputPayload

type ZkPassContractOutputLayout = Par1

type ZkPassCircuit =
  ArithmeticCircuit ZkPassBF (ZkPassContractCompiledInput) (ZkPassContractOutputLayout)

type ZkPassContractOutput = FieldElement

-- zksPassContract
--   :: forall n p q curve ctx.
--      ( KnownNat n
--      , KnownNat (NumberOfRegisters (BaseField ctx) n 'Auto)
--      , KnownNat (GetRegisterSize (BaseField ctx) n 'Auto)
--      , KnownNat (GetRegisterSize (BaseField ctx) 256 'Auto)
--      , KnownFFA p 'Auto ctx
--      , KnownFFA q 'Auto ctx
--      , ScalarFieldOf (Point (Weierstrass curve) (FFA q 'Auto) ctx) ~ FFA p 'Auto ctx
--      , CyclicGroup (Point (Weierstrass curve) (FFA q 'Auto) ctx)
--      , Keccak "Keccak256" ctx 1024
--      , Keccak "Keccak256" ctx 672
--      , Keccak "Keccak256" ctx 512
--      )
--   => ZkPassContractInput ctx -> ZkPassContractOutput ctx
-- zksPassContract  = zkPassSymbolicVerifier

zksPassCircuit
  :: forall n p q curve ctx.
  ( Signature n p q curve ctx 
  , SymbolicInput (ZKPassResult)
  )
  => ZkPassCircuit
zksPassCircuit = C.compileV1 @ZkPassBF zkPassSymbolicVerifier