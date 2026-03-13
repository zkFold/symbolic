{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Diagnostic benchmark: measure constraint cost of individual validation components.
--
-- This compiles isolated sub-circuits to attribute costs to specific checks.
-- Run with: cabal run bench-circuit-breakdown
module Main where

import Data.Function (($), (&))
import Data.Semigroup (Semigroup (..))
import Data.String (String)
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat, type (-))
import System.IO (IO, putStrLn)
import Text.Show (Show (..))
import ZkFold.Algebra.Class (Zero (..), FromConstant(..), one, (+), (-), (*))
import ZkFold.ArithmeticCircuit (ArithmeticCircuit)
import ZkFold.ArithmeticCircuit qualified as Circuit
import ZkFold.ArithmeticCircuit.Node qualified as C
import ZkFold.Data.Eq ((==))
import ZkFold.Data.Ord ((>), (>=))
import ZkFold.Data.Vector (Vector)
import ZkFold.Prelude (foldl')
import ZkFold.Algebra.Class qualified as Algebra
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Symbolic.Algorithm.EdDSA (eddsaVerify)
import ZkFold.Symbolic.Algorithm.Hash.Poseidon (poseidonCompress2)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (shamirDoubleScale)
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..), false, true, (||))
import ZkFold.Symbolic.Data.Int (Int (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement(..))
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry (..))
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import ZkFold.Symbolic.Data.UInt (toNative)
import Prelude qualified as Haskell

import ZkFold.Algorithm.Hash.MiMC.Constants (mimcConstants)
import ZkFold.Algorithm.Hash.Poseidon (poseidonPermutation, poseidonHash)
import ZkFold.Algorithm.Hash.Poseidon.Constants (defaultPoseidonParams)
import ZkFold.Symbolic.Algorithm.Hash.MiMC qualified as SymbolicMiMC
import qualified Data.Vector as V

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Validation.Transaction (outputHasValueSanity)
import ZkFold.Symbolic.Ledger.Circuit.Compile (ledgerCircuit)

metrics :: String -> ArithmeticCircuit a i o -> String
metrics name circuit =
  name
    <> ": "
    <> show (Circuit.acSizeN circuit)
    <> " poly, "
    <> show (Circuit.acSizeM circuit)
    <> " vars, "
    <> show (Circuit.acSizeL circuit)
    <> " lookup"

main :: IO ()
main = do
  putStrLn "=== Circuit Constraint Breakdown ==="
  putStrLn ""

  putStrLn "--- Merkle Proof (containsAndReplaceRoot) ---"
  putStrLn $ metrics "Merkle proof ud=2 (1 sibling)"
    (C.compileV1 @RollupBF(merkleProofCircuit @2))
  putStrLn $ metrics "Merkle proof ud=4 (3 siblings)"
    (C.compileV1 @RollupBF(merkleProofCircuit @4))
  putStrLn $ metrics "Merkle proof ud=10 (9 siblings)"
    (C.compileV1 @RollupBF(merkleProofCircuit @10))
  putStrLn $ metrics "Merkle proof ud=20 (19 siblings)"
    (C.compileV1 @RollupBF(merkleProofCircuit @20))
  putStrLn ""

  putStrLn "--- UTxO/Transaction Hashing ---"
  putStrLn $ metrics "Hash UTxO (a=1)"
    (C.compileV1 @RollupBF(hashUtxoCircuit @1))
  putStrLn $ metrics "Hash UTxO (a=2)"
    (C.compileV1 @RollupBF(hashUtxoCircuit @2))
  putStrLn $ metrics "Hash UTxO (a=5)"
    (C.compileV1 @RollupBF(hashUtxoCircuit @5))
  putStrLn $ metrics "txId (n=1, a=1)"
    (C.compileV1 @RollupBF(txIdCircuit @1 @1))
  putStrLn $ metrics "txId (n=2, a=2)"
    (C.compileV1 @RollupBF(txIdCircuit @2 @2))
  putStrLn ""

  putStrLn "--- EdDSA ---"
  putStrLn $ metrics "EdDSA verify (1 signature)"
    (C.compileV1 @RollupBF eddsaCircuit1)
  putStrLn ""

  putStrLn "--- Scalar Mul Isolation ---"
  putStrLn $ metrics "scale s G (constant base)"
    (C.compileV1 @RollupBF scaleConstBase)
  putStrLn $ metrics "scale h A (variable base)"
    (C.compileV1 @RollupBF scaleVarBase)
  putStrLn $ metrics "shamirDoubleScale s h A"
    (C.compileV1 @RollupBF shamirBench)
  putStrLn ""

  putStrLn "--- Balance Check (Schwartz-Zippel) ---"
  putStrLn $ metrics "Balance check (a=1, 1in vs 1out)"
    (C.compileV1 @RollupBF(balanceCheckCircuit @1))
  putStrLn $ metrics "Balance check (a=2, 1in vs 1out)"
    (C.compileV1 @RollupBF(balanceCheckCircuit @2))
  putStrLn $ metrics "Balance check (a=5, 1in vs 1out)"
    (C.compileV1 @RollupBF(balanceCheckCircuit @5))
  putStrLn ""

  putStrLn "--- Output Checks ---"
  putStrLn $ metrics "Output sanity check (a=1)"
    (C.compileV1 @RollupBF(sanityCkCircuit @1))
  putStrLn $ metrics "Output sanity check (a=2)"
    (C.compileV1 @RollupBF(sanityCkCircuit @2))
  putStrLn $ metrics "Output equality (a=1)"
    (C.compileV1 @RollupBF(outputEqCircuit @1))
  putStrLn $ metrics "Output equality (a=2)"
    (C.compileV1 @RollupBF(outputEqCircuit @2))
  putStrLn ""

  putStrLn "--- Address Match ---"
  putStrLn $ metrics "Address match (s=1)"
    (C.compileV1 @RollupBF(addressMatchCircuit @1))
  putStrLn $ metrics "Address match (s=2)"
    (C.compileV1 @RollupBF(addressMatchCircuit @2))
  putStrLn $ metrics "Address match (s=3)"
    (C.compileV1 @RollupBF(addressMatchCircuit @3))
  putStrLn ""

  putStrLn "--- hashFn (Poseidon) ---"
  putStrLn $ metrics "hashFn on PublicKey"
    (C.compileV1 @RollupBF hashPkCircuit)
  putStrLn ""

  putStrLn "--- Raw Hash Comparison ---"
  putStrLn $ metrics "MiMC 2->1 (single merkleHash)"
    (C.compileV1 @RollupBF mimcHash2to1)
  putStrLn $ metrics "MiMC 4->1 cascade h(h(a,b),h(c,d))"
    (C.compileV1 @RollupBF mimcHash4to1)
  putStrLn $ metrics "Poseidon perm w=3 (2 FE input)"
    (C.compileV1 @RollupBF poseidonPerm2)
  putStrLn $ metrics "Poseidon sponge (2 FE input)"
    (C.compileV1 @RollupBF poseidonSponge2)
  putStrLn $ metrics "Poseidon sponge (4 FE input)"
    (C.compileV1 @RollupBF poseidonSponge4)
  putStrLn $ metrics "Poseidon OPT w=3 (2 FE input)"
    (C.compileV1 @RollupBF poseidonOptCompress2)
  putStrLn ""

  -- Correctness check: optimized Poseidon should match generic permutation
  let testA = fromConstant @Haskell.Integer 42 :: FieldElement RollupBFInterpreter
      testB = fromConstant @Haskell.Integer 17 :: FieldElement RollupBFInterpreter
      genericResult = poseidonPerm2 (testA :*: testB)
      optResult = poseidonOptCompress2 (testA :*: testB)
  putStrLn $ "Poseidon OPT correctness: " <> (if genericResult Haskell.== optResult then "PASS" else "FAIL")
  putStrLn ""

  putStrLn "=== Full Circuits ==="
  putStrLn $ metrics "Full Ledger bi=1 bo=1 ud=2 a=1 s=1 n=1 t=1"
    (ledgerCircuit @1 @1 @2 @1 @1 @1 @1 @RollupBFInterpreter)
  putStrLn $ metrics "Full Ledger bi=1 bo=1 ud=20 a=1 s=1 n=1 t=1"
    (ledgerCircuit @1 @1 @20 @1 @1 @1 @1 @RollupBFInterpreter)
  putStrLn $ metrics "Full Ledger bi=1 bo=1 ud=2 a=2 s=2 n=2 t=2"
    (ledgerCircuit @1 @1 @2 @2 @2 @2 @2 @RollupBFInterpreter)

-- Sub-circuits: each isolates one validation component.

merkleProofCircuit
  :: forall ud c
   . ( Symbolic c
     , KnownNat (ud - 1)
     )
  => (MerkleEntry ud :*: FieldElement :*: FieldElement) c
  -> (Bool :*: FieldElement) c
merkleProofCircuit (entry :*: newVal :*: root) =
  let (b, r) = MerkleTree.containsAndReplaceRoot entry newVal root
   in b :*: r

hashUtxoCircuit
  :: forall a c
   . ( Symbolic c
     , KnownNat a
     , KnownRegistersAssetQuantity c
     )
  => UTxO a c -> FieldElement c
hashUtxoCircuit utxo = hash utxo & Base.hHash

txIdCircuit
  :: forall n a c
   . ( Symbolic c
     , KnownNat n
     , KnownNat a
     , KnownRegistersAssetQuantity c
     )
  => Transaction n a c -> FieldElement c
txIdCircuit tx = txId tx & Base.hHash

eddsaCircuit1
  :: forall c
   . ( Symbolic c
     , SignatureTransaction 2 1 1 1 c
     )
  => ( PublicKey
       :*: EdDSAPoint
       :*: EdDSAScalarField
       :*: FieldElement
     ) c
  -> Bool c
eddsaCircuit1 (pk :*: rPoint :*: s :*: msg) =
  eddsaVerify hashFn pk msg (rPoint :*: s)

balanceCheckCircuit
  :: forall a c
   . ( Symbolic c
     , KnownNat a
     , SignatureTransaction 2 1 1 a c
     )
  => ( (Vector a :.: AssetValue) :*: (Vector a :.: AssetValue) :*: FieldElement ) c
  -> Bool c
balanceCheckCircuit (inAssets :*: outAssets :*: r) =
  let weightedSum = foldl' (\s av ->
        let qtyFe = toNative (uint (assetQuantity av))
         in s + qtyFe * ((assetPolicy av + one) * r + (assetName av + one)))
      sIn = weightedSum (zero :: FieldElement c) (unComp1 inAssets)
      sOut = weightedSum (zero :: FieldElement c) (unComp1 outAssets)
   in sIn == sOut

sanityCkCircuit
  :: forall a c
   . ( Symbolic c
     , KnownNat a
     , KnownRegistersAssetQuantity c
     )
  => Output a c -> Bool c
sanityCkCircuit = outputHasValueSanity

outputEqCircuit
  :: forall a c
   . ( Symbolic c
     , KnownNat a
     , KnownRegistersAssetQuantity c
     )
  => (Output a :*: Output a) c -> Bool c
outputEqCircuit (a :*: b) = a == b

addressMatchCircuit
  :: forall s c
   . ( Symbolic c
     , KnownNat s
     )
  => ((Vector s :.: FieldElement) :*: FieldElement) c
  -> Bool c
addressMatchCircuit (addrs :*: target) =
  foldl' (\found addr -> found || addr == target) false (unComp1 addrs)

hashPkCircuit
  :: forall c
   . Symbolic c
  => PublicKey c -> FieldElement c
hashPkCircuit = hashFn

scaleConstBase
  :: forall c
   . ( Symbolic c
     , SignatureTransaction 2 1 1 1 c
     )
  => EdDSAScalarField c -> PublicKey c
scaleConstBase s = Algebra.scale s (pointGen @(PublicKey c))

scaleVarBase
  :: forall c
   . ( Symbolic c
     , SignatureTransaction 2 1 1 1 c
     )
  => (EdDSAScalarField :*: PublicKey) c -> PublicKey c
scaleVarBase (s :*: p) = Algebra.scale s p

shamirBench
  :: forall c
   . ( Symbolic c
     , SignatureTransaction 2 1 1 1 c
     )
  => (EdDSAScalarField :*: EdDSAScalarField :*: PublicKey) c -> PublicKey c
shamirBench (s1 :*: s2 :*: p) = shamirDoubleScale s1 s2 p

-- | Single MiMC 2-to-1 hash (same as merkleHash).
mimcHash2to1
  :: Symbolic c
  => (FieldElement :*: FieldElement) c -> FieldElement c
mimcHash2to1 (a :*: b) = SymbolicMiMC.mimcHash2 mimcConstants zero a b

-- | Cascaded MiMC for 4-to-1: h(h(a,b), h(c,d)).
mimcHash4to1
  :: Symbolic c
  => (FieldElement :*: FieldElement :*: FieldElement :*: FieldElement) c -> FieldElement c
mimcHash4to1 (a :*: b :*: c :*: d) =
  let h1 = SymbolicMiMC.mimcHash2 mimcConstants zero a b
      h2 = SymbolicMiMC.mimcHash2 mimcConstants zero c d
   in SymbolicMiMC.mimcHash2 mimcConstants zero h1 h2

-- | Raw Poseidon permutation on 2 field elements (width=3, no sponge).
poseidonPerm2
  :: Symbolic c
  => (FieldElement :*: FieldElement) c -> FieldElement c
poseidonPerm2 (a :*: b) =
  let state = V.fromList [a, b, zero]
      result = poseidonPermutation defaultPoseidonParams state
   in V.head result

-- | Poseidon sponge hash on 2 raw field elements.
poseidonSponge2
  :: Symbolic c
  => (FieldElement :*: FieldElement) c -> FieldElement c
poseidonSponge2 (a :*: b) = poseidonHash defaultPoseidonParams [a, b]

-- | Poseidon sponge hash on 4 raw field elements.
poseidonSponge4
  :: Symbolic c
  => (FieldElement :*: FieldElement :*: FieldElement :*: FieldElement) c -> FieldElement c
poseidonSponge4 (a :*: b :*: c :*: d) = poseidonHash defaultPoseidonParams [a, b, c, d]

-- | Circuit-optimized Poseidon on 2 raw field elements (711 poly constraints).
poseidonOptCompress2
  :: Symbolic c
  => (FieldElement :*: FieldElement) c -> FieldElement c
poseidonOptCompress2 (a :*: b) = poseidonCompress2 a b
