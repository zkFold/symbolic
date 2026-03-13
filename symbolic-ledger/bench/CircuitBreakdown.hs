{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Diagnostic benchmark: measure constraint cost of individual validation components.
--
-- This compiles isolated sub-circuits to attribute costs to specific checks.
-- Run with: cabal run bench-circuit-breakdown
module Main where

import Data.Function (($), (&))
import Data.Functor ((<$>))
import Data.List (zipWith)
import Data.Semigroup (Semigroup (..))
import Data.String (String)
import GHC.Generics (Par1(..), (:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat, type (-))
import Numeric.Natural (Natural)
import System.IO (IO, putStrLn)
import Text.Show (Show (..))
import ZkFold.Algebra.Class (Zero (..), Field, Ring, FromConstant(..), one, (+), (-), (*))
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
import ZkFold.Symbolic.Class (Symbolic (..), fromCircuit2F)
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (shamirDoubleScale)
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..), false, true, (||))
import ZkFold.Symbolic.Data.Int (Int (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement(..))
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree (MerkleEntry (..))
import ZkFold.Symbolic.Data.MerkleTree qualified as MerkleTree
import ZkFold.Symbolic.Data.UInt (toNative)
import ZkFold.Symbolic.MonadCircuit (MonadCircuit, newAssigned)
import ZkFold.Control.Conditional (ifThenElse)
import Prelude qualified as Haskell

import ZkFold.Algorithm.Hash.MiMC.Constants (mimcConstants)
import ZkFold.Algorithm.Hash.Poseidon (poseidonPermutation, poseidonHash)
import ZkFold.Algorithm.Hash.Poseidon.Constants (defaultPoseidonParams, PoseidonParams(..))
import ZkFold.Symbolic.Algorithm.Hash.MiMC qualified as SymbolicMiMC
import qualified Data.Vector as V

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Validation.Transaction (outputHasValueSanity)
import ZkFold.Symbolic.Ledger.Circuit.Compile (ledgerCircuit)

-- ============================================================
-- Circuit-optimized Poseidon permutation
-- ============================================================

-- | Linear combination of circuit variables: sum(coeff_i * var_i) + constant.
-- Used to track state elements without materializing them as constraints.
data LC v a = LC ![(a, v)] !a

lcVar :: Ring a => v -> LC v a
lcVar v = LC [(one, v)] zero

lcConst :: a -> LC v a
lcConst c = LC [] c

lcAdd :: Algebra.AdditiveSemigroup a => LC v a -> LC v a -> LC v a
lcAdd (LC t1 c1) (LC t2 c2) = LC (t1 Haskell.++ t2) (c1 + c2)

lcScale :: Ring a => a -> LC v a -> LC v a
lcScale k (LC ts c) = LC [(k * coeff, v) | (coeff, v) <- ts] (k * c)

-- | Evaluate a linear combination within a ClosedPoly expression.
lcEval :: Algebra.Algebra a x => LC v a -> (v -> x) -> x
lcEval (LC ts c) w =
  Haskell.foldl (\acc (coeff, var) -> acc + fromConstant coeff * w var) (fromConstant c) ts

-- | Apply S-box (x^5) to a linear combination. Produces 4 constraints
-- (materialize LC + 3 for x^5). The materialization is needed because
-- squaring a multi-term LC produces cross-terms that exceed plonk's L*R gate.
sboxLC :: (MonadCircuit v a w m, Field a) => LC v a -> m (LC v a)
sboxLC lc = do
  x   <- materializeLC lc
  xSq <- newAssigned (\w -> w x * w x)
  x4  <- newAssigned (\w -> w xSq * w xSq)
  x5  <- newAssigned (\w -> w x4 * w x)
  Haskell.pure (lcVar x5)

-- | Apply MDS matrix to state. No constraints — just linear combination manipulation.
applyMDSLC :: Ring a => V.Vector (V.Vector a) -> [LC v a] -> [LC v a]
applyMDSLC matrix state =
  [ Haskell.foldl lcAdd (lcConst zero)
      (zipWith (\mij sj -> lcScale mij sj) (V.toList row) state)
  | row <- V.toList matrix
  ]

-- | Add round constants to state. No constraints.
addRoundConstantsLC :: Ring a => [a] -> [LC v a] -> [LC v a]
addRoundConstantsLC rcs = zipWith (\rc s -> lcAdd s (lcConst rc)) rcs

-- | Materialize a linear combination into a concrete circuit variable.
-- Plonk gates have 3 wires (L, R, O), so newAssigned can reference at most
-- 2 existing variables. For LCs with 3+ terms, we decompose incrementally.
materializeLC :: (MonadCircuit v a w m, Ring a) => LC v a -> m v
materializeLC (LC [] c) = newAssigned (\_ -> fromConstant c)
materializeLC (LC [(c1,v1)] c0) =
  newAssigned (\w -> fromConstant c1 * w v1 + fromConstant c0)
materializeLC (LC [(c1,v1),(c2,v2)] c0) =
  newAssigned (\w -> fromConstant c1 * w v1 + fromConstant c2 * w v2 + fromConstant c0)
materializeLC (LC ((c1,v1):(c2,v2):rest) c0) = do
  t <- newAssigned (\w -> fromConstant c1 * w v1 + fromConstant c2 * w v2 + fromConstant c0)
  materializeLC (LC ((one, t) : rest) zero)

-- | Circuit-optimized Poseidon permutation.
-- S-box operations create 3 constraints each. MDS and round constants are free
-- (tracked as linear combinations). Non-S-boxed elements are materialized after
-- each partial round to prevent exponential growth of linear combinations.
-- Cost: fullRounds * width * 4 + partialRounds * (4 + width-1) constraints.
-- For width=3: 8*12 + 57*6 = 438 constraints (vs 1666 for generic).
poseidonPermOpt
  :: (MonadCircuit v a w m, Field a)
  => PoseidonParams a -> [LC v a] -> m [LC v a]
poseidonPermOpt params initState = do
  let w = Haskell.fromIntegral (width params) :: Haskell.Int
      nFirstFull = Haskell.fromIntegral (fullRounds params) :: Haskell.Int
      nPartial = Haskell.fromIntegral (partialRounds params) :: Haskell.Int
      nLastFull = Haskell.fromIntegral (fullRounds params) :: Haskell.Int
      mds = mdsMatrix params
      rcs = roundConstants params

      -- Get round constants for round number r
      getRCs r = [rcs V.! (r Haskell.* w Haskell.+ i) | i <- [0 .. w Haskell.- 1]]

      -- Full round: AddRC -> S-box all elements -> MDS -> Materialize
      fullRound state rIdx = do
        let rcState = addRoundConstantsLC (getRCs rIdx) state
        sboxed <- Haskell.mapM sboxLC rcState
        let mdsResult = applyMDSLC mds sboxed
        materialized <- Haskell.mapM materializeLC mdsResult
        Haskell.pure (Haskell.map lcVar materialized)

      -- Partial round: AddRC -> S-box first element -> MDS -> Materialize non-S-boxed
      -- Materialization prevents exponential growth of linear combinations.
      partialRound state rIdx = do
        let rcState = addRoundConstantsLC (getRCs rIdx) state
        s0' <- sboxLC (Haskell.head rcState)
        let sboxed = s0' : Haskell.tail rcState
            mdsResult = applyMDSLC mds sboxed
        -- Materialize all elements to keep LCs small
        materialized <- Haskell.mapM materializeLC mdsResult
        Haskell.pure (Haskell.map lcVar materialized)

      -- Apply a sequence of rounds
      applyRounds roundFn state startIdx count =
        Haskell.foldl
          (\ms i -> do s <- ms; roundFn s i)
          (Haskell.pure state)
          [startIdx .. startIdx Haskell.+ count Haskell.- 1]

  -- First full rounds
  s1 <- applyRounds fullRound initState 0 nFirstFull
  -- Partial rounds (with materialization after each)
  s2 <- applyRounds partialRound s1 nFirstFull nPartial
  -- Last full rounds
  applyRounds fullRound s2 (nFirstFull Haskell.+ nPartial) nLastFull

-- | Circuit-optimized Poseidon compression: hash 2 field elements into 1.
-- Uses a single Poseidon permutation with width=3 (rate=2, capacity=1).
-- Expected cost: ~438 poly constraints (vs ~1666 for generic implementation).
poseidonHashOpt2
  :: forall c. Symbolic c
  => FieldElement c -> FieldElement c -> FieldElement c
poseidonHashOpt2 (FieldElement a) (FieldElement b) =
  FieldElement $ fromCircuit2F a b $ \(Par1 iA) (Par1 iB) -> do
    let initState = [lcVar iA, lcVar iB, lcConst zero]
    finalState <- poseidonPermOpt defaultPoseidonParams initState
    Par1 <$> materializeLC (Haskell.head finalState)

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
    (C.compileV1 @RollupBF poseidonOpt2)
  putStrLn ""

  -- Correctness check: optimized Poseidon should match generic permutation
  let testA = fromConstant @Haskell.Integer 42 :: FieldElement RollupBFInterpreter
      testB = fromConstant @Haskell.Integer 17 :: FieldElement RollupBFInterpreter
      genericResult = poseidonPerm2 (testA :*: testB)
      optResult = poseidonOpt2 (testA :*: testB)
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

-- | Circuit-optimized Poseidon on 2 raw field elements.
poseidonOpt2
  :: Symbolic c
  => (FieldElement :*: FieldElement) c -> FieldElement c
poseidonOpt2 (a :*: b) = poseidonHashOpt2 a b
