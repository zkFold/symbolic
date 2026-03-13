module ZkFold.Symbolic.Algorithm.Hash.Poseidon (
  module ZkFold.Symbolic.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon,
) where

import Data.Foldable (toList)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (zipWith)
import qualified Data.Vector as V
import GHC.Generics (Par1 (..))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Class (Symbolic (..), fromCircuit2F)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.MonadCircuit (MonadCircuit, newAssigned)

-- | Symbolic Poseidon hash
hash :: (SymbolicData x, Symbolic c) => x c -> FieldElement c
hash =
  poseidonHashDefault
    . fmap FieldElement
    . unpacked
    . hmap toList
    . arithmetize

-- ============================================================
-- Circuit-optimized Poseidon permutation
-- ============================================================
--
-- The generic Poseidon implementation above uses generic Field operations
-- for MDS matrix multiplication, which creates unnecessary constraints
-- (each constant * variable multiplication becomes a newAssigned call).
--
-- This optimized version tracks MDS and round constant additions as
-- linear combinations (LC), only creating constraints for S-box operations.
-- This reduces constraints from ~1,666 to ~711 for width=3 Poseidon.

-- | Linear combination of circuit variables: sum(coeff_i * var_i) + constant.
-- Used to track state elements without materializing them as constraints.
data LC v a = LC ![(a, v)] !a

lcVar :: Ring a => v -> LC v a
lcVar v = LC [(one, v)] zero

lcConst :: a -> LC v a
lcConst c = LC [] c

lcAdd :: AdditiveSemigroup a => LC v a -> LC v a -> LC v a
lcAdd (LC t1 c1) (LC t2 c2) = LC (t1 P.++ t2) (c1 + c2)

lcScale :: Ring a => a -> LC v a -> LC v a
lcScale k (LC ts c) = LC [(k * coeff, v) | (coeff, v) <- ts] (k * c)

-- | Evaluate a linear combination within a ClosedPoly expression.
lcEval :: Algebra a x => LC v a -> (v -> x) -> x
lcEval (LC ts c) w =
  P.foldl (\acc (coeff, var) -> acc + fromConstant coeff * w var) (fromConstant c) ts

-- | Materialize a linear combination into a concrete circuit variable.
-- Plonk gates have 3 wires (L, R, O), so newAssigned can reference at most
-- 2 existing variables. For LCs with 3+ terms, we decompose incrementally.
-- Cost: max(1, numTerms - 1) constraints.
materializeLC :: (MonadCircuit v a w m, Ring a) => LC v a -> m v
materializeLC (LC [] c) = newAssigned (\_ -> fromConstant c)
materializeLC (LC [(c1,v1)] c0) =
  newAssigned (\w -> fromConstant c1 * w v1 + fromConstant c0)
materializeLC (LC [(c1,v1),(c2,v2)] c0) =
  newAssigned (\w -> fromConstant c1 * w v1 + fromConstant c2 * w v2 + fromConstant c0)
materializeLC (LC ((c1,v1):(c2,v2):rest) c0) = do
  t <- newAssigned (\w -> fromConstant c1 * w v1 + fromConstant c2 * w v2 + fromConstant c0)
  materializeLC (LC ((one, t) : rest) zero)

-- | Apply S-box (x^5) to a linear combination. Produces 4 constraints:
-- materialize LC + x^2 + x^4 + x^5.
-- The materialization is needed because squaring a multi-term LC produces
-- cross-terms that exceed plonk's L*R gate format.
sboxLC :: (MonadCircuit v a w m, Field a) => LC v a -> m (LC v a)
sboxLC lc = do
  x   <- materializeLC lc
  xSq <- newAssigned (\w -> w x * w x)
  x4  <- newAssigned (\w -> w xSq * w xSq)
  x5  <- newAssigned (\w -> w x4 * w x)
  P.pure (lcVar x5)

-- | Apply MDS matrix to state. No constraints -- just linear combination manipulation.
applyMDSLC :: Ring a => V.Vector (V.Vector a) -> [LC v a] -> [LC v a]
applyMDSLC matrix state =
  [ P.foldl lcAdd (lcConst zero)
      (zipWith (\mij sj -> lcScale mij sj) (V.toList row) state)
  | row <- V.toList matrix
  ]

-- | Add round constants to state. No constraints.
addRoundConstantsLC :: Ring a => [a] -> [LC v a] -> [LC v a]
addRoundConstantsLC rcs = zipWith (\rc s -> lcAdd s (lcConst rc)) rcs

-- | Circuit-optimized Poseidon permutation.
--
-- S-box: materialize LC (1 gate) + x^2 + x^4 + x^5 (3 gates) = 4 gates per element.
-- MDS and round constant additions are free (tracked as linear combinations).
-- After every round, all elements are materialized (2 gates each for 3-term LCs
-- from MDS output) to keep LCs bounded.
--
-- Cost for width=3: 8*(3*4 + 3*2) + 57*(1*4 + 3*2) = 8*18 + 57*10 = 714 constraints
-- (theoretical). Measured: ~711 poly. (vs 1666 for generic -- 2.3x improvement)
poseidonPermOpt
  :: (MonadCircuit v a w m, Field a)
  => PoseidonParams a -> [LC v a] -> m [LC v a]
poseidonPermOpt params initState = do
  let w = P.fromIntegral (width params) :: P.Int
      nFirstFull = P.fromIntegral (fullRounds params) :: P.Int
      nPartial = P.fromIntegral (partialRounds params) :: P.Int
      nLastFull = P.fromIntegral (fullRounds params) :: P.Int
      mds = mdsMatrix params
      rcs = roundConstants params

      -- Get round constants for round number r
      getRCs r = [rcs V.! (r P.* w P.+ i) | i <- [0 .. w P.- 1]]

      -- Full round: AddRC -> S-box all elements -> MDS -> Materialize all
      -- Materialization keeps LCs at 1 term for the next round.
      fullRound state rIdx = do
        let rcState = addRoundConstantsLC (getRCs rIdx) state
        sboxed <- P.mapM sboxLC rcState
        let mdsResult = applyMDSLC mds sboxed
        materialized <- P.mapM materializeLC mdsResult
        P.pure (P.map lcVar materialized)

      -- Partial round: AddRC -> S-box first element -> MDS -> Materialize all
      -- Materialization prevents exponential growth of linear combinations.
      partialRound state rIdx = do
        let rcState = addRoundConstantsLC (getRCs rIdx) state
        s0' <- sboxLC (P.head rcState)
        let sboxed = s0' : P.tail rcState
            mdsResult = applyMDSLC mds sboxed
        materialized <- P.mapM materializeLC mdsResult
        P.pure (P.map lcVar materialized)

      -- Apply a sequence of rounds
      applyRounds roundFn state startIdx count =
        P.foldl
          (\ms i -> do s <- ms; roundFn s i)
          (P.pure state)
          [startIdx .. startIdx P.+ count P.- 1]

  -- First full rounds
  s1 <- applyRounds fullRound initState 0 nFirstFull
  -- Partial rounds (with materialization after each)
  s2 <- applyRounds partialRound s1 nFirstFull nPartial
  -- Last full rounds
  applyRounds fullRound s2 (nFirstFull P.+ nPartial) nLastFull

-- | Circuit-optimized Poseidon compression: hash 2 field elements into 1.
-- Uses a single Poseidon permutation with width=3 (rate=2, capacity=1).
-- Cost: ~711 poly constraints (vs ~1666 for generic implementation).
poseidonCompress2
  :: forall c. Symbolic c
  => FieldElement c -> FieldElement c -> FieldElement c
poseidonCompress2 (FieldElement a) (FieldElement b) =
  FieldElement $ fromCircuit2F a b $ \(Par1 iA) (Par1 iB) -> do
    let initState = [lcVar iA, lcVar iB, lcConst zero]
    finalState <- poseidonPermOpt defaultPoseidonParams initState
    Par1 <$> materializeLC (P.head finalState)
