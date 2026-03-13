module ZkFold.Symbolic.Algorithm.Hash.Poseidon (
  module ZkFold.Symbolic.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon,
) where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Function (($))
import Data.Functor (fmap)
import qualified Data.Vector as V
import GHC.Generics (Par1 (..))
import Prelude (Int, pure)
import qualified Prelude as P

import ZkFold.Algebra.Class (
  AdditiveSemigroup (..),
  FromConstant (..),
  MultiplicativeSemigroup (..),
  Scale (..),
  Zero (..),
 )
import ZkFold.Algorithm.Hash.Poseidon hiding (mdsLayer)
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Class (Symbolic (..), fromCircuit2F)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.MonadCircuit (newAssigned)

-- | Poseidon hash for two field element inputs.
-- Uses 627 vanilla Plonk constraints.
-- S-box: 3 constraints (t², t⁴, t⁵) when input is single variable + constant.
-- MDS (3 inputs): 6 constraints (2 per output, chaining to keep ≤3 vars).
-- MDS (2 inputs): 3 constraints (1 per output).
poseidonHash2
  :: forall c
   . Symbolic c
  => FieldElement c
  -> FieldElement c
  -> FieldElement c
poseidonHash2 (FieldElement x1) (FieldElement x2) = FieldElement $
  fromCircuit2F x1 x2 $ \(Par1 i1) (Par1 i2) -> do
    -- Extract MDS matrix and round constants
    let mds :: V.Vector (V.Vector (BaseField c))
        mds = mdsMatrixBLS12381
        rc :: V.Vector (BaseField c)
        rc = roundConstantsBLS12381
        m00 = mds V.! 0 V.! 0
        m01 = mds V.! 0 V.! 1
        m02 = mds V.! 0 V.! 2
        m10 = mds V.! 1 V.! 0
        m11 = mds V.! 1 V.! 1
        m12 = mds V.! 1 V.! 2
        m20 = mds V.! 2 V.! 0
        m21 = mds V.! 2 V.! 1
        m22 = mds V.! 2 V.! 2
        r :: Int -> BaseField c
        r i = rc V.! i

        -- S-box: x^5 for single variable + constant (3 constraints)
        sbox1 v (c :: BaseField c) = do
          t2 <- newAssigned $ \w -> let t = w v + fromConstant c in t * t
          t4 <- newAssigned $ \w -> w t2 * w t2
          newAssigned $ \w -> let t = w v + fromConstant c in w t4 * t

        -- MDS layer for 3 input variables: must chain to keep ≤3 vars per constraint
        -- out = m0*v0 + m1*v1 + m2*v2 + const → 2 constraints per output
        -- Gate 1: tmp = m0*v0 + m1*v1 (3 vars: tmp, v0, v1)
        -- Gate 2: out = tmp + m2*v2 + const (3 vars: out, tmp, v2)
        mdsLayer (v0, v1, v2) ((c0, c1, c2) :: (BaseField c, BaseField c, BaseField c)) = do
          tmp0 <- newAssigned $ \w -> scale m00 (w v0) + scale m01 (w v1)
          o0 <- newAssigned $ \w -> w tmp0 + scale m02 (w v2) + fromConstant c0
          tmp1 <- newAssigned $ \w -> scale m10 (w v0) + scale m11 (w v1)
          o1 <- newAssigned $ \w -> w tmp1 + scale m12 (w v2) + fromConstant c1
          tmp2 <- newAssigned $ \w -> scale m20 (w v0) + scale m21 (w v1)
          o2 <- newAssigned $ \w -> w tmp2 + scale m22 (w v2) + fromConstant c2
          pure (o0, o1, o2)

        -- Full round: S-box on each element, then MDS (15 constraints)
        -- rc_idx points to first of 3 round constants for this round
        fullRound (v0, v1, v2) rc_idx = do
          s0 <- sbox1 v0 (r rc_idx)
          s1 <- sbox1 v1 (r (rc_idx P.+ 1))
          s2 <- sbox1 v2 (r (rc_idx P.+ 2))
          let z = zero :: BaseField c
          mdsLayer (s0, s1, s2) (z, z, z)

        -- Partial round: S-box only on element 0, then MDS (9 constraints)
        -- Elements 1,2 get round constants added before MDS, via MDS * [0, c1, c2]
        partialRound (v0, v1, v2) rc_idx = do
          s0 <- sbox1 v0 (r rc_idx)
          let c1 = r (rc_idx P.+ 1)
              c2 = r (rc_idx P.+ 2)
              c0' = m01 * c1 + m02 * c2
              c1' = m11 * c1 + m12 * c2
              c2' = m21 * c1 + m22 * c2
          mdsLayer (s0, v1, v2) (c0', c1', c2')

        -- MDS for Round 0: only 2 input variables, element 2 is constant sbox2c
        -- Each output is: m_0*v0 + m_1*v1 + m_2*sbox2c (2 variables, valid Plonk)
        mdsLayer2 (v0, v1) sbox2c_val = do
          o0 <- newAssigned $ \w -> scale m00 (w v0) + scale m01 (w v1) + fromConstant (m02 * sbox2c_val)
          o1 <- newAssigned $ \w -> scale m10 (w v0) + scale m11 (w v1) + fromConstant (m12 * sbox2c_val)
          o2 <- newAssigned $ \w -> scale m20 (w v0) + scale m21 (w v1) + fromConstant (m22 * sbox2c_val)
          pure (o0, o1, o2)

    -- Round 0: Initial state is [i1, i2, 0], S-box with rc[0..2], then MDS
    -- Element 2 starts as 0, so (0 + rc[2])^5 is constant - 6 constraints for S-boxes 0,1
    let sbox2c = let t = r 2; t2 = t * t; t4 = t2 * t2 in t4 * t
    s0_0 <- sbox1 i1 (r 0)
    s0_1 <- sbox1 i2 (r 1)
    -- MDS with constant sbox2c folded in (3 constraints, 2 variables each)
    (s1_0, s1_1, s1_2) <- mdsLayer2 (s0_0, s0_1) sbox2c

    -- Full rounds 1-3 (round 1 uses rc[3..5], etc.)
    (s2_0, s2_1, s2_2) <- fullRound (s1_0, s1_1, s1_2) 3
    (s3_0, s3_1, s3_2) <- fullRound (s2_0, s2_1, s2_2) 6
    (s4_0, s4_1, s4_2) <- fullRound (s3_0, s3_1, s3_2) 9

    -- Partial rounds 4-60 (57 rounds, rc[12..182])
    let partialIndices = [12, 15 .. 12 P.+ 56 P.* 3] :: [Int]
    (pf_0, pf_1, pf_2) <- foldM partialRound (s4_0, s4_1, s4_2) partialIndices

    -- Final full rounds 61-64 (rc[183..194])
    let ri = 12 P.+ 57 P.* 3 -- 183
    (f1_0, f1_1, f1_2) <- fullRound (pf_0, pf_1, pf_2) ri
    (f2_0, f2_1, f2_2) <- fullRound (f1_0, f1_1, f1_2) (ri P.+ 3)
    (f3_0, f3_1, f3_2) <- fullRound (f2_0, f2_1, f2_2) (ri P.+ 6)
    (f4_0, _, _) <- fullRound (f3_0, f3_1, f3_2) (ri P.+ 9)

    pure (Par1 f4_0)

-- | Symbolic Poseidon hash using optimized 2-input permutation.
-- For N elements, chains poseidonHash2 calls:
--   hash [a,b] = poseidonHash2 a b
--   hash [a,b,c,...] = foldl (\h x -> poseidonHash2 h x) (poseidonHash2 a b) [c,...]
hash :: (SymbolicData x, Symbolic c) => x c -> FieldElement c
hash x = case fmap FieldElement (unpacked (hmap toList (arithmetize x))) of
  [] -> poseidonHash2 zero zero
  [a] -> poseidonHash2 a zero
  [a, b] -> poseidonHash2 a b
  (a : b : rest) -> P.foldl poseidonHash2 (poseidonHash2 a b) rest
