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
import Prelude (Int, pure, reverse)
import qualified Prelude as P

import ZkFold.Algebra.Class (AdditiveSemigroup (..), FromConstant (..), MultiplicativeMonoid (..), MultiplicativeSemigroup (..), Scale (..), Zero (..))
import ZkFold.Algorithm.Hash.Poseidon
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Package (unpacked)
import ZkFold.Symbolic.Class (Symbolic (..), fromCircuit2F)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.MonadCircuit (newAssigned)

-- | Linear combination over a fixed set of variables: s3_0, s3_1, s3_2, plus accumulated o0 outputs.
-- Represents: c0*s3_0 + c1*s3_1 + c2*s3_2 + sum(co0s[i]*o0s[i]) + const
-- The co0s list grows by 1 each partial round (O(57) max).
data PLC a = PLC !a !a !a ![a] !a
--               ^   ^   ^   ^   ^
--              c0  c1  c2 co0s const

-- | Scale all coefficients by a constant
scalePLC :: MultiplicativeSemigroup a => a -> PLC a -> PLC a
scalePLC s (PLC c0 c1 c2 co0s cnst) =
  PLC (s*c0) (s*c1) (s*c2) (P.map (s*) co0s) (s*cnst)

-- | Add two PLCs (co0s lists padded with zeros implicitly)
addPLC :: (AdditiveSemigroup a) => PLC a -> PLC a -> PLC a
addPLC (PLC a0 a1 a2 ao0s ac) (PLC b0 b1 b2 bo0s bc) =
  PLC (a0+b0) (a1+b1) (a2+b2) (zipWithPad ao0s bo0s) (ac+bc)
  where
    zipWithPad [] ys = ys
    zipWithPad xs [] = xs
    zipWithPad (x:xs) (y:ys) = (x+y) : zipWithPad xs ys

-- | Add a constant to PLC
addConstPLC :: AdditiveSemigroup a => a -> PLC a -> PLC a
addConstPLC k (PLC c0 c1 c2 co0s cnst) = PLC c0 c1 c2 co0s (cnst+k)

-- | PLC for s3_0 variable (coef 1 on s3_0)
varS30 :: (MultiplicativeMonoid a, Zero a) => PLC a
varS30 = PLC one zero zero [] zero

-- | PLC for s3_1 variable
varS31 :: (MultiplicativeMonoid a, Zero a) => PLC a
varS31 = PLC zero one zero [] zero

-- | PLC for s3_2 variable
varS32 :: (MultiplicativeMonoid a, Zero a) => PLC a
varS32 = PLC zero zero one [] zero

-- | PLC for a new o0 output (extends coefficient list by 1)
-- numO0s is the current length of o0s list BEFORE this output is added
varO0New :: (MultiplicativeMonoid a, Zero a) => Int -> PLC a
varO0New numO0s = PLC zero zero zero (P.replicate numO0s zero P.++ [one]) zero

-- | Optimized Poseidon hash for two field element inputs.
-- Uses ~240 constraints by tracking elements 1,2 as linear combinations
-- during partial rounds instead of materializing them.
poseidonHash2
  :: forall c
   . Symbolic c
  => FieldElement c
  -> FieldElement c
  -> FieldElement c
poseidonHash2 (FieldElement x1) (FieldElement x2) = FieldElement $
  fromCircuit2F x1 x2 $ \(Par1 i1) (Par1 i2) -> do
    -- Extract constants (all typed as BaseField c)
    let mds :: V.Vector (V.Vector (BaseField c))
        mds = mdsMatrixBLS12381
        rc :: V.Vector (BaseField c)
        rc = roundConstantsBLS12381
        m00 = mds V.! 0 V.! 0; m01 = mds V.! 0 V.! 1; m02 = mds V.! 0 V.! 2
        m10 = mds V.! 1 V.! 0; m11 = mds V.! 1 V.! 1; m12 = mds V.! 1 V.! 2
        m20 = mds V.! 2 V.! 0; m21 = mds V.! 2 V.! 1; m22 = mds V.! 2 V.! 2
        r :: Int -> BaseField c
        r i = rc V.! i

        -- Evaluate PLC given base vars and o0 list
        evalPLC :: (Scale (BaseField c) x, FromConstant (BaseField c) x, AdditiveSemigroup x)
                => PLC (BaseField c) -> v -> v -> v -> [v] -> (v -> x) -> x
        evalPLC (PLC c0 c1 c2 co0s cnst) s30 s31 s32 o0s w =
          scale c0 (w s30) + scale c1 (w s31) + scale c2 (w s32)
          + P.foldr (\(coef, var) acc -> scale coef (w var) + acc) (fromConstant cnst) (P.zip co0s o0s)

    -- Round 0: initial state [i1, i2, 0] with round constants (6 constraints)
    let sbox0 = r 0; sbox1 = r 1; sbox2c = let t = r 2; t2 = t*t; t4 = t2*t2 in t4*t
    s0_0 <- do
      t2 <- newAssigned $ \w -> let t = w i1 + fromConstant sbox0 in t * t
      t4 <- newAssigned $ \w -> w t2 * w t2
      newAssigned $ \w -> let t = w i1 + fromConstant sbox0 in w t4 * t
    s0_1 <- do
      t2 <- newAssigned $ \w -> let t = w i2 + fromConstant sbox1 in t * t
      t4 <- newAssigned $ \w -> w t2 * w t2
      newAssigned $ \w -> let t = w i2 + fromConstant sbox1 in w t4 * t

    -- Round 1: MDS(s0) + rc[3..5], S-box (9 constraints, elem 2 constant)
    let cc1_0 = m02 * sbox2c + r 3
        cc1_1 = m12 * sbox2c + r 4
        cc1_2 = m22 * sbox2c + r 5
    s1_0 <- do
      t2 <- newAssigned $ \w -> let t = scale m00 (w s0_0) + scale m01 (w s0_1) + fromConstant cc1_0 in t * t
      t4 <- newAssigned $ \w -> w t2 * w t2
      newAssigned $ \w -> let t = scale m00 (w s0_0) + scale m01 (w s0_1) + fromConstant cc1_0 in w t4 * t
    s1_1 <- do
      t2 <- newAssigned $ \w -> let t = scale m10 (w s0_0) + scale m11 (w s0_1) + fromConstant cc1_1 in t * t
      t4 <- newAssigned $ \w -> w t2 * w t2
      newAssigned $ \w -> let t = scale m10 (w s0_0) + scale m11 (w s0_1) + fromConstant cc1_1 in w t4 * t
    s1_2 <- do
      t2 <- newAssigned $ \w -> let t = scale m20 (w s0_0) + scale m21 (w s0_1) + fromConstant cc1_2 in t * t
      t4 <- newAssigned $ \w -> w t2 * w t2
      newAssigned $ \w -> let t = scale m20 (w s0_0) + scale m21 (w s0_1) + fromConstant cc1_2 in w t4 * t

    -- Rounds 2-3 (full rounds, 18 constraints)
    let fullRound (v0, v1, v2) idx = do
          let rc0 = r idx; rc1 = r (idx P.+ 1); rc2 = r (idx P.+ 2)
          o0 <- do
            t2 <- newAssigned $ \w -> let t = scale m00 (w v0) + scale m01 (w v1) + scale m02 (w v2) + fromConstant rc0 in t * t
            t4 <- newAssigned $ \w -> w t2 * w t2
            newAssigned $ \w -> let t = scale m00 (w v0) + scale m01 (w v1) + scale m02 (w v2) + fromConstant rc0 in w t4 * t
          o1 <- do
            t2 <- newAssigned $ \w -> let t = scale m10 (w v0) + scale m11 (w v1) + scale m12 (w v2) + fromConstant rc1 in t * t
            t4 <- newAssigned $ \w -> w t2 * w t2
            newAssigned $ \w -> let t = scale m10 (w v0) + scale m11 (w v1) + scale m12 (w v2) + fromConstant rc1 in w t4 * t
          o2 <- do
            t2 <- newAssigned $ \w -> let t = scale m20 (w v0) + scale m21 (w v1) + scale m22 (w v2) + fromConstant rc2 in t * t
            t4 <- newAssigned $ \w -> w t2 * w t2
            newAssigned $ \w -> let t = scale m20 (w v0) + scale m21 (w v1) + scale m22 (w v2) + fromConstant rc2 in w t4 * t
          pure (o0, o1, o2)

    (s2_0, s2_1, s2_2) <- fullRound (s1_0, s1_1, s1_2) 6
    (s3_0, s3_1, s3_2) <- fullRound (s2_0, s2_1, s2_2) 9

    -- Partial rounds with PLC tracking (57 rounds × 3 constraints = 171)
    -- State: (o0s accumulated in reverse, plc1, plc2)
    -- o0s stored in reverse for efficient consing
    let partialRoundPLC (o0sRev, plc1, plc2) idx = do
          let rc0 = r idx; rc1 = r (idx P.+ 1); rc2 = r (idx P.+ 2)
              numO0s = P.length o0sRev
              -- Build PLC for element 0's S-box input:
              -- If first partial round: m00*s3_0 + m01*plc1 + m02*plc2 + rc0
              -- Otherwise: m00*(latest o0) + m01*plc1 + m02*plc2 + rc0
              plc0 = case o0sRev of
                [] -> addConstPLC rc0 $ addPLC (scalePLC m00 varS30)
                                      $ addPLC (scalePLC m01 plc1) (scalePLC m02 plc2)
                _  -> addConstPLC rc0 $ addPLC (scalePLC m00 (varO0New (numO0s P.- 1)))
                                      $ addPLC (scalePLC m01 plc1) (scalePLC m02 plc2)
              o0sList = reverse o0sRev  -- for evalPLC

          -- S-box for element 0 (3 constraints)
          t2 <- newAssigned $ \w -> let t = evalPLC plc0 s3_0 s3_1 s3_2 o0sList w in t * t
          t4 <- newAssigned $ \w -> w t2 * w t2
          o0_new <- newAssigned $ \w -> let t = evalPLC plc0 s3_0 s3_1 s3_2 o0sList w in w t4 * t

          -- Update PLCs for elements 1 and 2 (no constraints!)
          let plc1' = addConstPLC rc1 $ addPLC (scalePLC m10 (varO0New numO0s))
                                      $ addPLC (scalePLC m11 plc1) (scalePLC m12 plc2)
              plc2' = addConstPLC rc2 $ addPLC (scalePLC m20 (varO0New numO0s))
                                      $ addPLC (scalePLC m21 plc1) (scalePLC m22 plc2)

          pure (o0_new : o0sRev, plc1', plc2')

    -- Initialize: plc1 = s3_1, plc2 = s3_2
    let plc1_init = varS31 :: PLC (BaseField c)
        plc2_init = varS32 :: PLC (BaseField c)
        partialIndices = [12, 15 .. 12 P.+ 56 P.* 3] :: [Int]
    (o0sRevFinal, plc1Final, plc2Final) <- foldM partialRoundPLC ([], plc1_init, plc2_init) partialIndices

    -- Materialize PLCs (2 constraints)
    let o0sFinal = reverse o0sRevFinal
        pLast_0 = case o0sRevFinal of
          (x:_) -> x
          [] -> s3_0  -- shouldn't happen with 57 rounds
    p1 <- newAssigned $ \w -> evalPLC plc1Final s3_0 s3_1 s3_2 o0sFinal w
    p2 <- newAssigned $ \w -> evalPLC plc2Final s3_0 s3_1 s3_2 o0sFinal w

    -- Full rounds 61-64 (36 constraints)
    let ri = 12 P.+ 57 P.* 3
    (f0_0, f0_1, f0_2) <- fullRound (pLast_0, p1, p2) ri
    (f1_0, f1_1, f1_2) <- fullRound (f0_0, f0_1, f0_2) (ri P.+ 3)
    (f2_0, f2_1, f2_2) <- fullRound (f1_0, f1_1, f1_2) (ri P.+ 6)
    (f3_0, _, _) <- fullRound (f2_0, f2_1, f2_2) (ri P.+ 9)

    pure (Par1 f3_0)

-- | Symbolic Poseidon hash using optimized 2-input permutation.
-- For N elements, chains poseidonHash2 calls:
--   hash [a,b] = poseidonHash2 a b
--   hash [a,b,c,...] = foldl (\h x -> poseidonHash2 h x) (poseidonHash2 a b) [c,...]
hash :: (SymbolicData x, Symbolic c) => x c -> FieldElement c
hash x = case fmap FieldElement (unpacked (hmap toList (arithmetize x))) of
  []         -> poseidonHash2 zero zero
  [a]        -> poseidonHash2 a zero
  [a, b]     -> poseidonHash2 a b
  (a:b:rest) -> P.foldl poseidonHash2 (poseidonHash2 a b) rest
