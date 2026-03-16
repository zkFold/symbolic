module ZkFold.Symbolic.Algorithm.Hash.Poseidon (
  module ZkFold.Symbolic.Algorithm.Hash.Poseidon,
  module ZkFold.Algorithm.Hash.Poseidon,
) where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Function (($))
import Data.Functor (fmap)
import qualified Data.Vector as V
import GHC.Generics (Par1 (..), (:*:) (..))
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
import ZkFold.Symbolic.Class (Symbolic (..), fromCircuit3F)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.MonadCircuit (newAssigned)

-- | Symbolic Poseidon permutation on a 3-element state (width=3), parameterized by 'PoseidonParams'.
-- Takes (s0, s1, s2) and applies the Poseidon permutation, returning all 3 output elements.
-- For BLS12-381 default parameters, uses 630 vanilla Plonk constraints.
poseidonPermute3
  :: forall c
   . Symbolic c
  => PoseidonParams (BaseField c)
  -> FieldElement c
  -> FieldElement c
  -> FieldElement c
  -> (FieldElement c, FieldElement c, FieldElement c)
poseidonPermute3 params (FieldElement x0) (FieldElement x1) (FieldElement x2) =
  let result = fromCircuit3F x0 x1 x2 $ \(Par1 i0) (Par1 i1) (Par1 i2) -> do
        let mds = mdsMatrix params
            rc = roundConstants params
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
            mdsLayer (v0, v1, v2) ((c0, c1, c2) :: (BaseField c, BaseField c, BaseField c)) = do
              tmp0 <- newAssigned $ \w -> scale m00 (w v0) + scale m01 (w v1)
              o0 <- newAssigned $ \w -> w tmp0 + scale m02 (w v2) + fromConstant c0
              tmp1 <- newAssigned $ \w -> scale m10 (w v0) + scale m11 (w v1)
              o1 <- newAssigned $ \w -> w tmp1 + scale m12 (w v2) + fromConstant c1
              tmp2 <- newAssigned $ \w -> scale m20 (w v0) + scale m21 (w v1)
              o2 <- newAssigned $ \w -> w tmp2 + scale m22 (w v2) + fromConstant c2
              pure (o0, o1, o2)

            -- Full round: S-box on each element, then MDS (15 constraints)
            fullRound (v0, v1, v2) rc_idx = do
              s0 <- sbox1 v0 (r rc_idx)
              s1 <- sbox1 v1 (r (rc_idx P.+ 1))
              s2 <- sbox1 v2 (r (rc_idx P.+ 2))
              let z = zero :: BaseField c
              mdsLayer (s0, s1, s2) (z, z, z)

            -- Partial round: S-box only on element 0, then MDS (9 constraints)
            -- Round constants for elements 1,2 are folded into MDS output constants
            partialRound (v0, v1, v2) rc_idx = do
              s0 <- sbox1 v0 (r rc_idx)
              let c1 = r (rc_idx P.+ 1)
                  c2 = r (rc_idx P.+ 2)
                  c0' = m01 * c1 + m02 * c2
                  c1' = m11 * c1 + m12 * c2
                  c2' = m21 * c1 + m22 * c2
              mdsLayer (s0, v1, v2) (c0', c1', c2')

            nf = P.fromIntegral (fullRounds params) :: Int
            np = P.fromIntegral (partialRounds params) :: Int
            -- Width is fixed at 3 since fromCircuit3F requires exactly 3 inputs;
            -- callers must ensure their PoseidonParams also has width=3.
            w = 3 :: Int
            firstFullIdxs = [0, w .. (nf P.- 1) P.* w]
            partialIdxs = [nf P.* w, nf P.* w P.+ w .. (nf P.+ np P.- 1) P.* w]
            lastFullStart = (nf P.+ np) P.* w
            lastFullIdxs = [lastFullStart, lastFullStart P.+ w .. lastFullStart P.+ (nf P.- 1) P.* w]

        s <- foldM fullRound (i0, i1, i2) firstFullIdxs
        p <- foldM partialRound s partialIdxs
        (f0, f1, f2) <- foldM fullRound p lastFullIdxs

        pure ((Par1 f0 :*: Par1 f1) :*: Par1 f2)
   in ( FieldElement $ hmap (\((Par1 a :*: _) :*: _) -> Par1 a) result
      , FieldElement $ hmap (\((_ :*: Par1 b) :*: _) -> Par1 b) result
      , FieldElement $ hmap (\((_ :*: _) :*: Par1 c) -> Par1 c) result
      )

-- | Poseidon hash for two field element inputs, parameterized by 'PoseidonParams'.
-- Applies the Poseidon permutation to state (x, y, 0) and returns the first output element.
poseidonHash2
  :: forall c
   . Symbolic c
  => PoseidonParams (BaseField c)
  -> FieldElement c
  -> FieldElement c
  -> FieldElement c
poseidonHash2 params x y = let (o, _, _) = poseidonPermute3 params x y zero in o

-- | Poseidon hash for two field element inputs using default BLS12-381 parameters.
-- Uses 630 vanilla Plonk constraints.
poseidonHash2Default
  :: forall c
   . Symbolic c
  => FieldElement c
  -> FieldElement c
  -> FieldElement c
poseidonHash2Default = poseidonHash2 defaultPoseidonParams

-- | Symbolic Poseidon sponge hash, parameterized by 'PoseidonParams'.
-- Pads the input to a multiple of the rate (always adding at least one element per Poseidon spec),
-- then absorbs each block into the state and applies the permutation.
-- The result is the first element of the final state.
--
-- The params type uses 'FieldElement c' so that the standard 'Field' instance of
-- 'FieldElement c' can drive the non-symbolic 'poseidonHash', letting arbitrary
-- params work in a symbolic context.  For the optimized BLS12-381 default, use
-- 'hashDefault'.
hash :: (SymbolicData x, Symbolic c) => PoseidonParams (FieldElement c) -> x c -> FieldElement c
hash params x =
  let elems = fmap FieldElement (unpacked (hmap toList (arithmetize x)))
   in poseidonHash params elems

-- | Symbolic Poseidon sponge hash with default BLS12-381 parameters (rate=2, capacity=1).
-- Uses the optimized 'poseidonPermute3' for lower constraint counts.
hashDefault :: (SymbolicData x, Symbolic c) => x c -> FieldElement c
hashDefault x =
  let elems = fmap FieldElement (unpacked (hmap toList (arithmetize x)))
      padded = padInput 2 elems
      (result, _, _) = P.foldl absorbBlock (zero, zero, zero) (blocks 2 padded)
   in result
 where
  padInput r inp =
    let n = P.length inp
        paddingLen = r P.- (n `P.mod` r)
     in inp P.++ P.replicate paddingLen zero
  blocks _ [] = []
  blocks n xs = let (b, rest) = P.splitAt n xs in b : blocks n rest
  absorbBlock (s0, s1, s2) [b0, b1] = poseidonPermute3 defaultPoseidonParams (s0 + b0) (s1 + b1) s2
  absorbBlock _ _ = P.error "absorbBlock: unexpected block size"
