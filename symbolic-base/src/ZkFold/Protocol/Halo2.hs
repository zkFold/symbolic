{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Halo2 Integration Module
--
-- This module provides data types that mirror the Rust PlonkupCircuit structure
-- defined in rust-wrapper/src/halo2.rs. It enables JSON serialization for
-- cross-language communication between Haskell zkFold circuits and Halo2 proofs.
--
-- The module leverages the existing ZkFold.Protocol.Plonkup.Relation infrastructure
-- to properly generate PlonkUp witnesses and selectors from ArithmeticCircuits.
module ZkFold.Protocol.Halo2 (
  PlonkupCircuit (..),
  PlonkupWitnessData (..),
  PlonkupSelectors (..),
  PlonkupTableData (..),
  generatePlonkupCircuit,
) where

import Data.Aeson (ToJSON)
import Data.Binary (Binary)
import Data.Foldable (Foldable)
import Data.Functor.Rep (Rep, Representable)
import Data.Maybe (Maybe)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, natVal, type (*))
import Numeric.Natural (Natural)
import ZkFold.Algebra.Permutation (Permutation, fromPermutation)
import ZkFold.Algebra.Polynomial.Univariate (UnivariateRingPolyVec, fromPolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit)
import ZkFold.Protocol.Plonkup.Relation (PlonkupRelation (..), toPlonkupRelation)
import qualified ZkFold.Protocol.Plonkup.Relation as Plonkup
import ZkFold.Symbolic.Class (Arithmetic)
import Prelude (return, ($), (-), filter, map, div, mod, head, tail, 
                length, notElem, (>), fromIntegral, (==), elem, concat, 
                zip, (++), (*))

-- | Witness data for the three PlonkUp witness columns
data PlonkupWitnessData a = PlonkupWitnessData
  { w1 :: [a]
  , w2 :: [a]
  , w3 :: [a]
  }
  deriving (Generic, ToJSON)

-- | Selector polynomials for PlonkUp constraints
data PlonkupSelectors a = PlonkupSelectors
  { qMul :: [a]
  , qLeft :: [a]
  , qRight :: [a]
  , qOutput :: [a]
  , qConst :: [a]
  , qLookup :: [a]
  }
  deriving (Generic, ToJSON)

-- | Lookup table data for PlonkUp
data PlonkupTableData a = PlonkupTableData
  { table1 :: [a]
  , table2 :: [a]
  , table3 :: [a]
  }
  deriving (Generic, ToJSON)

-- | Complete PlonkUp circuit data structure mirroring the Rust implementation
data PlonkupCircuit a = PlonkupCircuit
  { circuitSize :: Natural
  , witness :: PlonkupWitnessData a
  , selectors :: PlonkupSelectors a
  , table :: PlonkupTableData a
  , copyConstraints :: [(Natural, Natural, Natural, Natural)]
  }
  deriving (Generic, ToJSON)

-- | Extract copy constraints from a PlonkUp permutation
-- The permutation represents connections between the three witness columns
extractCopyConstraints :: Natural -> Permutation (3 * n) -> [(Natural, Natural, Natural, Natural)]
extractCopyConstraints n sigma = 
  let 
    -- Convert permutation to vector (1-indexed)
    permVec = fromPermutation sigma
    
    -- Convert from 1-indexed permutation to 0-indexed with column information
    -- Index i maps to (column, row) where:
    -- - column = (i-1) `div` n  (0 = w1, 1 = w2, 2 = w3)
    -- - row = (i-1) `mod` n
    indexToPosition i = 
      let idx = i - 1
      in (idx `div` n, idx `mod` n)
    
    -- Find all cycles in the permutation that have length > 1
    findCycles :: [Natural] -> [[Natural]]
    findCycles [] = []
    findCycles (i:is) = 
      let cycle = findCycle i [i]
          remaining = filter (`notElem` cycle) is
      in if length cycle > 1 
         then cycle : findCycles remaining
         else findCycles remaining
      where
        findCycle start visited =
          let next = permVec V.! (fromIntegral start - 1)
          in if next == start
             then visited
             else if next `elem` visited
                  then visited  -- Avoid infinite loops
                  else findCycle next (next : visited)
    
    -- Convert cycles to copy constraints
    cyclesToConstraints cycleList = concat $ map cycleToConstraints cycleList
      where
        cycleToConstraints cycle = 
          let positions = map indexToPosition cycle
              pairs = zip positions (tail positions ++ [head positions])
          in map (\((col1, row1), (col2, row2)) -> (col1, row1, col2, row2)) pairs
    
    allIndices = [1 .. 3 * n]
    foundCycles = findCycles allIndices
    
  in cyclesToConstraints foundCycles

-- | Generate a complete PlonkupCircuit from an ArithmeticCircuit
-- This function leverages the existing PlonkupRelation infrastructure
generatePlonkupCircuit
  :: forall i o n a pv
   . ( KnownNat n
     , Arithmetic a
     , Binary (Rep i)
     , UnivariateRingPolyVec a pv
     , Representable i
     , Representable o
     , Foldable o
     )
  => ArithmeticCircuit a i o
  -- ^ The arithmetic circuit
  -> i a
  -- ^ The circuit inputs
  -> Maybe (PlonkupCircuit a)
generatePlonkupCircuit circuit inputs = do
  -- Convert ArithmeticCircuit to PlonkupRelation using existing infrastructure
  relation <- toPlonkupRelation @i @o @n @a @pv circuit

  -- Extract witness values using the PlonkupRelation witness function
  let (w1Vec, w2Vec, w3Vec) = Plonkup.witness relation inputs
      n = natVal (Proxy @n)

  -- Convert to witness data
  let witnessData =
        PlonkupWitnessData
          { w1 = V.toList (fromPolyVec w1Vec)
          , w2 = V.toList (fromPolyVec w2Vec)
          , w3 = V.toList (fromPolyVec w3Vec)
          }

  -- Extract selector polynomials from PlonkupRelation
  let selectorsData =
        PlonkupSelectors
          { qMul = V.toList (fromPolyVec (qM relation))
          , qLeft = V.toList (fromPolyVec (qL relation))
          , qRight = V.toList (fromPolyVec (qR relation))
          , qOutput = V.toList (fromPolyVec (qO relation))
          , qConst = V.toList (fromPolyVec (qC relation))
          , qLookup = V.toList (fromPolyVec (qK relation))
          }

  -- Extract lookup table data
  let tableData =
        PlonkupTableData
          { table1 = V.toList (fromPolyVec (t1 relation))
          , table2 = V.toList (fromPolyVec (t2 relation))
          , table3 = V.toList (fromPolyVec (t3 relation))
          }

  -- Extract copy constraints from the permutation sigma
  let copyConstraintsData = extractCopyConstraints n (sigma relation)

  return $
    PlonkupCircuit
      { circuitSize = n
      , witness = witnessData
      , selectors = selectorsData
      , table = tableData
      , copyConstraints = copyConstraintsData
      }
