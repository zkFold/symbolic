module Main where

import GHC.Natural
import System.Random (RandomGen, initStdGen)
import Test.Hspec (Spec, describe, hspec)
-- import Tests.Algebra.EllipticCurve (specEllipticCurve)
-- import Tests.Algebra.Field (specField)
-- import Tests.Algebra.GroebnerBasis (specGroebner)
-- import Tests.Algebra.Group (specGroup)
-- import Tests.Algebra.Pairing (specPairing)
-- import Tests.Algebra.Permutation (specPermutation)
-- import Tests.Algebra.ReedSolomon (ReedSolomonExample (r), specReedSolomon)
-- import Tests.Algebra.Univariate (specUnivariate)
-- import Tests.Data.Binary (specBinary)
import Tests.FFI.RustBLS
-- import Tests.Protocol.IVC (specIVC)
-- import Tests.Protocol.NonInteractiveProof (specNonInteractiveProof)
-- import Tests.Protocol.Plonkup (specPlonkup)
-- import Tests.Symbolic.Algorithm.Blake2b (specBlake2b)
-- import Tests.Symbolic.Algorithm.JWT (specJWT)
-- import Tests.Symbolic.Algorithm.Keccak (specKeccak)
-- import Tests.Symbolic.Algorithm.RSA (specRSA)
-- import Tests.Symbolic.Algorithm.SHA2 (specSHA2, specSHA2Natural)
-- import Tests.Symbolic.ArithmeticCircuit (specArithmeticCircuit)
-- import Tests.Symbolic.Compiler (specCompiler)
-- import Tests.Symbolic.Data.ByteString (specByteString)
-- import Tests.Symbolic.Data.FFA (specFFA)
-- import Tests.Symbolic.Data.Hash (specHash)
-- import Tests.Symbolic.Data.Int (specInt)
-- import Tests.Symbolic.Data.List (specList)
-- import Tests.Symbolic.Data.MerkleTree (specMerkleTree)
-- import Tests.Symbolic.Data.UInt (specUInt)

import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.FFI.Rust.Conversion (RustHaskell (..))
import Prelude hiding (
  Bool,
  Fractional (..),
  Num (..),
  drop,
  length,
  replicate,
  take,
  (==),
 )

spec :: RandomGen g => g -> Spec
spec gen = do
  specRustBLS

-- describe "symbolic-base-test (Algebra)" $ do
--     specGroup
--     specField
--     specEllipticCurve
--     specPairing
--     specPermutation
--     specUnivariate
--     specReedSolomon
--     specGroebner

-- describe "symbolic-base-test (Serialization)" $ do
--     specBinary

-- describe "symbolic-base-test (Protocols)" $ do
--     specPlonkup
--     specNonInteractiveProof
--     specIVC

-- describe "symbolic-base-test (Symbolic compiler)" $ do
--     specArithmeticCircuit
--     specCompiler

-- describe "symbolic-base-test (Symbolic data)" $ do
--     specUInt
--     specInt
--     specFFA
--     specByteString
--     specHash
--     specList
--     specMerkleTree

-- describe "symbolic-base-test (Symbolic cryptography)" $ do
--     specBlake2b
--     specJWT
--     specRSA gen
--     specSHA2Natural
--     specSHA2
--     specKeccak

main :: IO ()
main = do
  -- let g2Generator = pointGen :: BLS12_381_G2_Point
  -- let rustGenerator = h2r g2Generator
  -- let hGenerator = r2h rustGenerator
  -- print hGenerator
  -- return ()

  hspec . spec =<< initStdGen

--   let h = 239000 :: Natural
--   let r = h2r h
--   let hh = r2h r
--   print h
--   --   print r
--   print hh
