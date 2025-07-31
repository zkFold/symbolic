module Main where

import System.Random (RandomGen, initStdGen)
import Test.Hspec (Spec, describe, hspec)
import Prelude

import Tests.Algebra.EllipticCurve (specEllipticCurve)
import Tests.Algebra.Field (specField)
import Tests.Algebra.Groebner (specGroebner)
import Tests.Algebra.Group (specGroup)
import Tests.Algebra.Pairing (specPairing)
import Tests.Algebra.Permutation (specPermutation)
import Tests.Algebra.ReedSolomon (specReedSolomon)
import Tests.Algebra.Univariate (specUnivariate)
import Tests.Data.Binary (specBinary)
import Tests.FFI.Rust.Plonkup (specRustPlonkup)
import Tests.Protocol.IVC
import Tests.Protocol.NonInteractiveProof (specNonInteractiveProof)
import Tests.Protocol.Plonkup (specPlonkup)
import Tests.Symbolic.Algorithm.Blake2b (specBlake2b)
import Tests.Symbolic.Algorithm.JWT (specJWT)
import Tests.Symbolic.Algorithm.Keccak (specKeccak)
import Tests.Symbolic.Algorithm.Poseidon (specPoseidon)
import Tests.Symbolic.Algorithm.RSA (specRSA)
import Tests.Symbolic.Algorithm.SHA2 (specSHA2, specSHA2Natural)
import Tests.Symbolic.ArithmeticCircuit (specArithmeticCircuit)
import Tests.Symbolic.Compiler (specCompiler)
import Tests.Symbolic.Data.ByteString (specByteString)
import Tests.Symbolic.Data.FFA (specFFA)
import Tests.Symbolic.Data.Hash (specHash)
import Tests.Symbolic.Data.Int (specInt)
import Tests.Symbolic.Data.List (specList)
import Tests.Symbolic.Data.MerkleTree (specMerkleTree)
import Tests.Symbolic.Data.Sum (specSum)
import Tests.Symbolic.Data.UInt (specUInt)

spec :: RandomGen g => g -> Spec
spec gen = do
  describe "symbolic-base-test (Algebra)" $ do
    specGroup
    specField
    specEllipticCurve
    specPairing
    specPermutation
    specUnivariate
    specReedSolomon
    specGroebner

  describe "symbolic-base-test (Serialization)" $ do
    specBinary

  describe "symbolic-base-test (Protocols)" $ do
    specPlonkup
    specNonInteractiveProof
    specIVC

  describe "symbolic-base-test (Symbolic compiler)" $ do
    specArithmeticCircuit
    specCompiler

  describe "symbolic-base-test (Symbolic data)" $ do
    specUInt
    specInt
    specFFA
    specByteString
    specHash
    specList
    specMerkleTree
    specSum

  describe "symbolic-base-test (Symbolic cryptography)" $ do
    specBlake2b
    specJWT
    specPoseidon
    specRSA gen
    specSHA2Natural
    specSHA2
    specKeccak

  describe "symbolic-base-test (Rust FFI)" $ do
    specRustPlonkup

main :: IO ()
main = hspec . spec =<< initStdGen
