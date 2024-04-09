{-# LANGUAGE TypeApplications #-}

module Main where

import Tests.ArithmeticCircuit (specArithmeticCircuit)
import Tests.Arithmetization (specArithmetization)
import Tests.ByteString (specByteString)
import Tests.Field (specField)
import Tests.GroebnerBasis (specGroebner)
import Tests.Group (specAdditiveGroup)
import Tests.Multiplication (specMultiplication)
import Tests.NonInteractiveProof (specNonInteractiveProof)
import Tests.Pairing (specPairing)
import Tests.Permutations (specPermutations)
import Tests.Plonk (PlonkBS, specPlonk)
import Tests.Scripts.LockedByTxId (specLockedByTxId)
import Tests.UInt (specUInt)
import Tests.Univariate (specUnivariate)
import ZkFold.Base.Algebra.Basic.Field (Zp)
import ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import ZkFold.Base.Algebra.EllipticCurve.Class
import ZkFold.Base.Protocol.Commitment.KZG (KZG)
import Prelude hiding (Bool, Fractional (..), Num (..), drop, length, replicate, take, (==))

main :: IO ()
main = do
  specArithmeticCircuit @(Zp BLS12_381_Scalar)
  specUInt @BLS12_381_Scalar @32
  specUInt @BLS12_381_Scalar @500

  specByteString @BLS12_381_Scalar @32
  specByteString @BLS12_381_Scalar @500
  specByteString @BLS12_381_Scalar @508 -- Twice the number of bits encoded by BLS12_381_Scalar.
  specLockedByTxId

  specArithmetization @(Zp BLS12_381_Scalar)
  specGroebner

  specPermutations
  specField @Fr
  specField @Fq
  specField @Fq2
  specField @Fq6
  specField @Fq12
  specAdditiveGroup @(Point BLS12_381_G1)
  specAdditiveGroup @(Point BLS12_381_G2)
  specPairing @BLS12_381_G1 @BLS12_381_G2
  specUnivariate
  specMultiplication

  specNonInteractiveProof @(KZG BLS12_381_G1 BLS12_381_G2 BLS12_381_GT (Zp BLS12_381_Scalar) 32)
  specPlonk
  specNonInteractiveProof @PlonkBS

  putStrLn "\nAll tests passed!"
