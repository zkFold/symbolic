{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.ByteString (ByteString)
import GHC.Generics
import GHC.Generics (Par1 (..), (:*:) (..))
import System.Process
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.Field
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.Protocol.Halo2.Export
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Protocol.Plonkup.Prover.Secret
import Prelude hiding (Fractional (..), Num (..), length)

import ZkFold.Symbolic.Examples.SmartWallet

main :: IO ()
main = do
  runProof

{--
  (exitcode, stdout, stderr) <- readProcessWithExitCode (exePath <> exeName) ["prove", circuitFile] ""
  print exitcode
  putStrLn "-----------------"
  putStrLn stdout
  putStrLn "-----------------"
  putStrLn stderr
  ts <- powersOfTauSubset
  let setupBytes = expModSetupMock @ByteString ts
      (input, proofBytes) = expModProofMock @ByteString ts (PlonkupProverSecret $ pure (one + one)) (ExpModProofInput 17 3 7 11)
  print input
  print $ verify @(PlonkupTs (Par1 :*: Par1) ExpModCircuitGatesMock ByteString) setupBytes input proofBytes
  print proofBytes
--}

exePath :: FilePath
exePath = "/home/vladimir/Desktop/zkFold/zkfold-base/symbolic-halo2-bridge/target/release/"

exeName :: FilePath
exeName = "symbolic-halo2-bridge"

circuitFile :: FilePath
circuitFile = "circuit.cbor"

runProof :: IO ()
runProof = writeHalo2IrFile circuitFile ir
 where
  {--
    input :: Fr
    input = toZp (-42)

    witnessInputs :: (Par1 :*: Par1) Fr
    witnessInputs = Par1 input :*: Par1 input

    Right ir = exportHalo2Ir @_ @_ @ExpModCircuitGatesMock @_ @(PolyVec Fr) debugCircuit witnessInputs

  --}

  -- {--
  ac = expModCircuit 65537 239456238475263984572639847
  input = circuitInput 128752 129567120934
  Right ir = exportHalo2Ir @_ @_ @ExpModCircuitGates @_ @(PolyVec Fr) ac input

-- }
