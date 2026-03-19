{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import GHC.TypeNats (type (+), type (^))
import System.Environment (getArgs)
import Prelude

import ZkFold.Protocol.NonInteractiveProof.TrustedSetup (TrustedSetup (..), readTrustedSetup, saveTrustedSetup)

-- We need 2^20 + 6 points.
type N = 2 ^ 20 + 6

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> extractSetup inputPath "data/midnight_powers_of_tau_2e20"
    [inputPath, outputPath] -> extractSetup inputPath outputPath
    _ -> putStrLn "Usage: setup-generator <input-midnight-file> [output-file]"

extractSetup :: FilePath -> FilePath -> IO ()
extractSetup inputPath outputPath = do
  putStrLn $ "Extracting 2^20 + 6 G1 points from: " ++ inputPath
  -- The original Midnight file has uncompressed G1 points (96 bytes each)
  result <- readTrustedSetup @N inputPath False
  case result of
    Nothing -> putStrLn "ERROR: Failed to read trusted setup (points not on curve?)"
    Just ts -> do
      putStrLn $ "Successfully read " ++ show (2 ^ (20 :: Int) + 6) ++ " G1 points and 2 G2 points"
      putStrLn $ "Saving compressed to: " ++ outputPath
      saveTrustedSetup outputPath True ts
      putStrLn "Done."
