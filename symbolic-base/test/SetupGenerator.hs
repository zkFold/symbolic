{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import GHC.TypeNats (SomeNat (..), someNatVal)
import Data.Proxy (Proxy (..))
import System.Environment (getArgs)
import Prelude

import ZkFold.Algebra.Number (value)
import ZkFold.Protocol.NonInteractiveProof.TrustedSetup (readTrustedSetup, saveTrustedSetup)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nStr, inputPath, outputPath] ->
      case reads nStr of
        [(n, "")] | n > 0 -> extractSetup n inputPath outputPath
        _ -> putStrLn "ERROR: first argument must be a positive integer (number of G1 points)"
    _ -> putStrLn "Usage: setup-generator <n-points> <input-midnight-file> <output-file>"

extractSetup :: Integer -> FilePath -> FilePath -> IO ()
extractSetup n inputPath outputPath =
  case someNatVal (fromIntegral n) of
    SomeNat (_ :: Proxy n) -> do
      putStrLn $ "Extracting " ++ show n ++ " G1 points from: " ++ inputPath
      -- The original Midnight file has uncompressed G1 points (96 bytes each)
      result <- readTrustedSetup @n inputPath False
      case result of
        Nothing -> putStrLn "ERROR: Failed to read trusted setup (points not on curve?)"
        Just ts -> do
          putStrLn $ "Successfully read " ++ show (value @n) ++ " G1 points and 2 G2 points"
          putStrLn $ "Saving compressed to: " ++ outputPath
          saveTrustedSetup outputPath True ts
          putStrLn "Done."
