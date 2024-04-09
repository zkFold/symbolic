{-# LANGUAGE TypeApplications #-}

module Main where

import Examples.ByteString (exampleByteStringAnd, exampleByteStringOr)
import Examples.Conditional (exampleConditional)
import Examples.Eq (exampleEq)
import Examples.Fibonacci (exampleFibonacci)
import Examples.LEQ (exampleLEQ)
import Examples.MiMCHash (exampleMiMC)
import Examples.ReverseList (exampleReverseList)
import Examples.UInt (exampleUIntAdd, exampleUIntMul, exampleUIntStrictAdd, exampleUIntStrictMul)
import System.Directory (createDirectoryIfMissing)
import Prelude

main :: IO ()
main = do
  createDirectoryIfMissing True "compiled_scripts"

  exampleReverseList
  exampleEq
  exampleFibonacci
  exampleMiMC
  exampleLEQ
  exampleConditional
  exampleUIntAdd @32
  exampleUIntAdd @500
  exampleUIntMul @32
  exampleUIntMul @500
  exampleUIntStrictAdd @32
  exampleUIntStrictAdd @500
  exampleUIntStrictMul @32
  exampleUIntStrictMul @500
  exampleByteStringAnd @32
  exampleByteStringAnd @500
  exampleByteStringOr @32
  exampleByteStringOr @500
