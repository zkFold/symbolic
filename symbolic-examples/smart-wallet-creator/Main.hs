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
import GHC.Generics (Par1 (..), (:*:) (..))
import ZkFold.Algebra.Class
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Protocol.Plonkup.Prover.Secret
import Prelude hiding (Fractional (..), Num (..), length)

import ZkFold.Symbolic.Examples.SmartWallet

main :: IO ()
main = do
  ts <- powersOfTauSubset
  let setupBytes = expModSetup @ByteString ts
      proofBytes = expModProof @ByteString ts (PlonkupProverSecret $ pure (one + one)) expModCircuit (ExpModProofInput 17 3 7 11)
--  print $ verify @(PlonkupTs (Par1 :*: Par1) ExpModCircuitGates ByteString) setupBytes input proofBytes
  print proofBytes
