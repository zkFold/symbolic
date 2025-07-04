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
import ZkFold.Algebra.Class
import ZkFold.Protocol.Plonkup.Prover.Secret
import Prelude hiding (Fractional (..), Num (..), length)

import ZkFold.Symbolic.Examples.SmartWallet

main :: IO ()
main = do
  let proofBytes = mkProof $ expModProofDebug @ByteString one (PlonkupProverSecret $ pure (one + one)) (ExpModProofInput 17 3 7 11)
  print proofBytes
