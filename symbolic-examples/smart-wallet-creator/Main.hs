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
import qualified ZkFold.Symbolic.Compiler.ArithmeticCircuit as AC
import ZkFold.Symbolic.Examples.SmartWallet
import Prelude hiding (Fractional (..), Num (..), length)

main :: IO ()
main = do
  print $ AC.acSizeN expModCircuit
  print $ AC.acSizeL expModCircuit
  let proofBytes =
        mkProof $
          expModProof @ByteString one (PlonkupProverSecret $ pure (one + one)) expModCircuit (ExpModProofInput 17 3 7 11)
  print proofBytes
