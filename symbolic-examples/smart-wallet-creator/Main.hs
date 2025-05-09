{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Data.ByteString                       (ByteString)
import           Prelude                               hiding (Fractional (..), Num (..), length)

import           ZkFold.Algebra.Class                  (zero)
import           ZkFold.Protocol.Plonkup.Prover.Secret
import           ZkFold.Symbolic.Examples.SmartWallet

main :: IO ()
main = do
    let setupBytes = mkSetup $ expModSetupMock @ByteString zero
        proofBytes = mkProof $ expModProofMock @ByteString zero (PlonkupProverSecret $ pure zero) (ExpModProofInput 1 1 1 1)
    print setupBytes
    print proofBytes
