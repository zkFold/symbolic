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

import           ZkFold.Algebra.Class                  
import           ZkFold.Protocol.Plonkup.Prover.Secret
import           ZkFold.Symbolic.Examples.SmartWallet

main :: IO ()
main = do
    let setupBytes = mkSetup $ expModSetupMock @ByteString one 
        proofBytes = mkProof $ expModProofMock @ByteString one (PlonkupProverSecret $ pure (one + one)) (ExpModProofInput 1 1 1 1)
    print setupBytes
    print proofBytes
