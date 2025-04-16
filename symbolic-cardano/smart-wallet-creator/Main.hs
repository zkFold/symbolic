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

import           Data.ByteString                                   (ByteString)
import           GHC.Generics                                      (Generic)
import           GHC.Natural                                       (naturalToInteger)
import           Prelude                                           hiding (Bool, Eq (..), Fractional (..), Num (..),
                                                                    length)

import           ZkFold.Base.Algebra.Basic.Class                   (zero)
import           ZkFold.Base.Algebra.Basic.Field                   (Zp, fromZp)
import qualified ZkFold.Base.Algebra.Basic.Number                  as Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class           (compress)
import           ZkFold.Base.Data.ByteString                       (toByteString)
import           ZkFold.Base.Protocol.NonInteractiveProof          as NP (NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup.Verifier.Commitments
import           ZkFold.Base.Protocol.Plonkup.Verifier.Setup
import           ZkFold.Prelude                                    (log2ceiling)
import           ZkFold.Symbolic.Cardano.Contracts.SmartWallet

convertZp :: Zp p -> Integer
convertZp = naturalToInteger . fromZp

convertG1 :: BLS12_381_G1_Point -> ByteString
convertG1 = toByteString . compress

convertG2 :: BLS12_381_G2_Point -> ByteString
convertG2 = toByteString . compress

data SetupBytes = SetupBytes {
    n          :: Integer
  , pow        :: Integer
  , omega_int  :: Integer
  , k1_int     :: Integer
  , k2_int     :: Integer
  , h1_bytes   :: ByteString
  , cmQm_bytes :: ByteString
  , cmQl_bytes :: ByteString
  , cmQr_bytes :: ByteString
  , cmQo_bytes :: ByteString
  , cmQc_bytes :: ByteString
  , cmQk_bytes :: ByteString
  , cmS1_bytes :: ByteString
  , cmS2_bytes :: ByteString
  , cmS3_bytes :: ByteString
  , cmT1_bytes :: ByteString
} deriving stock (Show, Generic)

mkSetup :: SetupVerify (PlonkupTs ByteString) -> SetupBytes
mkSetup PlonkupVerifierSetup {..} =
  let PlonkupCircuitCommitments {..} = commitments
  in SetupBytes
    { n          = fromIntegral (Number.value @NGates)
    , pow        = log2ceiling (Number.value @NGates)
    , omega_int  = convertZp omega
    , k1_int     = convertZp k1
    , k2_int     = convertZp k2
    , h1_bytes   = convertG2 h1
    , cmQm_bytes = convertG1 cmQm
    , cmQl_bytes = convertG1 cmQl
    , cmQr_bytes = convertG1 cmQr
    , cmQo_bytes = convertG1 cmQo
    , cmQc_bytes = convertG1 cmQc
    , cmQk_bytes = convertG1 cmQk
    , cmS1_bytes = convertG1 cmS1
    , cmS2_bytes = convertG1 cmS2
    , cmS3_bytes = convertG1 cmS3
    , cmT1_bytes = convertG1 cmT1
    }

main :: IO ()
main = do
    print bytes
    where
        bytes = mkSetup $ expModSetup @ByteString zero


