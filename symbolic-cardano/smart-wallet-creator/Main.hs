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
import           Prelude                                           hiding (Fractional (..), Num (..), length)

import           ZkFold.Base.Algebra.Basic.Class                   (zero, (+))
import           ZkFold.Base.Algebra.Basic.Field                   (Zp, fromZp)
import qualified ZkFold.Base.Algebra.Basic.Number                  as Number
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class           (compress)
import           ZkFold.Base.Data.ByteString                       (toByteString)
import           ZkFold.Base.Protocol.NonInteractiveProof          as NP (NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup.Proof
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
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

data ProofBytes = ProofBytes {
    cmA_bytes     :: ByteString
  , cmB_bytes     :: ByteString
  , cmC_bytes     :: ByteString
  , cmF_bytes     :: ByteString
  , cmH1_bytes    :: ByteString
  , cmH2_bytes    :: ByteString
  , cmZ1_bytes    :: ByteString
  , cmZ2_bytes    :: ByteString
  , cmQlow_bytes  :: ByteString
  , cmQmid_bytes  :: ByteString
  , cmQhigh_bytes :: ByteString
  , proof1_bytes  :: ByteString
  , proof2_bytes  :: ByteString
  , a_xi_int      :: Integer
  , b_xi_int      :: Integer
  , c_xi_int      :: Integer
  , s1_xi_int     :: Integer
  , s2_xi_int     :: Integer
  , f_xi_int      :: Integer
  , t_xi_int      :: Integer
  , t_xi'_int     :: Integer
  , z1_xi'_int    :: Integer
  , z2_xi'_int    :: Integer
  , h1_xi'_int    :: Integer
  , h2_xi_int     :: Integer
  , l1_xi         :: Integer
} deriving stock (Show, Generic)

mkProof :: Proof (PlonkupTs ByteString) -> ProofBytes
mkProof PlonkupProof {..} = ProofBytes
  { cmA_bytes     = convertG1 cmA
  , cmB_bytes     = convertG1 cmB
  , cmC_bytes     = convertG1 cmC
  , cmF_bytes     = convertG1 cmF
  , cmH1_bytes    = convertG1 cmH1
  , cmH2_bytes    = convertG1 cmH2
  , cmZ1_bytes    = convertG1 cmZ1
  , cmZ2_bytes    = convertG1 cmZ2
  , cmQlow_bytes  = convertG1 cmQlow
  , cmQmid_bytes  = convertG1 cmQmid
  , cmQhigh_bytes = convertG1 cmQhigh
  , proof1_bytes  = convertG1 proof1
  , proof2_bytes  = convertG1 proof2
  , a_xi_int      = convertZp a_xi
  , b_xi_int      = convertZp b_xi
  , c_xi_int      = convertZp c_xi
  , s1_xi_int     = convertZp s1_xi
  , s2_xi_int     = convertZp s2_xi
  , f_xi_int      = convertZp f_xi
  , t_xi_int      = convertZp t_xi
  , t_xi'_int     = convertZp t_xi'
  , z1_xi'_int    = convertZp z1_xi'
  , z2_xi'_int    = convertZp z2_xi'
  , h1_xi'_int    = convertZp h1_xi'
  , h2_xi_int     = convertZp h2_xi
  , l1_xi         = convertZp $ head l_xi
  }

genProof :: ExpModProofInput -> ProofBytes
genProof = undefined -- mkProof . expModProof @ByteString zero (PlonkupProverSecret $ pure zero)

foo :: ExpModProofInput -> Bool
foo (ExpModProofInput a b c d) = a + b + c + d > 0

main :: IO ()
main = do
    putStrLn "Hello"
    print $ foo (ExpModProofInput 1 1 1 1)
    where
        --bytes = mkSetup $ expModSetup @ByteString zero
        bytes = genProof (ExpModProofInput 1 1 1 1)


