{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}

module ZkFold.Base.Protocol.NonInteractiveProof.Prover where

import           Control.DeepSeq                          (NFData)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                          (ByteString)
import qualified Data.ByteString.Base64                   as B64
import qualified Data.ByteString.Char8                    as BS
import qualified Data.Text                                as T
import           GHC.Generics                             (Generic)
import           Optics                                   ((&))
import           Prelude
import           Test.QuickCheck                          (Arbitrary (..), generate, vectorOf)

import           ZkFold.Base.Data.ByteString
import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))

newtype ProofBytes = ProofBytes
  { fromWitnessBytes :: ByteString }
  deriving (Show, Eq, Generic, NFData)

instance ToJSON ProofBytes where
    toJSON (ProofBytes b) = String . T.pack . BS.unpack . B64.encode $ b

instance FromJSON ProofBytes where
    parseJSON = withText "Bytes of proof" $ \t ->
        case B64.decode . BS.pack . T.unpack $ t of
            Left err -> fail err
            Right bs -> return $ ProofBytes bs

data ProveAPIResult = ProveAPISuccess ProofBytes | ProveAPIErrorSetup | ProveAPIErrorWitness
    deriving (Show, Eq, Generic, NFData)

instance ToJSON ProveAPIResult where
  toJSON (ProveAPISuccess bs) = object
    [ "status" .= ("success" :: String)
    , "data" .= bs
    ]
  toJSON ProveAPIErrorSetup = object
    [ "status" .= ("error" :: String)
    , "message" .= ("Setup error" :: String)
    ]
  toJSON ProveAPIErrorWitness = object
    ["status" .= ("error" :: String)
    , "message" .= ("Witness error" :: String)
    ]

instance FromJSON ProveAPIResult where
  parseJSON = withObject "ProveAPIResult" $ \v ->
    v .: "status" & id @(Parser String) >>= \case
      "success" -> ProveAPISuccess <$> v .: "data"
      "error" -> v .: "message" & id @(Parser String) >>= \case
        "Setup error" -> return ProveAPIErrorSetup
        "Witness error" -> return ProveAPIErrorWitness
        _ -> fail "Unknown error message"
      _ -> fail "Unknown status"

proveAPI
    :: forall a core
    . (NonInteractiveProof a core
    , Binary (SetupProve a)
    , Binary (Witness a)
    , Binary (Input a)
    , Binary (Proof a))
    => ByteString
    -> ByteString
    -> ProveAPIResult
proveAPI bsS bsW =
    let mS = fromByteString bsS
        mW = fromByteString bsW
    in case (mS, mW) of
        (Nothing, _)     -> ProveAPIErrorSetup
        (_, Nothing)     -> ProveAPIErrorWitness
        (Just s, Just w) -> ProveAPISuccess . ProofBytes $ toByteString $ prove @a @core s w

testVector :: forall a core .
    NonInteractiveProof a core =>
    Arbitrary a =>
    Arbitrary (Witness a) =>
    Binary (SetupProve a) =>
    Binary (Input a) =>
    Binary (Proof a) =>
    Int -> IO [(ByteString, ByteString, ByteString)]
testVector n = generate . vectorOf n $ (,)
    <$> arbitrary @a
    <*> arbitrary @(Witness a)
    >>= \(a, w) -> do
        let s = setupProve @a @core a
        let (i, p) = prove @a @core s w
        pure (toByteString s, toByteString i, toByteString p)
