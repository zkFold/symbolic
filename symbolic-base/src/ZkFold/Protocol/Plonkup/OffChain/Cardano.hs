{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- | Types specifically for off-chain Cardano blockchain usage of the plonkup protocol.
module ZkFold.Protocol.Plonkup.OffChain.Cardano (
  ZKSetupBytes (..),
  ZKF (..),
  ByteStringFromHex (..),
  byteStringFromHexToHex,
  ZKProofBytes (..),
)
where
import Data.Aeson (ToJSON (..), FromJSON (..), withText, Value (..))
import Data.Coerce (coerce)
import Prelude (Integer, Show (..), ($), (.), MonadFail (..), Applicative (..), either)
import qualified Prelude as P
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


data ZKSetupBytes = ZKSetupBytes
  { n :: Integer
  , nPrv :: Integer
  , pow :: Integer
  , omega_int :: Integer
  , omegaNPrv_int :: Integer
  , k1_int :: Integer
  , k2_int :: Integer
  , h1_bytes :: ByteString
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
  , cmT2_bytes :: ByteString
  , cmT3_bytes :: ByteString
  }
  deriving stock (Generic, Show)

-- | Field element.
newtype ZKF = ZKF Integer
  deriving stock (Generic, P.Eq, P.Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | 'ByteString' whose on wire representation is given in hexadecimal encoding.
newtype ByteStringFromHex = ByteStringFromHex ByteString
  deriving stock Generic
  deriving newtype (P.Eq, P.Ord)

byteStringFromHexToHex :: ByteStringFromHex -> Text
byteStringFromHexToHex = decodeUtf8 . BS16.encode . coerce

instance Show ByteStringFromHex where
  showsPrec d bs =
    P.showParen (d P.> 10) $
      P.showString "ByteStringFromHex "
        . P.showsPrec 11 (byteStringFromHexToHex bs)

instance FromJSON ByteStringFromHex where
  parseJSON = withText "ByteStringFromHex" $ \t ->
    either (fail . show) (pure . ByteStringFromHex) $ BS16.decode (encodeUtf8 t)

instance ToJSON ByteStringFromHex where
  toJSON = String . byteStringFromHexToHex

-- | ZK proof bytes, assuming hex encoding for relevant bytes.
data ZKProofBytes = ZKProofBytes
  { cmA_bytes :: !ByteStringFromHex
  , cmB_bytes :: !ByteStringFromHex
  , cmC_bytes :: !ByteStringFromHex
  , cmF_bytes :: !ByteStringFromHex
  , cmH1_bytes :: !ByteStringFromHex
  , cmH2_bytes :: !ByteStringFromHex
  , cmZ1_bytes :: !ByteStringFromHex
  , cmZ2_bytes :: !ByteStringFromHex
  , cmQlow_bytes :: !ByteStringFromHex
  , cmQmid_bytes :: !ByteStringFromHex
  , cmQhigh_bytes :: !ByteStringFromHex
  , proof1_bytes :: !ByteStringFromHex
  , proof2_bytes :: !ByteStringFromHex
  , a_xi_int :: !Integer
  , b_xi_int :: !Integer
  , c_xi_int :: !Integer
  , s1_xi_int :: !Integer
  , s2_xi_int :: !Integer
  , f_xi_int :: !Integer
  , t_xi_int :: !Integer
  , t_xi'_int :: !Integer
  , z1_xi'_int :: !Integer
  , z2_xi'_int :: !Integer
  , h1_xi'_int :: !Integer
  , h2_xi_int :: !Integer
  , l_xi :: ![ZKF]
  , l1_xi :: !ZKF
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)