{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Data.JWT.RS256 (SigningKey (..), Certificate (..)) where

import Data.Aeson
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import GHC.Generics (Generic, Generic1)
import Prelude (pure, ($))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (Natural)
import ZkFold.Symbolic.Algorithm.RSA
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.JWT
import ZkFold.Symbolic.Data.UInt (KnownUInt)
import ZkFold.Symbolic.Data.VarByteString (VarByteString)

-- | RSA Public key with Key ID
data Certificate ctx
  = Certificate
  { pubKid :: VarByteString 320 ctx
  , pubKey :: PublicKey 2048 ctx
  }
  deriving (Generic, Generic1, SymbolicData)

instance
  (Symbolic ctx, KnownUInt 2048 ctx, KnownUInt PubExponentSize ctx)
  => FromJSON (Certificate ctx)
  where
  parseJSON = withObject "Certificate" $ \v -> do
    kid <- v .: "kid"
    n <- v .: "n"
    e <- v .: "e"
    let nNat = bsToNat (B64.decodeLenient n)
        eNat = bsToNat (B64.decodeLenient e)
        nUInt = fromConstant nNat
        eUInt = fromConstant eNat
        kidBs = fromConstant @BS.ByteString kid

        pubKey = PublicKey eUInt nUInt

    pure $ Certificate kidBs pubKey

bsToNat :: BS.ByteString -> Natural
bsToNat = BS.foldl' (\i b -> (i `B.shiftL` 8) + P.fromIntegral b) 0

-- | RSA Private key with Key ID
data SigningKey ctx
  = SigningKey
  { prvKid :: VarByteString 320 ctx
  , prvKey :: PrivateKey 2048 ctx
  }
  deriving (Generic, Generic1, SymbolicData)

instance SigningAlgorithm "RS256" where
  type SKey "RS256" ctx = SigningKey ctx
  type VKey "RS256" ctx = Certificate ctx
  type Signature "RS256" ctx = ByteString 2048 ctx
  type Hash "RS256" ctx = ByteString 256 ctx
