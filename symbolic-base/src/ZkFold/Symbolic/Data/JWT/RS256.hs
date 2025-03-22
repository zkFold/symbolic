{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZkFold.Symbolic.Data.JWT.RS256 (SigningKey (..), Certificate (..)) where

import           Control.DeepSeq                    (NFData)
import           GHC.Generics                       (Generic)
import qualified Prelude                            as P

import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.ByteString    (ByteString)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.VarByteString (VarByteString)

-- | RSA Public key with Key ID
--
data Certificate ctx
    = Certificate
        { pubKid :: VarByteString 320 ctx
        , pubKey :: PublicKey 2048 ctx
        }
    deriving Generic

deriving instance
    ( P.Eq (PublicKey 2048 ctx)
    , P.Eq (VarByteString 320 ctx)
    ) => P.Eq (Certificate ctx)
deriving instance
    ( P.Show (PublicKey 2048 ctx)
    , P.Show (VarByteString 320 ctx)
    ) => P.Show (Certificate ctx)
deriving instance
    ( NFData (PublicKey 2048 ctx)
    , NFData (VarByteString 320 ctx)
    ) => NFData (Certificate ctx)
instance
    ( SymbolicData (PublicKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicData (Certificate ctx)
instance
    ( SymbolicInput (PublicKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicInput (Certificate ctx)


-- | RSA Private key with Key ID
--
data SigningKey ctx
    = SigningKey
        { prvKid :: VarByteString 320 ctx
        , prvKey :: PrivateKey 2048 ctx
        }
    deriving Generic

deriving instance
    ( P.Eq (PrivateKey 2048 ctx)
    , P.Eq (VarByteString 320 ctx)
    ) => P.Eq (SigningKey ctx)
deriving instance
    ( P.Show (PrivateKey 2048 ctx)
    , P.Show (VarByteString 320 ctx)
    ) => P.Show (SigningKey ctx)
deriving instance
    ( NFData (PrivateKey 2048 ctx)
    , NFData (VarByteString 320 ctx)
    ) => NFData (SigningKey ctx)
instance
    ( SymbolicData (PrivateKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicData (SigningKey ctx)
instance
    ( SymbolicInput (PrivateKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicInput (SigningKey ctx)

instance SigningAlgorithm "RS256" where
    type SKey "RS256" ctx = SigningKey ctx
    type VKey "RS256" ctx = Certificate ctx
    type Signature "RS256" ctx = ByteString 2048 ctx
    type Hash "RS256" ctx = ByteString 256 ctx
