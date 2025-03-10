{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT.RS256 (SigningKey (..), Certificate (..)) where

import           Control.DeepSeq                    (NFData, force)
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import qualified Data.Aeson                         as JSON
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Constraint                    (Dict (..), withDict, (:-) (..))
import           Data.Constraint.Nat                (Max, divNat, minusNat, plusNat, timesNat)
import           Data.Constraint.Unsafe             (unsafeAxiom, unsafeSNat)
import           Data.Maybe                         (fromMaybe)
import           Data.Scientific                    (toBoundedInteger)
import qualified Data.Text                          as T
import           Generic.Random                     (genericArbitrary, uniform)
import           GHC.Generics                       (Generic, Par1 (..))
import           GHC.TypeLits                       (withKnownNat)
import           Prelude                            (fmap, pure, type (~), ($), (.), (<$>))
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor          (hmap)
import qualified ZkFold.Base.Data.Vector            as V
import           ZkFold.Base.Data.Vector            ((!!))
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString    (ByteString (..), concat, toWords)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.JWT
import           ZkFold.Symbolic.Data.UInt
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), wipeUnassigned, (@+))
import           ZkFold.Symbolic.MonadCircuit       (newAssigned)

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

instance SigningAlgorithm "RS256" ctx where
    type SKey "RS256" ctx = SigningKey ctx
    type VKey "RS256" ctx = Certificate ctx
    type Signature "RS256" ctx = ByteString 2048 ctx
