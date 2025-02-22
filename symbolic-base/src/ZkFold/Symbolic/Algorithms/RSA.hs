{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Algorithms.RSA
    ( sign
    , verify
    , signVar
    , verifyVar
    , RSA
    , PublicKey (..)
    , PrivateKey (..)
    , Signature
    ) where

import           Control.DeepSeq                      (NFData, force)
import           GHC.Generics                         (Generic)
import           Prelude                              (($))
import qualified Prelude                              as P

import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.Vector              (Vector)
import           ZkFold.Symbolic.Algorithms.Hash.SHA2 (SHA2, sha2, sha2Var)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool            (Bool, (&&))
import           ZkFold.Symbolic.Data.ByteString      (ByteString)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators     (Ceil, GetRegisterSize, Iso (..), KnownRegisters,
                                                       NumberOfRegisters, RegisterSize (..), Resize (..))
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.Input           (SymbolicInput, isValid)
import           ZkFold.Symbolic.Data.UInt            (OrdWord, UInt, expMod)
import           ZkFold.Symbolic.Data.VarByteString   (VarByteString)

type Signature keyLen ctx = ByteString keyLen ctx

data PrivateKey keyLen ctx
    = PrivateKey
        { prvD :: UInt keyLen 'Auto ctx
        , prvN :: UInt keyLen 'Auto ctx
        }

deriving instance Generic (PrivateKey keyLen context)
deriving instance (NFData (context (Vector (NumberOfRegisters (BaseField context) keyLen 'Auto)))) => NFData (PrivateKey keyLen context)
deriving instance (P.Eq (context (Vector (NumberOfRegisters (BaseField context) keyLen 'Auto))))   => P.Eq   (PrivateKey keyLen context)
deriving instance
    ( P.Show (BaseField context)
    , P.Show (context (Vector (NumberOfRegisters (BaseField context) keyLen 'Auto)))
    ) => P.Show (PrivateKey keyLen context)

deriving instance (Symbolic ctx, KnownRegisters ctx keyLen 'Auto) => SymbolicData (PrivateKey keyLen ctx)

instance
  ( Symbolic ctx
  , KnownNat keyLen
  , KnownRegisters ctx keyLen 'Auto
  ) => SymbolicInput (PrivateKey keyLen ctx) where
    isValid PrivateKey{..} = isValid prvD && isValid prvN

type PubExponentSize = 18

data PublicKey keyLen ctx
    = PublicKey
        { pubE :: UInt PubExponentSize 'Auto ctx
        , pubN :: UInt keyLen 'Auto ctx
        }

deriving instance Generic (PublicKey keyLen context)
deriving instance
    ( NFData (context (Vector (NumberOfRegisters (BaseField context) keyLen 'Auto)))
    , NFData (context (Vector (NumberOfRegisters (BaseField context) PubExponentSize 'Auto)))
    ) =>  NFData  (PublicKey keyLen context)
deriving instance
    ( P.Eq (context (Vector (NumberOfRegisters (BaseField context) keyLen 'Auto)))
    , P.Eq (context (Vector (NumberOfRegisters (BaseField context) PubExponentSize 'Auto)))
    ) =>  P.Eq    (PublicKey keyLen context)
deriving instance
    ( P.Show (context (Vector (NumberOfRegisters (BaseField context) keyLen 'Auto)))
    , P.Show (context (Vector (NumberOfRegisters (BaseField context) PubExponentSize 'Auto)))
    , P.Show (BaseField context)
    ) =>  P.Show  (PublicKey keyLen context)

deriving instance
    ( Symbolic ctx
    , KnownRegisters ctx PubExponentSize 'Auto
    , KnownRegisters ctx keyLen 'Auto
    ) => SymbolicData (PublicKey keyLen ctx)

instance
  ( Symbolic ctx
  , KnownNat keyLen
  , KnownRegisters ctx PubExponentSize 'Auto
  , KnownRegisters ctx keyLen 'Auto
  ) => SymbolicInput (PublicKey keyLen ctx) where
    isValid PublicKey{..} = isValid pubE && isValid pubN

type RSA keyLen msgLen ctx =
   ( SHA2 "SHA256" ctx msgLen
   , KnownNat keyLen
   , KnownNat (2 * keyLen)
   , KnownRegisters ctx keyLen 'Auto
   , KnownRegisters ctx (2 * keyLen) 'Auto
   , KnownNat (Ceil (GetRegisterSize (BaseField ctx) (2 * keyLen) 'Auto) OrdWord)
   , NFData (ctx (Vector keyLen))
   , NFData (ctx (Vector (NumberOfRegisters (BaseField ctx) keyLen 'Auto)))
   , NFData (ctx (Vector (NumberOfRegisters (BaseField ctx) (2 * keyLen) 'Auto)))
   )

sign
    :: forall keyLen msgLen ctx
    .  RSA keyLen msgLen ctx
    => ByteString msgLen ctx
    -> PrivateKey keyLen ctx
    -> Signature keyLen ctx
sign msg PrivateKey{..} = force $ from $ expMod msgI prvD prvN
    where
        h :: ByteString 256 ctx
        h = sha2 @"SHA256" msg

        msgI :: UInt 256 'Auto ctx
        msgI = from h

verify
    :: forall keyLen msgLen ctx
    .  RSA keyLen msgLen ctx
    => ByteString msgLen ctx
    -> Signature keyLen ctx
    -> PublicKey keyLen ctx
    -> Bool ctx
verify msg sig PublicKey{..} = target == input
    where
        h :: ByteString 256 ctx
        h = sha2 @"SHA256" msg

        target :: UInt keyLen 'Auto ctx
        target = force $ expMod (from sig :: UInt keyLen 'Auto ctx) pubE pubN

        input :: UInt keyLen 'Auto ctx
        input = force $ resize (from h :: UInt 256 'Auto ctx)

signVar
    :: forall keyLen msgLen ctx
    .  RSA keyLen msgLen ctx
    => VarByteString msgLen ctx
    -> PrivateKey keyLen ctx
    -> Signature keyLen ctx
signVar msg PrivateKey{..} = force $ from $ expMod msgI prvD prvN
    where
        h :: ByteString 256 ctx
        h = sha2Var @"SHA256" msg

        msgI :: UInt 256 'Auto ctx
        msgI = from h

verifyVar
    :: forall keyLen msgLen ctx
    .  RSA keyLen msgLen ctx
    => VarByteString msgLen ctx
    -> Signature keyLen ctx
    -> PublicKey keyLen ctx
    -> (Bool ctx, ByteString 256 ctx)
verifyVar msg sig PublicKey{..} = (target == input, h)
    where
        h :: ByteString 256 ctx
        h = sha2Var @"SHA256" msg

        target :: UInt keyLen 'Auto ctx
        target = force $ expMod (from sig :: UInt keyLen 'Auto ctx) pubE pubN

        input :: UInt keyLen 'Auto ctx
        input = force $ resize (from h :: UInt 256 'Auto ctx)
