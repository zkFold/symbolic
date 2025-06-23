{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Symbolic.Algorithm.RSA (
  sign,
  verify,
  signVar,
  verifyVar,
  RSA,
  PublicKey (..),
  PrivateKey (..),
  PubExponentSize,
  Signature,
) where

import Control.DeepSeq (NFData, force)
import GHC.Generics (Generic)
import ZkFold.Algebra.Number
import ZkFold.Data.HFunctor.Classes (HEq, HNFData, HShow)
import ZkFold.Symbolic.Algorithm.Hash.SHA2 (SHA2, sha2, sha2Var)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Bool (Bool, (&&))
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (
  Ceil,
  GetRegisterSize,
  Iso (..),
  KnownRegisters,
  RegisterSize (..),
  Resize (..),
 )
import ZkFold.Symbolic.Data.Eq
import ZkFold.Symbolic.Data.Input (SymbolicInput, isValid)
import ZkFold.Symbolic.Data.UInt (OrdWord, UInt, expMod)
import ZkFold.Symbolic.Data.VarByteString (VarByteString)
import Prelude (($))
import qualified Prelude as P

type Signature keyLen ctx = ByteString keyLen ctx

data PrivateKey keyLen ctx
  = PrivateKey
  { prvD :: UInt keyLen 'Auto ctx
  , prvN :: UInt keyLen 'Auto ctx
  }

deriving instance Generic (PrivateKey keyLen context)

deriving instance HNFData context => NFData (PrivateKey keyLen context)

deriving instance HEq context => P.Eq (PrivateKey keyLen context)

deriving instance HShow context => P.Show (PrivateKey keyLen context)

deriving instance (KnownRegisters ctx keyLen 'Auto, Symbolic ctx) => SymbolicData (PrivateKey keyLen ctx)

instance
  ( KnownNat keyLen
  , KnownRegisters ctx keyLen 'Auto
  , Symbolic ctx
  )
  => SymbolicInput (PrivateKey keyLen ctx)
  where
  isValid PrivateKey {..} = isValid prvD && isValid prvN

type PubExponentSize = 18

data PublicKey keyLen ctx
  = PublicKey
  { pubE :: UInt PubExponentSize 'Auto ctx
  , pubN :: UInt keyLen 'Auto ctx
  }

deriving instance Generic (PublicKey keyLen context)

deriving instance HNFData context => NFData (PublicKey keyLen context)

deriving instance HEq context => P.Eq (PublicKey keyLen context)

deriving instance HShow context => P.Show (PublicKey keyLen context)

deriving instance
  ( KnownRegisters ctx PubExponentSize 'Auto
  , KnownRegisters ctx keyLen 'Auto
  , Symbolic ctx
  )
  => SymbolicData (PublicKey keyLen ctx)

instance
  ( KnownNat keyLen
  , KnownRegisters ctx PubExponentSize 'Auto
  , KnownRegisters ctx keyLen 'Auto
  , Symbolic ctx
  )
  => SymbolicInput (PublicKey keyLen ctx)
  where
  isValid PublicKey {..} = isValid pubE && isValid pubN

type RSA keyLen msgLen ctx =
  ( SHA2 "SHA256" ctx msgLen
  , KnownNat keyLen
  , KnownNat (2 * keyLen)
  , KnownRegisters ctx keyLen 'Auto
  , KnownRegisters ctx (2 * keyLen) 'Auto
  , KnownNat (Ceil (GetRegisterSize (BaseField ctx) (2 * keyLen) 'Auto) OrdWord)
  )

sign
  :: forall keyLen msgLen ctx
   . RSA keyLen msgLen ctx
  => ByteString msgLen ctx
  -> PrivateKey keyLen ctx
  -> Signature keyLen ctx
sign msg PrivateKey {..} = force $ from $ expMod msgI prvD prvN
 where
  h :: ByteString 256 ctx
  h = sha2 @"SHA256" msg

  msgI :: UInt 256 'Auto ctx
  msgI = from h

verify
  :: forall keyLen msgLen ctx
   . RSA keyLen msgLen ctx
  => ByteString msgLen ctx
  -> Signature keyLen ctx
  -> PublicKey keyLen ctx
  -> Bool ctx
verify msg sig PublicKey {..} = target == input
 where
  h :: ByteString 256 ctx
  h = sha2 @"SHA256" msg

  target :: UInt keyLen 'Auto ctx
  target = force $ expMod (from sig :: UInt keyLen 'Auto ctx) pubE pubN

  input :: UInt keyLen 'Auto ctx
  input = force $ resize (from h :: UInt 256 'Auto ctx)

signVar
  :: forall keyLen msgLen ctx
   . RSA keyLen msgLen ctx
  => VarByteString msgLen ctx
  -> PrivateKey keyLen ctx
  -> Signature keyLen ctx
signVar msg PrivateKey {..} = force $ from $ expMod msgI prvD prvN
 where
  h :: ByteString 256 ctx
  h = sha2Var @"SHA256" msg

  msgI :: UInt 256 'Auto ctx
  msgI = from h

verifyVar
  :: forall keyLen msgLen ctx
   . RSA keyLen msgLen ctx
  => VarByteString msgLen ctx
  -> Signature keyLen ctx
  -> PublicKey keyLen ctx
  -> (Bool ctx, ByteString 256 ctx)
verifyVar msg sig PublicKey {..} = (target == input, h)
 where
  h :: ByteString 256 ctx
  h = sha2Var @"SHA256" msg

  target :: UInt keyLen 'Auto ctx
  target = force $ expMod (from sig :: UInt keyLen 'Auto ctx) pubE pubN

  input :: UInt keyLen 'Auto ctx
  input = force $ resize (from h :: UInt 256 'Auto ctx)
