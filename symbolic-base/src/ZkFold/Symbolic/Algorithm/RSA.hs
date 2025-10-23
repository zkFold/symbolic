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

import GHC.Generics (Generic, Generic1)
import Prelude (($))

import ZkFold.Algebra.Number (type (*), type (<=))
import ZkFold.Data.Eq
import ZkFold.Symbolic.Algorithm.Hash.SHA2
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Data.VarByteString (VarByteString)

type Signature keyLen ctx = ByteString keyLen ctx

data PrivateKey keyLen ctx
  = PrivateKey
  { prvD :: UInt keyLen ctx
  , prvN :: UInt keyLen ctx
  }
  deriving (Generic, Generic1, SymbolicData)

type PubExponentSize = 18

data PublicKey keyLen ctx
  = PublicKey
  { pubE :: UInt PubExponentSize ctx
  , pubN :: UInt keyLen ctx
  }
  deriving (Generic, Generic1, SymbolicData, SymbolicInput)

type RSA keyLen msgLen ctx =
  ( SHA2 "SHA256" ctx msgLen
  , KnownUInt keyLen ctx
  , KnownUInt (2 * keyLen) ctx
  , KnownUInt PubExponentSize ctx
  , KnownUInt 256 ctx
  , 1 <= PaddedLength msgLen (ChunkSize "SHA256") (2 * WordSize "SHA256")
  )

sign
  :: forall keyLen msgLen ctx
   . RSA keyLen msgLen ctx
  => ByteString msgLen ctx
  -> PrivateKey keyLen ctx
  -> Signature keyLen ctx
sign msg PrivateKey {..} = uintToBSbe $ expMod msgI prvD prvN
 where
  h :: ByteString 256 ctx
  h = sha2 @"SHA256" msg

  msgI :: UInt 256 ctx
  msgI = beBSToUInt h

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

  target :: UInt keyLen ctx
  target = expMod (beBSToUInt sig) pubE pubN

  input :: UInt keyLen ctx
  input = resizeUInt (beBSToUInt h)

signVar
  :: forall keyLen msgLen ctx
   . RSA keyLen msgLen ctx
  => VarByteString msgLen ctx
  -> PrivateKey keyLen ctx
  -> Signature keyLen ctx
signVar msg PrivateKey {..} = uintToBSbe $ expMod msgI prvD prvN
 where
  h :: ByteString 256 ctx
  h = sha2Var @"SHA256" msg

  msgI :: UInt 256 ctx
  msgI = beBSToUInt h

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

  target :: UInt keyLen ctx
  target = expMod (beBSToUInt sig) pubE pubN

  input :: UInt keyLen ctx
  input = resizeUInt (beBSToUInt h)
