{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Algorithm.ECDSA.ECDSA where

import Data.Type.Equality
import GHC.Generics ((:*:) (..))
import GHC.TypeLits (KnownNat)
import qualified Prelude as P

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import qualified ZkFold.Algorithm.Hash.Poseidon as Poseidon
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import qualified ZkFold.Symbolic.Class as S
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Combinators (GetRegisterSize, NumberOfRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point (..))
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA, toUInt)
import ZkFold.Symbolic.Data.UInt (UInt)

-- Verify ECDSA where a caller-provided hash function maps a message to it's hash.
ecdsaVerify
  :: forall message n point curve p q baseField scalarField ctx
   . ( S.Symbolic ctx
     , baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto
     , point ~ Point (Weierstrass curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     , KnownFFA p 'Auto ctx
     , KnownNat n
     , KnownNat (NumberOfRegisters (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) n 'Auto)
     )
  => (message -> scalarField ctx)
  -> point
  -> message
  -> (scalarField :*: scalarField) ctx
  -> Bool ctx
ecdsaVerify hashFn publicKey message (r :*: s) =
  case c of
    (Point x _ isInf) ->
      if isInf || r == zero || s == zero
        then false
        else (toUInt r :: UInt n 'Auto ctx) == toUInt x
 where
  g = pointGen @point

  sInv = finv s

  u1 = hashFn message * sInv

  u2 = r * sInv

  c = u1 `scale` g + u2 `scale` publicKey

-- | Verify ECDSA where the message is hashed using Poseidon.
ecdsaVerifyPoseidon
  :: forall n point curve p q baseField scalarField ctx
   . ( S.Symbolic ctx
     , baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto
     , point ~ Point (Weierstrass curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     , KnownFFA p 'Auto ctx
     , KnownNat n
     , KnownNat (NumberOfRegisters (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) n 'Auto)
     )
  => point
  -> [scalarField ctx]
  -> (scalarField :*: scalarField) ctx
  -> Bool ctx
ecdsaVerifyPoseidon = ecdsaVerify @_ @n Poseidon.poseidonHashDefault

-- | Variant of 'ecdsaVerify' where the message is already hashed.
ecdsaVerifyMessageHash
  :: forall n point curve p q baseField scalarField ctx
   . ( S.Symbolic ctx
     , baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto
     , point ~ Point (Weierstrass curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     , KnownFFA p 'Auto ctx
     , KnownNat n
     , KnownNat (NumberOfRegisters (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) n 'Auto)
     )
  => point
  -> scalarField ctx
  -> (scalarField :*: scalarField) ctx
  -> Bool ctx
ecdsaVerifyMessageHash = ecdsaVerify @_ @n P.id
