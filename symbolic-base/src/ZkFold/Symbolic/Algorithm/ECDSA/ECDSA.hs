{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Algorithm.ECDSA.ECDSA where

import Data.Type.Equality
import GHC.Generics ((:*:) (..))
import qualified Prelude as P

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point (..))
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA, toUInt)
import ZkFold.Symbolic.Data.UInt

-- Verify ECDSA where a caller-provided hash function maps a message to it's hash.
ecdsaVerify
  :: forall message n point curve p q baseField scalarField ctx
   . ( Symbolic ctx
     , baseField ~ FFA q
     , scalarField ~ FFA p
     , point ~ Point (Weierstrass curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , KnownFFA q ctx
     , KnownFFA p ctx
     , KnownUInt n ctx
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
        else (toUInt r :: UInt n ctx) == toUInt x
 where
  g = pointGen @point

  sInv = finv s

  u1 = hashFn message * sInv

  u2 = r * sInv

  c = u1 `scale` g + u2 `scale` publicKey

-- | Variant of 'ecdsaVerify' where the message is already hashed.
ecdsaVerifyMessageHash
  :: forall n point curve p q baseField scalarField ctx
   . ( Symbolic ctx
     , baseField ~ FFA q
     , scalarField ~ FFA p
     , point ~ Point (Weierstrass curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , KnownFFA q ctx
     , KnownFFA p ctx
     , KnownUInt n ctx
     )
  => point
  -> scalarField ctx
  -> (scalarField :*: scalarField) ctx
  -> Bool ctx
ecdsaVerifyMessageHash = ecdsaVerify @_ @n P.id
