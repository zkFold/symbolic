{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Algorithm.EdDSA (
  eddsaVerify,
  eddsaSign,
) where

import Data.Coerce (coerce)
import Data.Type.Equality
import GHC.Generics ((:*:) (..))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (AffinePoint, Point)
import qualified ZkFold.Algebra.EllipticCurve.Class as Elliptic
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import qualified ZkFold.Symbolic.Class as S
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Combinators (RegisterSize (..))
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA
import Prelude (undefined)

-- | Verify EdDSA signature on a Twisted Edwards curve.
--
-- It checks the standard relation:  s*G == R + H(R, A, M) * A
-- where:
--   - A is the public key (point)
--   - (R, s) is the signature; R is a point, s is a scalar
--   - H is a caller-provided hash-to-scalar function
eddsaVerify
  :: forall message point curve p q baseField scalarField ctx
   . ( S.Symbolic ctx
     , baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto
     , point ~ SymAffine.AffinePoint (TwistedEdwards curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     )
  => ( point
       -> point
       -> message
       -> scalarField ctx
     )
  -> point
  -- ^ public key A
  -> message
  -- ^ message M
  -> (SymAffine.AffinePoint (TwistedEdwards curve) baseField :*: scalarField) ctx
  -- ^ signature (R, s)
  -> Bool ctx
eddsaVerify hashFn publicKey message (rPoint :*: s) =
  if isIdentity rPoint || s == zero
    then false
    else unwrap lhs == unwrap rhs -- `unwrap` as `Eq` instance is missing.
 where
  g = pointGen @point

  h = hashFn rPoint publicKey message

  lhs = s `scale` g

  rhs = rPoint + h `scale` publicKey

  unwrap :: point -> Elliptic.AffinePoint (baseField ctx)
  unwrap = coerce

  isIdentity :: point -> Bool ctx
  isIdentity p = unwrap p == Elliptic.pointXY zero one


-- | Sign EdDSA signature on a Twisted Edwards curve.
eddsaSign
  :: forall message point curve p q baseField scalarField ctx
   . ( baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto
     , point ~ SymAffine.AffinePoint (TwistedEdwards curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     )
  => ( point
       -> point
       -> message
       -> scalarField ctx
     )
  -> scalarField ctx
  -- ^ private key
  -> message
  -- ^ message M
  -> (SymAffine.AffinePoint (TwistedEdwards curve) baseField :*: scalarField) ctx
  -- ^ signature (R, s)
eddsaSign hashFn privKey message =
   rPoint :*: s
 where
  g = pointGen @point
  publicKey = privKey `scale` g
  r :: scalarField ctx = undefined
  s = r + h * privKey
  rPoint = r `scale` g
  h = hashFn rPoint publicKey message