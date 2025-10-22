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
import Prelude (($))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class hiding (AffinePoint, Point)
import qualified ZkFold.Algebra.EllipticCurve.Class as Elliptic
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class (Symbolic (..))
import qualified ZkFold.Symbolic.Class as S
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Combinators (Iso (..), RegisterSize (..))
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt (UInt (..))

-- https://cryptobook.nakov.com/digital-signatures/eddsa-and-ed25519 for how to derive the signature and perform verification.

-- | Verify EdDSA signature on a Twisted Edwards curve.
--
-- It checks the standard relation:  s*G == R + H(R, A, M) * A
-- where:
--   - A is the public key (point)
--   - (R, s) is the signature; R is a point, s is a scalar
--   - H is a caller-provided hash-to-scalar function
eddsaVerify
  :: forall point curve p q baseField scalarField ctx
   . ( S.Symbolic ctx
     , baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto
     , point ~ SymAffine.AffinePoint (TwistedEdwards curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     , KnownFFA p 'Auto ctx
     )
  => (forall x. SymbolicData x => x ctx -> FieldElement ctx)
  -- ^ hash function
  -> point
  -- ^ public key A
  -> FieldElement ctx
  -- ^ message M
  -> (SymAffine.AffinePoint (TwistedEdwards curve) baseField :*: scalarField) ctx
  -- ^ signature (R, s)
  -> Bool ctx
eddsaVerify hashFn publicKey message (rPoint :*: s) =
  unwrap lhs == unwrap rhs -- `unwrap` as `Eq` instance is missing.
 where
  g = pointGen @point

  h :: scalarField ctx = scalarFieldFromFE $ hashFn (rPoint :*: publicKey :*: message)

  lhs = s `scale` g

  rhs = rPoint + h `scale` publicKey

  unwrap :: point -> Elliptic.AffinePoint (baseField ctx)
  unwrap = coerce

-- | Sign EdDSA signature on a Twisted Edwards curve.
eddsaSign
  :: forall point curve p q baseField scalarField ctx
   . ( baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto
     , point ~ SymAffine.AffinePoint (TwistedEdwards curve) baseField ctx
     , ScalarFieldOf point ~ scalarField ctx
     , CyclicGroup point
     , Symbolic ctx
     , KnownFFA p 'Auto ctx
     )
  => (forall x. SymbolicData x => x ctx -> FieldElement ctx)
  -- ^ hash function
  -> scalarField ctx
  -- ^ private key
  -> FieldElement ctx
  -- ^ message M
  -> (SymAffine.AffinePoint (TwistedEdwards curve) baseField :*: scalarField) ctx
  -- ^ signature (R, s)
eddsaSign hashFn privKey message =
  rPoint :*: s
 where
  g = pointGen @point
  publicKey = privKey `scale` g
  r :: scalarField ctx = scalarFieldFromFE $ hashFn (hashFn privKey :*: message)
  s = r + h * privKey
  rPoint = r `scale` g
  h = scalarFieldFromFE $ hashFn (rPoint :*: publicKey :*: message)

-- | __NOTE__: This function assumes that the given field element is in base field.
scalarFieldFromFE
  :: forall p c
   . ( Symbolic c
     , KnownFFA p 'Auto c
     )
  => FieldElement c -> FFA p 'Auto c
scalarFieldFromFE fe =
  let
    u :: UInt (NumberOfBits (BaseField c)) 'Auto c = from fe
   in
    fromUInt u
