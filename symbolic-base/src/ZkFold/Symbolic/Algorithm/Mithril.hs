{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Algorithm.Mithril where

import Data.Foldable (foldl')
import Data.Type.Equality
import GHC.Generics ((:*:) (..))
import GHC.TypeLits (KnownNat)

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Algebra.EllipticCurve.Class hiding (Point)
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Algorithm.ECDSA.ECDSA (ecdsaVerifyMessageHash)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Combinators (GetRegisterSize, KnownRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.V2 (Symbolic)

type StakeDistribution m point ctx =
  Vector m (point, CompatData FieldElement ctx)

mithril
  :: forall m n point curve p q baseField scalarField ctx
   . ( Symbolic ctx
     , baseField ~ FFA q 'Auto
     , scalarField ~ FFA p 'Auto ctx
     , point ~ Point (Weierstrass curve) baseField ctx
     , ScalarFieldOf point ~ scalarField
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     , KnownFFA p 'Auto ctx
     , KnownNat n
     , KnownRegisters ctx n 'Auto
     , KnownNat (GetRegisterSize ctx n 'Auto)
     )
  => StakeDistribution m point ctx
  -> scalarField
  -> (scalarField, scalarField)
  -> CompatData FieldElement ctx
mithril stakeDist messageHash (r, s) =
  let
   in foldl'
        ( \acc (point, stake) ->
            if ecdsaVerifyMessageHash @n point messageHash (r :*: s)
              then acc + stake
              else acc
        )
        zero
        stakeDist
