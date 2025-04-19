{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Symbolic.Algorithm.Mithril where

import           Data.Foldable                           (foldl')
import           Data.Type.Equality
import           GHC.TypeLits                            (KnownNat)

import           ZkFold.Algebra.Class                    hiding (Euclidean (..))
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Data.Vector                      (Vector)
import           ZkFold.Symbolic.Algorithm.ECDSA.ECDSA  (ecdsaVerify)
import           ZkFold.Symbolic.Class                   (BaseField, Symbolic)
import           ZkFold.Symbolic.Data.Combinators        (GetRegisterSize, NumberOfRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.FFA                (FFA, KnownFFA)
import           ZkFold.Symbolic.Data.FieldElement       (FieldElement)

type StakeDistribution m point ctx = Vector m (point, FieldElement ctx)

mithril
  :: forall m n point curve p q baseField scalarField ctx .
     ( Symbolic ctx
     , baseField ~ FFA q 'Auto ctx
     , scalarField ~ FFA p 'Auto ctx
     , point ~ Weierstrass curve (Point baseField)
     , ScalarFieldOf point ~ scalarField
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     , KnownFFA p 'Auto ctx
     , KnownNat n
     , KnownNat (NumberOfRegisters (BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (BaseField ctx) n 'Auto)
     )
  => StakeDistribution m point ctx
  -> scalarField
  -> (scalarField, scalarField)
  -> FieldElement ctx
mithril stakeDist messageHash (r, s) =
    let
    in
        foldl' (
          \acc (point, stake) ->
            if (ecdsaVerify @n @point point messageHash (r, s))
              then acc + stake
              else acc
          )
          zero
          stakeDist
