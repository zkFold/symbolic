{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE TypeOperators       #-}

module ZkFold.Symbolic.Algorithms.ECDSA.ECDSA where
import           Data.Type.Equality
import           GHC.TypeLits                            (KnownNat)

import           ZkFold.Base.Algebra.Basic.Class         hiding (Euclidean (..))
import           ZkFold.Base.Algebra.EllipticCurve.Class
import qualified ZkFold.Symbolic.Class                   as S
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Combinators        (GetRegisterSize, NumberOfRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FFA                (FFA, KnownFFA, toUInt)
import           ZkFold.Symbolic.Data.UInt               (UInt)

-- TODO: verify the actual message instead of a hash
ecdsaVerify
  :: forall n point curve p q baseField scalarField ctx .
     ( S.Symbolic ctx
     , baseField ~ FFA q 'Auto ctx
     , scalarField ~ FFA p 'Auto ctx
     , point ~ Weierstrass curve (Point baseField)
     , ScalarFieldOf point ~ scalarField
     , CyclicGroup point
     , KnownFFA q 'Auto ctx
     , KnownFFA p 'Auto ctx
     , KnownNat n
     , KnownNat (NumberOfRegisters (S.BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (S.BaseField ctx) n 'Auto)
     )
  => point
  -> scalarField
  -> (scalarField, scalarField)
  -> Bool ctx
ecdsaVerify publicKey messageHash (r, s) =
    case c of
      Weierstrass (Point x _ isInf) ->
        if isInf || r == zero || s == zero
          then false
          else (toUInt r :: UInt n 'Auto ctx) == toUInt x
    where
        g = pointGen @point

        sInv = finv s

        u1 = messageHash * sInv

        u2 = r * sInv

        c = u1 `scale` g + u2 `scale` publicKey
