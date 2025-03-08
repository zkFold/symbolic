{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE TypeOperators       #-}
module ZkFold.Symbolic.Algorithms.ECDSA.ECDSA where
import           Data.Type.Equality
import           GHC.Base                                (($))
import           GHC.TypeLits                            (KnownNat, Log2)
import qualified GHC.TypeNats

import           ZkFold.Base.Algebra.Basic.Class         hiding (Euclidean (..))
import           ZkFold.Base.Algebra.Basic.Number        (value)
import           ZkFold.Base.Algebra.EllipticCurve.Class
import qualified ZkFold.Symbolic.Class                   as S
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString         (ByteString)
import           ZkFold.Symbolic.Data.Combinators        (Iso (..), NumberOfRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement       (FieldElement)
import           ZkFold.Symbolic.Data.UInt               (UInt, eea)

ecdsaVerify
  :: forall point n c curve baseField.
     ( S.Symbolic c
     , KnownNat n
     , baseField ~ UInt 256 'Auto c
     , ScalarFieldOf point ~ FieldElement c
     , point ~ Weierstrass curve (Point baseField)
     , CyclicGroup point
     , SemiEuclidean (UInt 256 'Auto c)
     , KnownNat (NumberOfRegisters (S.BaseField c) 256 'Auto)
     , Log2 (Order (S.BaseField c) GHC.TypeNats.- 1) ~ 255
     )
  => point
  -> ByteString 256 c
  -> (UInt 256 'Auto c, UInt 256 'Auto c)
  -> Bool c
ecdsaVerify publicKey message (r, s) =
    case c of Weierstrass (Point x _ isInf) -> if isInf then false else r == (x `mod` n)
    where
        n = fromConstant $ value @n

        g = pointGen @point

        (sInv, _, _) = eea s n

        u = (from message * sInv) `mod` n

        v = r * sInv `mod` n

        c = (from u :: FieldElement c) `scale` g + (from v :: FieldElement c) `scale` publicKey
