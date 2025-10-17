{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Algorithm.ScaleIssue (specScaleIssue) where

import Data.Function (($))
import GHC.Generics ((:*:) (..))
import Test.Hspec (Spec, it, shouldBe)
import Text.Show (show)
import Prelude (putStrLn, unlines, (<>))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (pointGen)
import ZkFold.Algebra.EllipticCurve.Jubjub (
  Fl,
  Jubjub_Base,
  Jubjub_Scalar,
 )
import ZkFold.Algebra.Number (value)
import ZkFold.Data.Bool (BoolType (..))
import ZkFold.Data.Eq
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA, FFAMaxBits, KnownFFA, fromUInt)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Interpreter (Interpreter)

type I = Interpreter Fl

type Point = Jubjub_Point I

type Scalar = FFA Jubjub_Scalar 'Auto I

scalarFieldFromFE
  :: forall p c
   . ( Symbolic c
     , KnownFFA p 'Auto c
     )
  => FieldElement c -> FFA p 'Auto c
scalarFieldFromFE fe =
  let
    u :: UInt (NumberOfBits (BaseField c)) 'Auto c = from fe
    uWide = resize u
    m :: UInt (FFAMaxBits p c) 'Auto c = fromConstant (value @p)
   in
    fromUInt $ mod uWide m

hashToScalar
  :: (KnownFFA Jubjub_Base 'Auto I, KnownFFA Jubjub_Scalar 'Auto I) => Point -> Point -> FieldElement I -> Scalar
hashToScalar rPoint pubKey m = scalarFieldFromFE (MiMC.hash (rPoint :*: pubKey :*: m))

hashToScalar'
  :: Point -> Point -> FieldElement I -> FieldElement I
hashToScalar' rPoint pubKey m = MiMC.hash (rPoint :*: pubKey :*: m)

specScaleIssue :: Spec
specScaleIssue =
  it "specScaleIssue" $ do
    let g = pointGen @Point
        msg = zero :: FieldElement I
        privKey = one
        pubKey = privKey `scale` g
        r :: Scalar = one -- + one -- Works for `r = one`.
        rPoint = r `scale` g
        h = hashToScalar rPoint pubKey msg
        h' = hashToScalar' rPoint pubKey msg
        hpubKey' = (h * privKey) `scale` g
        hpubKey = h `scale` pubKey -- `hpubKey` and `hpubKey'` should both be equal.
        ok = SymAffine.affinePoint hpubKey == SymAffine.affinePoint hpubKey'
        ok' = toConstant h' == toConstant h
    putStrLn $
      unlines
        [ "g = " <> show (SymAffine.affinePoint g)
        , "privKey = " <> show privKey
        , "pubKey = " <> show (SymAffine.affinePoint pubKey)
        , "r = " <> show r
        , "rPoint = " <> show (SymAffine.affinePoint rPoint)
        , "ok = " <> show ok
        , "hpubKey' = " <> show (SymAffine.affinePoint hpubKey')
        , "hpubKey = " <> show (SymAffine.affinePoint hpubKey)
        , "h = " <> show h
        , "h' = " <> show h'
        ]
    ok' `shouldBe` true
    ok `shouldBe` true
