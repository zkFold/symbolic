module Tests.Symbolic.Algorithm.ScaleIssue (specScaleIssue) where

import Data.Function (($))
import Test.Hspec (Spec, it, shouldBe)
import Text.Show (show)
import Prelude (putStrLn, unlines, (<>))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (TwistedEdwards (..), pointGen)
import ZkFold.Algebra.EllipticCurve.Jubjub (
  Fl,
  Fq,
  Jubjub_Scalar,
 )
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA)
import ZkFold.Symbolic.Interpreter (Interpreter)

type I = Interpreter Fq

type Point = Jubjub_Point I

type Scalar = FFA Jubjub_Scalar 'Auto I

specScaleIssue :: Spec
specScaleIssue =
  it "specScaleIssue" $ do
    let g = pointGen @Point
        privKey :: Scalar = one
        pubKey :: Point = privKey `scale` g
    putStrLn $
      unlines
        [ "g = " <> show (SymAffine.affinePoint g)
        , "privKey = " <> show privKey
        , "pubKey = " <> show (SymAffine.affinePoint pubKey)
        ]
    SymAffine.affinePoint pubKey `shouldBe` SymAffine.affinePoint g
    SymAffine.affinePoint (((one :: Scalar) + one + one) `scale` g) `shouldBe` SymAffine.affinePoint (g + g + g)
