module Tests.Symbolic.Algorithm.ScaleIssue (specScaleIssue) where

import Data.Function (($))
import Test.Hspec (Spec, it, shouldBe)
import Text.Show (show)
import Prelude (putStrLn, unlines, (<>))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (TwistedEdwards (..), pointGen)
import ZkFold.Algebra.EllipticCurve.Jubjub (
  Fq,
  Jubjub_Scalar,
 )
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA, fromUInt)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt (UInt)
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
        orderNatural = order @Scalar
        orderFFA :: Scalar = fromConstant orderNatural
        orderUInt = fromConstant orderNatural
        -- orderFFAViaUInt :: Scalar = fromUInt (fromConstant orderNatural)
        hashResult :: FieldElement I = MiMC.hash g
        hashResultUInt :: UInt (NumberOfBits (BaseField I)) 'Auto I = from hashResult
        hashResultC1 = toConstant hashResult
        hashResultC2 = toConstant hashResultUInt -- 'hashResultC1' is same as 'hashResultC2', so UInt <-> FE conversation is likely correct.
        hashResultUIntMod = hashResultUInt `mod` orderUInt
        hashResultMod = toConstant hashResultUIntMod -- I verified that it equals to the value computed manually.
        hashResultModFFA :: Scalar = fromUInt hashResultUIntMod
        hashResultModFFAConstant = toConstant hashResultModFFA -- Should be same as 'hashResultMod'. But was not until the accompanying change in ZkFold.Symbolic.Data.FFA module.
        hashResultModFFART :: Scalar = fromConstant hashResultModFFAConstant -- Likewise, round-trip was failing earlier.
        hpubKey' = (hashResultModFFA * privKey) `scale` g
        hpubKey = hashResultModFFA `scale` pubKey
    putStrLn $
      unlines
        [ "g = " <> show (SymAffine.affinePoint g)
        , "privKey = " <> show privKey
        , "pubKey = " <> show (SymAffine.affinePoint pubKey)
        , "orderNatural = " <> show orderNatural
        , "orderFFA = " <> show orderFFA
        , "hashResult = " <> show hashResult
        , "hashResultUInt = " <> show hashResultUInt
        , "hashResultC1 = " <> show hashResultC1
        , "hashResultC2 = " <> show hashResultC2
        , "hashResultMod = " <> show hashResultMod
        , "hashResultModFFAConstant = " <> show hashResultModFFAConstant -- Not same as 'hashResultMod'!
        , "hashResultModFFA " <> show hashResultModFFA
        , "hashResultModFFART " <> show hashResultModFFART
        , "hpubKey = " <> show (SymAffine.affinePoint hpubKey)
        , "hpubKey' = " <> show (SymAffine.affinePoint hpubKey')
        ]
    SymAffine.affinePoint pubKey `shouldBe` SymAffine.affinePoint g
    SymAffine.affinePoint (((one :: Scalar) + one + one) `scale` g) `shouldBe` SymAffine.affinePoint (g + g + g)
    SymAffine.affinePoint (orderNatural `scale` g) `shouldBe` SymAffine.affinePoint (zero :: Point)
    SymAffine.affinePoint (orderFFA `scale` g) `shouldBe` SymAffine.affinePoint (zero :: Point)
    hashResultModFFA `shouldBe` hashResultModFFART
    SymAffine.affinePoint hpubKey `shouldBe` SymAffine.affinePoint hpubKey'
