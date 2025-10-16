{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Symbolic.Algorithm.ScaleIssue (specScaleIssue) where

import Data.Function (($))
import GHC.Generics ((:*:) (..))
import Test.Hspec (Spec, shouldBe, it)
import Text.Show (show)
import Prelude ((<>), putStrLn, unlines)

import Tests.Common (evalBool)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import ZkFold.Algebra.EllipticCurve.Jubjub (Jubjub_Base, Jubjub_Scalar)
import ZkFold.Algebra.EllipticCurve.Class (pointGen)
import ZkFold.Symbolic.Data.EllipticCurve.Jubjub (Jubjub_Point)
import qualified ZkFold.Symbolic.Data.EllipticCurve.Point.Affine as SymAffine
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA, fromUInt)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Interpreter (Interpreter)
import qualified ZkFold.Symbolic.Algorithm.Hash.MiMC as MiMC
import ZkFold.Data.Eq
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Combinators
import GHC.TypeNats (KnownNat)
import ZkFold.Symbolic.Data.UInt (OrdWord, UInt)
import ZkFold.Algebra.Number (value)

type I = Interpreter Fr
type Point = Jubjub_Point I
type Scalar = FFA Jubjub_Scalar 'Auto I

scalarFieldFromFE
  :: forall p c n
   . (Symbolic c, KnownFFA p 'Auto c
     , n ~ NumberOfBits (BaseField c)
     , KnownRegisters c n 'Auto
     , KnownNat (GetRegisterSize (BaseField c) n 'Auto)
     , KnownNat (Ceil (GetRegisterSize (BaseField c) n 'Auto) OrdWord)
   )
  => FieldElement c -> FFA p 'Auto c
scalarFieldFromFE fe =
  let
    u :: UInt (NumberOfBits (BaseField c)) 'Auto c = from fe
    m = fromConstant (value @p)
   in
    fromUInt $ mod u m

hashToScalar :: (KnownFFA Jubjub_Base 'Auto I, KnownFFA Jubjub_Scalar 'Auto I) => Point -> Point -> FieldElement I -> Scalar
hashToScalar rPoint pubKey m = scalarFieldFromFE (MiMC.hash (rPoint :*: pubKey :*: m))

specScaleIssue :: Spec
specScaleIssue = 
  it "specScaleIssue" $ do
    let g = pointGen @Point
        msg = zero :: FieldElement I
        privKey = one
        pubKey = privKey `scale` g
        r :: Scalar = one + one  -- Works for `r = one`.
        rPoint = r `scale` g
        h = hashToScalar rPoint pubKey msg
        hpubKey' = (h * privKey) `scale` g
        hpubKey = h `scale` pubKey  -- `hpubKey` and `hpubKey'` should both be equal.
        ok = SymAffine.affinePoint hpubKey == SymAffine.affinePoint hpubKey'
    putStrLn $ 
      unlines [
        "g = " <> show (SymAffine.affinePoint g),
        "privKey = " <> show privKey,
        "pubKey = " <> show (SymAffine.affinePoint pubKey),
        "r = " <> show r,
        "rPoint = " <> show (SymAffine.affinePoint rPoint),
        "ok = " <> show ok,
        "hpubKey' = " <> show (SymAffine.affinePoint hpubKey'),
        "hpubKey = " <> show (SymAffine.affinePoint hpubKey),
        "h = " <> show h
      ]
    
    evalBool ok `shouldBe` one