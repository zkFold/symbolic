{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Algebra.EllipticCurve.Pairing (
  millerAlgorithmBN,
  millerAlgorithmBLS12,
  finalExponentiation,
) where

import qualified Data.Bool as H
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int8)
import Data.Tuple (snd)
import Data.Type.Equality (type (~))
import Numeric.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Field
import ZkFold.Symbolic.Data.Bool hiding (Bool)
import ZkFold.Symbolic.Data.Conditional
import ZkFold.Symbolic.Data.Eq
import Prelude (fromInteger)
import qualified Prelude

-- Ate pairing implementation adapted from:
-- https://github.com/sdiehl/pairing/blob/master/src/Data/Pairing/Ate.hs

type Untwisted baseField i j = Ext2 (Ext3 baseField i) j

finalExponentiation
  :: forall scalarField baseField g i j
   . (Finite scalarField, Finite baseField)
  => (g ~ Untwisted baseField i j, Exponent g Natural)
  => g -> g
finalExponentiation x = x ^ ((p ^ (12 :: Natural) -! 1) `div` r)
 where
  p = order @baseField
  r = order @scalarField

millerAlgorithmBLS12
  :: forall c d fldC fldD i j g
   . WeierstrassCurve d fldD
  => FiniteField fldC
  => Scale fldC fldD
  => Conditional (BooleanOf fldD) (BooleanOf fldD)
  => BooleanOf fldC ~ BooleanOf fldD
  => Untwisted fldD i j ~ g
  => Field g
  => [Int8]
  -> Weierstrass c (Point fldC)
  -> Weierstrass d (Point fldD)
  -> g
millerAlgorithmBLS12 (x : xs) p q =
  snd $
    millerLoop p q xs (H.bool (negate q) q (x Prelude.> 0), one)
millerAlgorithmBLS12 _ _ _ = one

millerAlgorithmBN
  :: forall c d fldC fldD i j g
   . WeierstrassCurve d fldD
  => FiniteField fldC
  => Scale fldC fldD
  => Conditional (BooleanOf fldD) (BooleanOf fldD)
  => BooleanOf fldC ~ BooleanOf fldD
  => Untwisted fldD i j ~ g
  => Field g
  => fldD
  -> [Int8]
  -> Weierstrass c (Point fldC)
  -> Weierstrass d (Point fldD)
  -> g
millerAlgorithmBN xi (x : xs) p q =
  finalStepBN xi p q $
    millerLoop p q xs (H.bool (negate q) q (x Prelude.> 0), one)
millerAlgorithmBN _ _ _ _ = one

-- --------------------------------------------------------------------------------

finalStepBN
  :: forall c d fldC fldD i j g
   . WeierstrassCurve d fldD
  => FiniteField fldC
  => Scale fldC fldD
  => Conditional (BooleanOf fldD) (BooleanOf fldD)
  => BooleanOf fldC ~ BooleanOf fldD
  => Untwisted fldD i j ~ g
  => Field g
  => fldD
  -> Weierstrass c (Point fldC)
  -> Weierstrass d (Point fldD)
  -> (Weierstrass d (Point fldD), g)
  -> g
finalStepBN xi p q (t, f) = f * f' * f''
 where
  o = order @fldC
  q1 = frobTwisted o xi q
  (t', f') = lineFunction p t q1
  q2 = negate (frobTwisted o xi q1)
  (_, f'') = lineFunction p t' q2

millerLoop
  :: WeierstrassCurve d fldD
  => Field fldC
  => Scale fldC fldD
  => Conditional (BooleanOf fldD) (BooleanOf fldD)
  => BooleanOf fldC ~ BooleanOf fldD
  => Untwisted fldD i j ~ g
  => Field g
  => Weierstrass c (Point fldC)
  -> Weierstrass d (Point fldD)
  -> [Int8]
  -> (Weierstrass d (Point fldD), g)
  -> (Weierstrass d (Point fldD), g)
millerLoop p q = impl
 where
  impl [] tf = tf
  impl (x : xs) tf
    | x Prelude.== 0 = impl xs tf2
    | x Prelude.== 1 = impl xs $ additionStep p q tf2
    | H.otherwise = impl xs $ additionStep p (negate q) tf2
   where
    tf2 = doublingStep p tf

--------------------------------------------------------------------------------

frobTwisted
  :: forall c fld
   . ( WeierstrassCurve c fld
     , Conditional (BooleanOf fld) (BooleanOf fld)
     )
  => Natural -> fld -> Weierstrass c (Point fld) -> Weierstrass c (Point fld)
frobTwisted q xi (Weierstrass (Point x y isInf)) =
  if isInf then pointInf else pointXY ((x ^ q) * (xi ^ tx)) ((y ^ q) * (xi ^ ty))
 where
  tx = (q -! 1) `div` 3
  ty = q `div` 2

additionStep
  :: WeierstrassCurve d fldD
  => Field fldC
  => Scale fldC fldD
  => BooleanOf fldC ~ BooleanOf fldD
  => -- Conditional bool fldD =>
  Conditional (BooleanOf fldD) (BooleanOf fldD)
  => Untwisted fldD i j ~ g
  => Field g
  => Weierstrass c (Point fldC)
  -> Weierstrass d (Point fldD)
  -> (Weierstrass d (Point fldD), g)
  -> (Weierstrass d (Point fldD), g)
additionStep p q (t, f) = (* f) <$> lineFunction p q t

doublingStep
  :: WeierstrassCurve d fldD
  => Field fldC
  => Scale fldC fldD
  => Conditional (BooleanOf fldD) (BooleanOf fldD)
  => BooleanOf fldC ~ BooleanOf fldD
  => Untwisted fldD i j ~ g
  => Field g
  => Weierstrass c (Point fldC)
  -> (Weierstrass d (Point fldD), g)
  -> (Weierstrass d (Point fldD), g)
doublingStep p (t, f) = (* f) . (* f) <$> lineFunction p t t

lineFunction
  :: forall c d baseFieldC baseFieldD i j g
   . WeierstrassCurve d baseFieldD
  => Field baseFieldC
  => Scale baseFieldC baseFieldD
  => Conditional (BooleanOf baseFieldD) (BooleanOf baseFieldD)
  => -- Conditional bool baseFieldD =>
  BooleanOf baseFieldC ~ BooleanOf baseFieldD
  => Untwisted baseFieldD i j ~ g
  => Weierstrass c (Point baseFieldC)
  -> Weierstrass d (Point baseFieldD)
  -> Weierstrass d (Point baseFieldD)
  -> (Weierstrass d (Point baseFieldD), g)
lineFunction
  (Weierstrass (Point x y isInf))
  (Weierstrass (Point x1 y1 isInf1))
  (Weierstrass (Point x2 y2 isInf2)) =
    if isInf || isInf1 || isInf2
      then (pointInf, Ext2 (Ext3 one zero zero) zero)
      else
        if x1 /= x2
          then (pointXY x3 y3, untwist (negate y) (x `scale` l) (y1 - l * x1))
          else
            if y1 + y2 == zero
              then (pointInf, untwist x (negate x1) zero)
              else (pointXY x3' y3', untwist (negate y) (x `scale` l') (y1 - l' * x1))
   where
    l = (y2 - y1) // (x2 - x1)
    x3 = l * l - x1 - x2
    y3 = l * (x1 - x3) - y1
    x12 = x1 * x1
    l' = (x12 + x12 + x12) // (y1 + y1)
    x3' = l' * l' - x1 - x2
    y3' = l' * (x1 - x3') - y1
    untwist a b c = Ext2 (Ext3 (a `scale` one) zero zero) (Ext3 b c zero)
