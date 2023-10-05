{-# LANGUAGE TypeApplications #-}

module Main where

import           Prelude                            hiding (Num(..), (^))

import           ZkFold.Crypto.Algebra.Class
import           ZkFold.Crypto.Algebra.Field
import           ZkFold.Crypto.Arithmetization.R1CS
import           ZkFold.Crypto.EllipticCurve.Class 

-- Tests with Elliptic Curve
instance EllipticCurve Integer where
  on_curve _ = True
  generator = BaseField 1

  add (BaseField p1) (BaseField p2) = BaseField (p1 + p2)
  mul (ScalarField p1) (BaseField p2) = BaseField (p1 * p2)

-- TODO: move this elsewhere.
data SmallField
instance Finite SmallField where
    order = 7
instance Prime SmallField

c :: forall a . (FiniteField a) => Integer -> a
c x = foldl (+) zero $ map (const one) [1..x]

-- f = x^3 + 3 x + 5
f :: forall a . (FiniteField a) => a -> a
f x = x ^ (2 :: Integer) + c 3 * x + c 5

main :: IO ()
main = do
    let r = r1csCompile f :: R1CS (Zp SmallField)
        x = toZp 3 :: Zp SmallField
    r1csPrint r x
    print (f x :: Zp SmallField)

    print @String "Success!"