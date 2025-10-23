{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Testing where

import Control.Applicative (Applicative (liftA2))
import qualified Data.Bool as Haskell
import Data.Function (flip, ($), (.))
import Data.List ((++))
import Data.Type.Equality (type (~))
import Data.Typeable (Typeable, typeRep)
import GHC.Integer (Integer)
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck hiding (scale, (===))
import Text.Show (Show, show)

import ZkFold.Algebra.Class
import ZkFold.Data.Eq (BooleanOf, Eq, (==))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicData (HasRep))
import ZkFold.Symbolic.Data.Input (SymbolicInput (isValid))

instance Arithmetic a => Testable (Bool a) where
  property = property . toConstant

type TestResult a =
  (Show a, Eq a, ToConstant (BooleanOf a), Const (BooleanOf a) ~ Haskell.Bool)

infix 4 ===

(===) :: TestResult a => a -> a -> Property
x === y = counterexample (show x ++ interpret res ++ show y) res
 where
  res = toConstant (x == y)
  interpret Haskell.True = " == "
  interpret Haskell.False = " /= "

infixr 0 ==>

(==>)
  :: (ToConstant b, Const b ~ Haskell.Bool, Testable prop)
  => b -> prop -> Property
(toConstant -> Haskell.False) ==> prop = property prop
_ ==> _ = property Discard

type Checkable a c = (SymbolicInput a, HasRep a c, Arithmetic c)

validity :: (Checkable a c, Show (a c)) => a c -> Property
validity x =
  counterexample ("data is not valid: " ++ show x) $ toConstant (isValid x)

-- | Checks that for Symbolic datatype:
-- * @toConstant . fromConstant == id@;
-- * @fromConstant . toConstant == id@;
-- * @isValid == const true@.
embedding
  :: (Checkable a c, a c ~ ac, Show ac, Eq ac, BooleanOf ac ~ Bool c)
  => (ToConstant ac, Const ac ~ cac, FromConstant cac ac, TestResult cac)
  => Gen ac -> Gen cac -> Property
embedding (gx :: Gen ac) gcx =
  forAll gcx (\cx -> toConstant @ac (fromConstant cx) === cx)
    .&&. forAll gx \x -> fromConstant (toConstant x) === x .&&. validity x

type TestInput a = (Show a, ToConstant a)

type TestOutput a = (ToConstant a, TestResult (Const a))

-- | @commutative1 _ f f'@ checks that the following holds:
-- @
--          f
-- Const a --> Const b
--    ^           ^
--    |     f'    |
--    a --------> b
-- @
-- where vertical arrows are provided by 'toConstant'.
--
-- This also means that if 'embedding' for @b@ holds, the following is also true:
-- * @toConstant . f' . fromConstant == f@ (@f'@ "models" @f@);
-- * @isValid . f' == const true@ (@f'@ "preserves validity").
commutative1
  :: (TestInput a, TestOutput b)
  => (Const a -> Const b) -> (a -> b) -> Gen a -> Property
commutative1 f f' g = forAll g \x -> f (toConstant x) === toConstant (f' x)

commutative0 :: TestOutput a => Const a -> a -> Property
commutative0 c c' = c === toConstant c'

-- | Same as 'commutative1', but for binary functions.
commutative2
  :: (TestInput a, TestInput b, TestOutput c)
  => (Const a -> Const b -> Const c)
  -> (a -> b -> c)
  -> Gen (a, b)
  -> Property
commutative2 f f' g = forAll g \(x, y) ->
  f (toConstant x) (toConstant y) === toConstant (f' x y)

-- | Same as 'commutative1', but for parameterized functions.
commutativePar
  :: (Show p, TestInput a, TestOutput b)
  => (p -> Const a -> Const b) -> (p -> a -> b) -> Gen (p, a) -> Property
commutativePar f f' g = forAll g \(p, x) ->
  f p (toConstant x) === toConstant (f' p x)

type TestableModel a c =
  ( Checkable a c
  , TestResult (a c)
  , BooleanOf (a c) ~ Bool c
  , ToConstant (a c)
  , FromConstant (Const (a c)) (a c)
  , TestResult (Const (a c))
  , Typeable a
  , Typeable c
  , Typeable (Const (a c))
  )

-- | Tests that Symbolic datatype @a@ correctly models ring operations of its
-- 'Const' in context @c@.
ringModelAxioms
  :: (TestableModel a c, Ring (Const (a c)), Ring (a c), Arbitrary Natural)
  => Gen (a c) -> Gen (Const (a c)) -> Spec
ringModelAxioms (ga :: Gen (a c)) gc =
  describe
    (show (typeRep ga) ++ " as model of ring " ++ show (typeRep gc))
    do
      it "embeds" (embedding ga gc)
      let pr :: Applicative f => f x -> f y -> f (x, y)
          pr = liftA2 (,)
          gg = pr ga ga
      it "has addition" $ commutative2 (+) (+) gg
      it "has zero" $ commutative0 @(a c) zero zero
      it "has scaleNat" $
        commutativePar @Natural scale scale (pr arbitrary ga)
      it "has negation" (commutative1 negate negate ga)
      it "has subtraction" $ commutative2 (-) (-) gg
      it "has scaleInt" $
        commutativePar @Integer scale scale (pr arbitrary ga)
      it "has multiplication" $ commutative2 (*) (*) gg
      it "has exponent" $
        commutativePar @Natural (flip (^)) (flip (^)) (pr arbitrary ga)
