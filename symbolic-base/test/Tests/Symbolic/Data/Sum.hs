{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Sum (specSum) where

import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import Data.Void (absurd)
import GHC.Generics (Generic, V1, (:+:) (..), Rep1, Generic1, U1)
import Test.Hspec (Spec, describe)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Instances ()
import Text.Show (Show, show)

import Tests.Common (it, typeAt)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Class (Arithmetic, BaseField, Symbolic)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Sum
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Interpreter (Interpreter)
import Control.Applicative (pure)

instance {-# OVERLAPPING #-} Q.Arbitrary (f a) => Q.Arbitrary ((f :+: V1) a) where
  arbitrary = L1 <$> Q.arbitrary

instance (Q.Arbitrary (f a), Q.Arbitrary (g a)) => Q.Arbitrary ((f :+: g) a) where
  arbitrary = Q.oneof [L1 <$> Q.arbitrary, R1 <$> Q.arbitrary]

instance Q.Function (V1 a) where
  function = Q.functionMap (\case {}) absurd

instance Q.Function (U1 a)

instance (Q.Function (f a), Q.Function (g a)) => Q.Function ((f :+: g) a)

instance Arithmetic a => Q.Function (FieldElement (Interpreter a)) where
  function = Q.functionMap (toConstant . toConstant) fromConstant

instance (Arithmetic a, KnownNat n) => Q.Function (ByteString n (Interpreter a)) where
  function = Q.functionMap toConstant fromConstant

instance
  (Arithmetic a, KnownNat n, KnownRegisterSize r)
  => Q.Function (UInt n r (Interpreter a))
  where
  function = Q.functionMap toConstant fromConstant

instance Q.CoArbitrary (V1 a) where
  coarbitrary x = case x of {}

instance Q.CoArbitrary (U1 a)

instance (Q.CoArbitrary (f a), Q.CoArbitrary (g a)) => Q.CoArbitrary ((f :+: g) a)

instance Arithmetic a => Q.CoArbitrary (FieldElement (Interpreter a)) where
  coarbitrary = Q.coarbitrary . toConstant . toConstant

instance Arithmetic a => Q.CoArbitrary (ByteString n (Interpreter a)) where
  coarbitrary = Q.coarbitrary . toConstant

instance
  (Arithmetic a, KnownNat n, KnownRegisterSize r)
  => Q.CoArbitrary (UInt n r (Interpreter a))
  where
  coarbitrary = Q.coarbitrary . toConstant

specOneOf'
  :: forall a ts
   . ( Arithmetic a
     , Show a
     , Typeable a
     , Q.Arbitrary a
     , Typeable ts
     , Embed ts (Interpreter a)
     , Show (Eithers ts (Interpreter a))
     , Q.Arbitrary (Eithers ts (Interpreter a))
     , Q.CoArbitrary (Eithers ts (Interpreter a))
     , Q.Function (Eithers ts (Interpreter a))
     )
  => Spec
specOneOf' = describe (show (typeAt @(OneOf ts (Interpreter a))) <> " spec") do
  it "preserves sum" \(Q.Fn f) e ->
    matchOneOf @FieldElement @ts @(Interpreter a) (embedOneOf e) f Q.=== f e

type TestFun t a = t (Interpreter a) -> FieldElement (Interpreter a)

specSumOf'
  :: forall a t
   . ( Arithmetic a
     , Show a
     , Typeable a
     , Q.Arbitrary a
     , Generic1 t
     , Show (t (Interpreter a))
     , Typeable t
     , Q.Arbitrary (t (Interpreter a))
     , Q.CoArbitrary (t (Interpreter a))
     , Q.Function (t (Interpreter a))
     , Injects (Rep1 t) (Interpreter a)
     )
  => Spec
specSumOf' = describe (show (typeAt @(Sum t (Interpreter a))) <> " spec") do
  it "preserves sum" \(Q.Fn (f :: TestFun t a)) t ->
    match (inject t) f Q.=== f t

specOneOf
  :: forall a
   . ( Arithmetic a
     , Show a
     , Typeable a
     , Q.Arbitrary a
     , KnownNat (NumberOfRegisters a 32 Auto)
     )
  => Spec
specOneOf = do
  specOneOf' @a @'[FieldElement]
  specOneOf' @a @'[U1, FieldElement]
  specOneOf' @a @'[FieldElement, ByteString 16]
  specOneOf' @a @'[ByteString 16, FieldElement, UInt 32 Auto]

newtype Only f c = Only { runOnly :: f c }
  deriving stock (Generic, Generic1, Show)
  deriving newtype (Q.Arbitrary, Q.CoArbitrary)
  deriving anyclass (Q.Function)

data Might b c = Indeed (b c) | None
  deriving stock (Generic, Generic1, Show)
  deriving anyclass (Q.CoArbitrary, Q.Function)

instance Q.Arbitrary (b c) => Q.Arbitrary (Might b c) where
  arbitrary = Q.oneof [Indeed <$> Q.arbitrary, pure None]

data OneOf3 c = BS (ByteString 16 c) | FE (FieldElement c) | UD (UInt 32 Auto c)
  deriving (Generic, Generic1, Show)

instance (Symbolic c, Q.Arbitrary (BaseField c)) => Q.Arbitrary (OneOf3 c) where
  arbitrary = Q.oneof [BS <$> Q.arbitrary, FE <$> Q.arbitrary, UD <$> Q.arbitrary]

instance Arithmetic a => Q.CoArbitrary (OneOf3 (Interpreter a))

instance Arithmetic a => Q.Function (OneOf3 (Interpreter a))

specSumOf
  :: forall a
   . (Arithmetic a, Show a, Typeable a, Q.Arbitrary a, KnownNat (NumberOfRegisters a 32 Auto)) => Spec
specSumOf = do
  specSumOf' @a @(Only FieldElement)
  specSumOf' @a @(Might FieldElement)
  specSumOf' @a @(FieldElement :+: ByteString 16)
  specSumOf' @a @OneOf3

specSum :: Spec
specSum = do
  specOneOf @(Zp BLS12_381_Scalar)
  specSumOf @(Zp BLS12_381_Scalar)
