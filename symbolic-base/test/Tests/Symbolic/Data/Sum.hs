{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Data.Sum (specSum) where

import Data.Either (Either (..))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Proxy (Proxy)
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic, Rep)
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

instance {-# OVERLAPPING #-} Q.Arbitrary a => Q.Arbitrary (Either a Void) where
  arbitrary = Left <$> Q.arbitrary

instance Arithmetic a => Q.Function (FieldElement (Interpreter a)) where
  function = Q.functionMap (toConstant . toConstant) fromConstant

instance (Arithmetic a, KnownNat n) => Q.Function (ByteString n (Interpreter a)) where
  function = Q.functionMap toConstant fromConstant

instance
  (Arithmetic a, KnownNat n, KnownRegisterSize r)
  => Q.Function (UInt n r (Interpreter a))
  where
  function = Q.functionMap toConstant fromConstant

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
   . ( Show a
     , Typeable a
     , Q.Arbitrary a
     , Typeable ts
     , Embed ts (Interpreter a)
     , Show (Eithers ts)
     , Q.Arbitrary (Eithers ts)
     , Q.CoArbitrary (Eithers ts)
     , Q.Function (Eithers ts)
     )
  => Spec
specOneOf' = describe (show (typeAt @(OneOf ts (Interpreter a))) <> " spec") do
  it "preserves sum" \(Q.Fn f) e ->
    matchOneOf @(FieldElement (Interpreter a)) @ts (embedOneOf e) f Q.=== f e

specSumOf'
  :: forall a t
   . ( Show a
     , Typeable a
     , Q.Arbitrary a
     , Generic t
     , Show t
     , Typeable t
     , Q.Arbitrary t
     , Q.CoArbitrary t
     , Q.Function t
     , Injects (Rep t) (Interpreter a)
     )
  => Spec
specSumOf' = describe (show (typeAt @(Sum t (Interpreter a))) <> " spec") do
  it "preserves sum" \(Q.Fn (f :: t -> FieldElement (Interpreter a))) t ->
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
  specOneOf' @a @'[FieldElement (Interpreter a)]
  specOneOf' @a @'[Proxy (Interpreter a), FieldElement (Interpreter a)]
  specOneOf' @a @'[FieldElement (Interpreter a), ByteString 16 (Interpreter a)]
  specOneOf' @a
    @'[ ByteString 16 (Interpreter a)
      , FieldElement (Interpreter a)
      , UInt 32 Auto (Interpreter a)
      ]

data OneOf3 c = BS (ByteString 16 c) | FE (FieldElement c) | UD (UInt 32 Auto c)
  deriving (Generic, Show)

instance (Symbolic c, Q.Arbitrary (BaseField c)) => Q.Arbitrary (OneOf3 c) where
  arbitrary = Q.oneof [BS <$> Q.arbitrary, FE <$> Q.arbitrary, UD <$> Q.arbitrary]

instance Arithmetic a => Q.CoArbitrary (OneOf3 (Interpreter a))

instance Arithmetic a => Q.Function (OneOf3 (Interpreter a))

specSumOf
  :: forall a
   . (Arithmetic a, Show a, Typeable a, Q.Arbitrary a, KnownNat (NumberOfRegisters a 32 Auto)) => Spec
specSumOf = do
  specSumOf' @a @(FieldElement (Interpreter a))
  specSumOf' @a @(Maybe (FieldElement (Interpreter a)))
  specSumOf' @a @(Either (FieldElement (Interpreter a)) (ByteString 16 (Interpreter a)))
  specSumOf' @a @(OneOf3 (Interpreter a))

specSum :: Spec
specSum = do
  specOneOf @(Zp BLS12_381_Scalar)
  specSumOf @(Zp BLS12_381_Scalar)
