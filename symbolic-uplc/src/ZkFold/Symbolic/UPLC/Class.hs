{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.UPLC.Class (
  IsData (..),
  Sym,
  ExValue (..),
  ExList (..),
  BLS12_381_G2_Point (..),
  BLS12_381_GT (..),
) where

import Data.Maybe (Maybe (..))
import Data.Typeable (Typeable)
import GHC.Generics (U1, (:*:) (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Base, BLS12_381_Scalar)
import ZkFold.Algebra.EllipticCurve.Class (Weierstrass (..))
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic (BaseField))
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.EllipticCurve.BLS12_381
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.Int
import ZkFold.Symbolic.Data.List qualified as L
import ZkFold.Symbolic.Data.UInt (OrdWord)
import ZkFold.Symbolic.Data.VarByteString
import ZkFold.Symbolic.Fold (SymbolicFold)

import ZkFold.Symbolic.UPLC.Constants
import ZkFold.Symbolic.UPLC.Data qualified as Symbolic
import ZkFold.UPLC.BuiltinType

-- | Class of Symbolic datatypes used inside Converter.
-- Each instance enforces a one-to-one correspondence between some 'BuiltinType'
-- and its interpretation as a Symbolic datatype in arbitrary context 'c'.
class (Typeable v, SymbolicData v) => IsData (t :: BuiltinType) v | t -> v, v -> t where
  asPair :: HasRep v c => v c -> Maybe (ExValue c, ExValue c)
  asPair _ = Nothing
  asList :: HasRep v c => v c -> Maybe (ExList c)
  asList _ = Nothing
  asData :: HasRep v c => v c -> Maybe (Symbolic.Data c)
  asData _ = Nothing

-- | Existential wrapper around list of 'IsData' Symbolic types.
data ExList c = forall t v. (IsData t v, HasRep v c) => ExList (L.List v c)

-- | Existential wrapper around 'IsData' Symbolic types.
data ExValue c = forall t v. (IsData t v, HasRep v c) => ExValue (v c)

-- | We can evaluate UPLC terms in arbitrary 'Symbolic' context as long as
-- it is also 'Typeable'.
type Sym c =
  ( SymbolicFold c
  , Typeable c
  , Symbolic.KnownData c
  , KnownRegisters c IntLength IntRegSize
  , KnownRegisters c BSLength Auto
  , KnownRegisters c (NumberOfBits (BaseField c)) Auto
  , KnownNat (GetRegisterSize (BaseField c) IntLength IntRegSize)
  , KnownNat (GetRegisterSize (BaseField c) IntLength IntRegSize `Ceil` OrdWord)
  , KnownNat (GetRegisterSize (BaseField c) BSLength Auto `Ceil` OrdWord)
  , KnownNat (GetRegisterSize (BaseField c) (NumberOfBits (BaseField c)) Auto `Ceil` OrdWord)
  , KnownFFA BLS12_381_Base Auto c
  , KnownFFA BLS12_381_Scalar Auto c
  )

instance IsData BTInteger (Int IntLength Auto)

instance IsData BTByteString (VarByteString BSLength)

instance IsData BTString (VarByteString StrLength)

instance IsData BTBool Bool

instance IsData BTUnit U1

instance IsData BTData Symbolic.Data where
  asData = Just

instance IsData t v => IsData (BTList t) (L.List v) where
  asList l = Just (ExList l)

instance (IsData t v, IsData t' v') => IsData (BTPair t t') (v :*: v') where
  asPair (p :*: q) = Just (ExValue p, ExValue q)

instance IsData BTBLSG1 BLS12_381_G1_Point

newtype BLS12_381_G2_Point c = MkG2 {runG2 :: BLS12_381_G1_Point c}
-- ^ TODO: Replace with proper G2 impl once it's done

deriving newtype instance SymbolicData BLS12_381_G2_Point

deriving newtype instance Sym c => Eq (BLS12_381_G2_Point c)

deriving newtype instance (Sym c, FromConstant k (FFA BLS12_381_Scalar Auto c)) => Scale k (BLS12_381_G2_Point c)

deriving newtype instance Sym c => AdditiveSemigroup (BLS12_381_G2_Point c)

deriving newtype instance Sym c => Zero (BLS12_381_G2_Point c)

deriving newtype instance Sym c => AdditiveMonoid (BLS12_381_G2_Point c)

deriving newtype instance Sym c => AdditiveGroup (BLS12_381_G2_Point c)

instance IsData BTBLSG2 BLS12_381_G2_Point

newtype BLS12_381_GT c = MkGT {runGT :: BLS12_381_G1_Point c}
-- ^ TODO: Replace with proper GT impl once it's done

deriving newtype instance SymbolicData BLS12_381_GT

deriving newtype instance Sym c => AdditiveSemigroup (BLS12_381_GT c)

instance IsData BTBLSMLResult BLS12_381_GT
