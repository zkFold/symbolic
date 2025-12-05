{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.UInt where

import qualified Data.Aeson as Aeson
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Kind (Type)
import Data.List (iterate)
import Data.Tuple (snd)
import Data.Type.Bool (If)
import Data.Type.Ord (type (<=?))
import Data.Word (Word64)
import GHC.Err (error)
import GHC.Real (toInteger)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq
import ZkFold.Prelude ((!!))
import ZkFold.Symbolic.Class (Arithmetic, Symbolic)
import ZkFold.Symbolic.Data.Bool (assert)
import ZkFold.Symbolic.Data.ByteString hiding (resize)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import ZkFold.Symbolic.Data.Input (SymbolicInput (..))
import ZkFold.Symbolic.Data.Ord
import ZkFold.Symbolic.Data.UIntData
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

newtype SizeHint = SizeHint Natural

type family GetRegisterSize n h m where
  GetRegisterSize n ('SizeHint h) m =
    -- 'else' branch is the biggest solution of a system
    -- 1. x = h * k
    -- 2. 2 x + ceil(n / x) <= m
    If (2 * n <=? m) n (h * Div (m + Sqrt (m ^ 2 - 8 * n)) (4 * h))

type RegisterSize (n :: Natural) (h :: SizeHint) (c :: Type) =
  GetRegisterSize n h (Log2 (Order c))

newtype UInt (n :: Natural) (h :: SizeHint) c = UInt
  {uintData :: UIntData n (RegisterSize n h c) c}
  deriving newtype (Collect (ConstrainedDatum c), Eq)

type KnownUInt n h c = KnownUIntData n (RegisterSize n h c)

instance SymbolicData (UInt n h) where
  type Layout (UInt n h) c = Layout (UIntData n (RegisterSize n h c)) c
  type HasRep (UInt n h) c = KnownUInt n h c
  toLayout = toLayout . uintData
  fromLayout = UInt . fromLayout
  interpolate c = UInt . interpolate c . fmap (uintData <$>)

instance SymbolicInput (UInt n h) where
  isValid = isValid . uintData

instance {-# OVERLAPPING #-} FromConstant (UInt n h c) (UInt n h c)

instance {-# OVERLAPPING #-} (KnownUInt n h c, Symbolic c) => Scale (UInt n h c) (UInt n h c)

deriving newtype instance (KnownUInt n h c, Symbolic c) => Ord (UInt n h c)

deriving newtype instance
  FromConstant a (UIntData n (RegisterSize n h c) c)
  => FromConstant a (UInt n h c)

deriving newtype instance
  (KnownUInt n h a, Arithmetic a) => ToConstant (UInt n h a)

deriving newtype instance
  Scale a (UIntData n (RegisterSize n h c) c) => Scale a (UInt n h c)

deriving newtype instance
  (KnownUInt n h c, Symbolic c) => AdditiveSemigroup (UInt n h c)

deriving newtype instance (KnownUInt n h c, Symbolic c) => Zero (UInt n h c)

instance (KnownUInt n h c, Symbolic c) => AdditiveMonoid (UInt n h c)

deriving newtype instance
  (KnownUInt n h c, Symbolic c) => AdditiveGroup (UInt n h c)

deriving newtype instance
  (KnownUInt n h c, Symbolic c) => MultiplicativeSemigroup (UInt n h c)

deriving newtype instance
  (KnownUInt n h c, Symbolic c) => MultiplicativeMonoid (UInt n h c)

instance (KnownUInt n h c, Symbolic c) => Semiring (UInt n h c)

instance (KnownUInt n h c, Symbolic c) => Ring (UInt n h c)

deriving newtype instance
  (KnownUInt n h c, Symbolic c) => SemiEuclidean (UInt n h c)

deriving newtype instance
  (KnownUInt n h c, Symbolic c) => Euclidean (UInt n h c)

instance (KnownUInt 64 h c, Symbolic c) => FromConstant Word64 (UInt 64 h c) where
  fromConstant = fromConstant . toInteger

instance (KnownUInt n h c, Symbolic c) => Aeson.FromJSON (UInt n h c) where
  parseJSON = fmap strictNatToUInt . Aeson.parseJSON @Natural

instance (KnownUInt n h a, Arithmetic a) => Aeson.ToJSON (UInt n h a) where
  toJSON = Aeson.toJSON . toConstant

instance MultiplicativeMonoid (UInt n h c) => Exponent (UInt n h c) Natural where
  (^) = natPow

resizeUInt :: UInt m g c -> UInt n h c
resizeUInt = UInt . resizeUIntData . uintData

beBSToUInt :: (KnownUInt n h c, Symbolic c) => ByteString n c -> UInt n h c
beBSToUInt = UInt . beBSToUIntData

uintToBSbe :: (KnownUInt n h c, Symbolic c) => UInt n h c -> ByteString n c
uintToBSbe = uintDataToBSbe . uintData

feToUInt
  :: (KnownUInt (NumberOfBits c) h c, Symbolic c)
  => FieldElement c -> UInt (NumberOfBits c) h c
feToUInt = beBSToUInt . feToBSbe

uintToFE
  :: (KnownNat (RegisterSize (NumberOfBits c) h c), Symbolic c)
  => UInt (NumberOfBits c) h c -> FieldElement c
uintToFE = uintDataToFE . uintData

strictNatToUInt
  :: forall n h c. (KnownUInt n h c, Symbolic c) => Natural -> UInt n h c
strictNatToUInt x =
  assert (\_ -> fromConstant (x < 2 ^ value @n)) (fromConstant x)

strictZpToUInt :: (KnownUInt n h c, Symbolic c) => Zp p -> UInt n h c
strictZpToUInt = strictNatToUInt . toConstant

strictFEToUInt :: FieldElement c -> UInt n h c
strictFEToUInt = error "TODO"

strictAdd :: UInt n h c -> UInt n h c -> UInt n h c
strictAdd = error "TODO"

strictSub :: UInt n h c -> UInt n h c -> UInt n h c
strictSub = error "TODO"

strictMul :: UInt n h c -> UInt n h c -> UInt n h c
strictMul = error "TODO"

--------------------------------------------------------------------------------

-- | Calculate @a * b `divMod` m@ using less constraints than would've been
-- required by these operations used consequently
productMod
  :: forall c n r
   . UInt n r c
  -> UInt n r c
  -> UInt n r c
  -> (UInt n r c, UInt n r c)
productMod (UInt _aRegs) (UInt _bRegs) (UInt _mRegs) = error "TODO"

bitsPow
  :: forall c n p r
   . Symbolic c
  => KnownNat n
  => KnownNat p
  => Natural
  -> ByteString p c
  -> UInt n r c
  -> UInt n r c
  -> UInt n r c
  -> UInt n r c
bitsPow 0 _ res _ _ = res
bitsPow b bits res n m = bitsPow (b -! 1) bits newRes sq m
 where
  sq = snd $ productMod n n m
  newRes = ifThenElse (isSet bits (b -! 1)) (snd $ productMod res n m) res

-- | @expMod n pow modulus@ calculates @n^pow % modulus@
-- where all values are arithmetised
expMod
  :: forall c n p m r
   . Symbolic c
  => KnownUInt p r c
  => KnownUInt (2 * m) r c
  => UInt n r c
  -> UInt p r c
  -> UInt m r c
  -> UInt m r c
expMod n pow modulus = resizeUInt result
 where
  m' :: UInt (2 * m) r c
  m' = resizeUInt modulus

  result :: UInt (2 * m) r c
  result = bitsPow (value @p) (uintToBSbe pow) one (resizeUInt n `mod` m') m'

-- | n ^ 65537 `mod` modulus
exp65537Mod
  :: forall c n m r
   . UInt n r c
  -> UInt m r c
  -> UInt m r c
-- exp65537Mod n modulus = resize $ Haskell.snd $ productMod n' n' m'
exp65537Mod n modulus = resizeUInt $ snd $ productMod sq_2_16 n' m'
 where
  m' :: UInt (2 * m) r c
  m' = resizeUInt modulus

  n' :: UInt (2 * m) r c
  n' = resizeUInt n

  sq_2_16 = iterate (\x -> snd $ productMod x x m') n' !! (16 :: Natural)
