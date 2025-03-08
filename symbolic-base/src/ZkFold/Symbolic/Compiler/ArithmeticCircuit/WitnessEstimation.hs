{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.WitnessEstimation where



import           Control.Applicative                            ()
import           Data.Functor.Rep                               (Rep)
import           GHC.Generics                                   (Generic)
import           GHC.Integer                                    (Integer)
import           GHC.Natural                                    (Natural)
import           Prelude                                        (Eq, Maybe (..), ($), (.), (==))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.ByteString                    ()
import           ZkFold.Symbolic.Class                          (Arithmetic)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var
import           ZkFold.Symbolic.MonadCircuit                   (ResidueField (..))



data UVar a i
  = ConstUVar a
  | LinUVar a (SysVar i) a
  | More
  deriving Generic

instance FromConstant a (UVar a i) where
    fromConstant = ConstUVar
instance FromConstant Natural a => FromConstant Natural (UVar a i) where fromConstant = ConstUVar . fromConstant
instance FromConstant Integer a => FromConstant Integer (UVar a i) where fromConstant = ConstUVar . fromConstant

instance (Semiring a, Eq a) => Scale a (UVar a i) where
  scale k (ConstUVar c) = ConstUVar $ k * c
  scale k (LinUVar a x b) = if k == zero
    then ConstUVar zero
    else LinUVar (k * a) x (k * b)
  scale k More = if k == zero
    then ConstUVar zero
    else More

instance (Semiring a, Eq a) => Scale Natural (UVar a i) where scale k = scale (fromConstant k :: a)
instance (Semiring a, Eq a, FromConstant Integer a) => Scale Integer (UVar a i) where scale k = scale (fromConstant k :: a)

instance MultiplicativeMonoid a => Exponent (UVar a i) Natural where
  (ConstUVar c) ^ n = ConstUVar $ c ^ n
  v ^ 1             = v
  _ ^ 0             = ConstUVar one
  _ ^ _             = More

instance (Exponent a Integer, MultiplicativeMonoid a) => Exponent (UVar a i) Integer where
  (ConstUVar c) ^ n   = ConstUVar $ c ^ n
  (LinUVar k x b) ^ 1 = LinUVar k x b
  _ ^ 0               = ConstUVar one
  _ ^ _               = More

instance (AdditiveMonoid a, Eq a, Eq (Rep i)) => AdditiveSemigroup (UVar a i) where
  ConstUVar c + x = c .+ x
  x + ConstUVar c = c .+ x
  LinUVar k1 x1 b1 + (LinUVar k2 x2 b2) = if x1 == x2 then
    if k1 + k2 == zero
      then ConstUVar $ b1 + b2
      else LinUVar (k1 + k2) x1 (b1 + b2)
    else More
  _ + _ = More

(.+) :: AdditiveSemigroup a => a -> UVar a i -> UVar a i
c1 .+ ConstUVar c2 = ConstUVar $ c1 + c2
c .+ LinUVar k x b = LinUVar k x (b + c)
_ .+ More          = More

instance (Semiring a, Eq a, Eq (Rep i)) => AdditiveMonoid (UVar a i) where
  zero = ConstUVar zero

instance (Ring a, Eq a, Eq (Rep i)) => AdditiveGroup (UVar a i) where
  negate (ConstUVar c)   = ConstUVar (negate c)
  negate (LinUVar k x b) = LinUVar (negate k) x (negate b)
  negate More            = More

instance (Semiring a, Eq a) => MultiplicativeSemigroup (UVar a i) where
  ConstUVar c * x = scale c x
  x * ConstUVar c = scale c x
  _ * _           = More

instance (Semiring a, Eq a) => MultiplicativeMonoid (UVar a i) where
  one = ConstUVar one

instance (Semiring a, Eq a, Eq (Rep i)) => Semiring (UVar a i)

instance (Ring a, Eq a, Eq (Rep i)) => Ring (UVar a i)

instance (Field a, Eq a, Eq (Rep i)) => Field (UVar a i) where
  finv (ConstUVar c) = ConstUVar $ finv c
  finv _             = More

instance Finite a => Finite (UVar a i) where type Order (UVar a i) = Order a

instance (Arithmetic a, Eq (Rep i)) => ResidueField (UVar a i) where
  type IntegralOf (UVar a i) = Maybe Integer
  fromIntegral (Just x) = ConstUVar (fromConstant x)
  fromIntegral Nothing  = More
  toIntegral (ConstUVar c) = Just (toIntegral c)
  toIntegral _             = Nothing
