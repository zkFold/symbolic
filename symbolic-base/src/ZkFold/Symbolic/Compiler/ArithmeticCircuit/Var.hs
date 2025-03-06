{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var where

import           Control.Applicative             ()
import           Control.DeepSeq                 (NFData)
import           Data.Aeson                      (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Binary                     (Binary)
import           Data.Bool                       (bool)
import           Data.ByteString                 (ByteString)
import           Data.Functor.Rep                (Rep, Representable, index, tabulate)
import           GHC.Generics                    (Generic)
import           GHC.Show                        (Show)
import           Prelude                         (Eq, Ord, error, (&&), (==))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.ByteString     ()
import           ZkFold.Symbolic.Class           (Arithmetic)

data NewVar
  = EqVar ByteString
  | FoldVar ByteString ByteString
  deriving
    ( Generic, Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey
    , Show, Eq, Ord, NFData)

data SysVar i
  = InVar (Rep i)
  | NewVar NewVar
  deriving (Generic)

imapSysVar ::
  (Representable i, Representable j) =>
  (forall x. j x -> i x) -> SysVar i -> SysVar j
imapSysVar f (InVar r)  = index (f (tabulate InVar)) r
imapSysVar _ (NewVar v) = NewVar v

instance Binary (Rep i) => Binary (SysVar i)
instance NFData (Rep i) => NFData (SysVar i)
instance FromJSON (Rep i) => FromJSON (SysVar i)
instance FromJSON (Rep i) => FromJSONKey (SysVar i)
instance ToJSON (Rep i) => ToJSON (SysVar i)
instance ToJSON (Rep i) => ToJSONKey (SysVar i)
deriving stock instance Show (Rep i) => Show (SysVar i)
deriving stock instance Eq (Rep i) => Eq (SysVar i)
deriving stock instance Ord (Rep i) => Ord (SysVar i)

data Var a i
  = LinVar a (SysVar i) a
  | ConstVar a
  deriving Generic

toVar :: Semiring a => SysVar i -> Var a i
toVar x = LinVar one x zero

fromVar :: Arithmetic a => Var a i -> SysVar i
fromVar (LinVar k x b) = bool (error "there are no SysVar inside Var") x (k == one && b == zero)
fromVar _              = error "there are no SysVar inside Var"

imapVar ::
  (Representable i, Representable j) =>
  (forall x. j x -> i x) -> Var a i -> Var a j
imapVar f (LinVar k x b) = LinVar k (imapSysVar f x) b
imapVar _ (ConstVar c)   = ConstVar c

deriving anyclass instance (Binary (Rep i), Binary a) => Binary (Var a i)
deriving anyclass instance (FromJSON (Rep i), FromJSON a) => FromJSON (Var a i)
deriving anyclass instance (FromJSON (Rep i), FromJSON a) => FromJSONKey (Var a i)
deriving anyclass instance (ToJSON (Rep i), ToJSON a) => ToJSONKey (Var a i)
deriving anyclass instance (ToJSON (Rep i), ToJSON a) => ToJSON (Var a i)
deriving stock instance (Show (Rep i), Show a) => Show (Var a i)
deriving stock instance (Eq (Rep i), Eq a) => Eq (Var a i)
deriving stock instance (Ord (Rep i), Ord a) => Ord (Var a i)
deriving instance (NFData (Rep i), NFData a) => NFData (Var a i)
instance FromConstant a (Var a i) where fromConstant = ConstVar
