{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var where

import           Control.Applicative                                (Applicative, pure, (<*>))
import           Control.DeepSeq                                    (NFData)
import           Control.Monad                                      (Monad, ap, (>>=))
import           Data.Aeson                                         (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Bifunctor                                     (first)
import           Data.Bifunctor.TH                                  (deriveBifunctor)
import           Data.Binary                                        (Binary)
import           Data.ByteString                                    (ByteString)
import           Data.Function                                      ((.))
import           Data.Functor                                       (Functor)
import           GHC.Generics                                       (Generic)
import           GHC.Show                                           (Show)
import           Prelude                                            (Eq, Ord)

import           ZkFold.Algebra.Class
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Witness (WitnessF)
import           ZkFold.Symbolic.MonadCircuit                       (WitnessBuilder (..))

data LinVar a v = LinVar a v a | ConstVar a
    deriving
        ( Generic, Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey
        , Show, Eq, Ord, NFData, Functor)

$(deriveBifunctor ''LinVar)

(.+) :: AdditiveSemigroup a => a -> LinVar a v -> LinVar a v
c .+ LinVar k x b = LinVar k x (c + b)
c .+ ConstVar d   = ConstVar (c + d)

evalVar :: Algebra a b => (v -> b) -> LinVar a v -> b
evalVar f (LinVar k x b) = scale k (f x) + fromConstant b
evalVar _ (ConstVar c)   = fromConstant c

instance Scale k a => Scale k (LinVar a v) where
    scale = first . scale

instance FromConstant c a => FromConstant c (LinVar a v) where
    fromConstant = ConstVar . fromConstant

instance Semiring a => Applicative (LinVar a) where
    pure x = LinVar one x zero
    (<*>) = ap

instance Semiring a => Monad (LinVar a) where
    LinVar k x b >>= f = b .+ scale k (f x)
    ConstVar c >>= _   = ConstVar c

data NewVar
    = EqVar ByteString
    | FoldLVar ByteString ByteString
    | FoldPVar ByteString ByteString
    deriving
        ( Generic, Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey
        , Show, Eq, Ord, NFData)

type CircuitWitness a = WitnessF a NewVar

type Var a = LinVar a NewVar

toVar :: Semiring a => NewVar -> Var a
toVar = pure

instance Finite a => WitnessBuilder (Var a) (CircuitWitness a) where at = evalVar pure
