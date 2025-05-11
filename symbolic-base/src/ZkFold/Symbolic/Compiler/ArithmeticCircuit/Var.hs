{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var where

import           Control.Applicative                                (Applicative (..))
import           Control.DeepSeq                                    (NFData)
import           Data.Aeson                                         (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Binary                                        (Binary)
import           Data.ByteString                                    (ByteString)
import           GHC.Generics                                       (Generic)
import           GHC.Show                                           (Show)
import           Prelude                                            (Eq, Ord)

import           ZkFold.Algebra.Class
import           ZkFold.Data.ByteString                             ()
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Witness (WitnessF)
import           ZkFold.Symbolic.MonadCircuit                       (Witness (..))

data NewVar
    = EqVar ByteString
    | FoldLVar ByteString ByteString
    | FoldPVar ByteString ByteString
    deriving
        ( Generic, Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey
        , Show, Eq, Ord, NFData)

data Var a = LinVar a NewVar a | ConstVar a
    deriving
        ( Generic, Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey
        , Show, Eq, Ord, NFData)

toVar :: Semiring a => NewVar -> Var a
toVar x = LinVar one x zero

instance FromConstant a (Var a) where fromConstant = ConstVar

type CircuitWitness a = WitnessF a NewVar

instance Finite a => Witness (Var a) (CircuitWitness a) where
    at (ConstVar cV)   = fromConstant cV
    at (LinVar k sV b) = fromConstant k * pure sV + fromConstant b
