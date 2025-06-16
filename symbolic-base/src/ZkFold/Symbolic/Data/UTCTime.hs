{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.UTCTime where

import           GHC.Natural                      (Natural)
import           Prelude                          hiding (Bool, Eq, Ord)
import qualified Prelude                          as Haskell

import           ZkFold.Algebra.Class             (FromConstant)
import           ZkFold.Data.HFunctor.Classes     (HEq)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (IsValidRegister)
import           ZkFold.Symbolic.Data.Conditional (Conditional)
import           ZkFold.Symbolic.Data.Eq          (Eq)
import           ZkFold.Symbolic.Data.Ord         (Ord)
import           ZkFold.Symbolic.Data.UInt

newtype UTCTime c = UTCTime (UInt 11 1 c)

deriving newtype instance HEq c => Haskell.Eq (UTCTime c)
deriving newtype instance (Symbolic c, IsValidRegister 11 1 c) => Conditional (Bool c) (UTCTime c)
deriving newtype instance (Symbolic c, IsValidRegister 11 1 c) => Eq (UTCTime c)
deriving newtype instance (Symbolic c, IsValidRegister 11 1 c) => SymbolicData (UTCTime c)
deriving newtype instance (Symbolic c, IsValidRegister 11 1 c) => Ord (UTCTime c)

deriving newtype instance FromConstant Natural (UInt 11 1 c) => FromConstant Natural (UTCTime c)
