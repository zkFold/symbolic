{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- This is what our life will look like from now on if we keep using NumberOfRegisters
{-# OPTIONS_GHC -freduction-depth=0 #-}

module ZkFold.Symbolic.Data.UTCTime where

import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat)
import Prelude hiding (Bool, Eq, Ord)
import qualified Prelude as Haskell

import ZkFold.Algebra.Class (FromConstant)
import ZkFold.Data.Eq (Eq)
import ZkFold.Data.HFunctor.Classes (HEq)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Combinators (Ceil, GetRegisterSize, KnownRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.Ord (Ord)
import ZkFold.Symbolic.Data.UInt

newtype UTCTime c = UTCTime (UInt 11 Auto c)

deriving newtype instance HEq c => Haskell.Eq (UTCTime c)

deriving newtype instance (Symbolic c, KnownRegisters c 11 Auto) => Eq (UTCTime c)

deriving newtype instance (Symbolic c, KnownRegisters c 11 Auto) => SymbolicData (UTCTime c)

deriving newtype instance
  (Symbolic c, KnownRegisters c 11 Auto, regSize ~ GetRegisterSize (BaseField c) 11 Auto, KnownNat (Ceil regSize OrdWord))
  => Ord (UTCTime c)

deriving newtype instance FromConstant Natural (UInt 11 Auto c) => FromConstant Natural (UTCTime c)
