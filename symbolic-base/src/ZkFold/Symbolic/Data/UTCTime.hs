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
import ZkFold.Data.Collect (Collect)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData)
import ZkFold.Symbolic.Data.Ord (Ord)
import ZkFold.Symbolic.Data.UInt
import ZkFold.Symbolic.Data.Unconstrained (ConstrainedDatum)

newtype UTCTime c = UTCTime (UInt 11 Auto c)
  deriving newtype
    ( Collect (ConstrainedDatum c)
    , Eq
    , FromConstant Natural
    , Haskell.Eq
    , SymbolicData
    )

deriving newtype instance
  ( Symbolic c
  , KnownRegisters c 11 Auto
  , regSize ~ GetRegisterSize c 11 Auto
  , KnownNat (Ceil regSize OrdWord)
  )
  => Ord (UTCTime c)
