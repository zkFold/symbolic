{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -freduction-depth=0 #-} -- This is what our life will look like from now on if we keep using NumberOfRegisters

module ZkFold.Symbolic.Data.UTCTime where

import           GHC.Natural                      (Natural)
import           Prelude                          hiding (Bool, Eq, Ord)
import qualified Prelude                          as Haskell

import           ZkFold.Algebra.Class             (FromConstant)
import           ZkFold.Data.HFunctor.Classes     (HEq)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (RegisterSize (..))
import           ZkFold.Symbolic.Data.UInt

newtype UTCTime c = UTCTime (UInt 11 Auto c)

deriving newtype instance HEq c => Haskell.Eq (UTCTime c)
deriving newtype instance SymbolicData (UTCTime)

deriving newtype instance FromConstant Natural (UInt 11 Auto c) => FromConstant Natural (UTCTime c)
