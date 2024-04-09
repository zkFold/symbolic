{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.UTCTime where

import ZkFold.Symbolic.Compiler.Arithmetizable
import ZkFold.Symbolic.Data.UInt
import Prelude

newtype UTCTime a = UTCTime (UInt 11 a)
  deriving (Eq)

instance (Arithmetizable a (UInt 11 x)) => Arithmetizable a (UTCTime x) where
  arithmetize (UTCTime x) = arithmetize x
  restore tx = UTCTime $ restore tx
  typeSize = typeSize @a @(UInt 11 x)
