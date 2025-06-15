{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Payloaded where

import           GHC.Generics                     (Generic1)

import           ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input (SymbolicInput)

newtype Payloaded f c = Payloaded { runPayloaded :: Wit f c }
  deriving Generic1

instance PayloadFunctor f => SymbolicData (Payloaded f)

instance PayloadFunctor f => SymbolicInput (Payloaded f)
