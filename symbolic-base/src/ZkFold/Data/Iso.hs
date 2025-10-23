{-# LANGUAGE UndecidableSuperClasses #-}

module ZkFold.Data.Iso where

class Iso b a => Iso a b where
  from :: a -> b
