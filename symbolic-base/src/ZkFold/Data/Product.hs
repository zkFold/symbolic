{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.Product where

import           Data.Function (const, id)
import           GHC.Generics  ((:*:) (..))

uncurryP :: (f a -> g a -> b) -> (f :*: g) a -> b
uncurryP f (x :*: y) = f x y

fstP :: (f :*: g) a -> f a
fstP = uncurryP const

sndP :: (f :*: g) a -> g a
sndP = uncurryP (const id)
