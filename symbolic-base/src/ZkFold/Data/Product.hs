{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.Product where

import Data.Function ((.))
import Data.Tuple (fst, snd, uncurry)
import GHC.Generics ((:*:) (..))

fromPair :: (f a, g a) -> (f :*: g) a
fromPair = uncurry (:*:)

uncurryP :: (f a -> g a -> b) -> (f :*: g) a -> b
uncurryP f (x :*: y) = f x y

toPair :: (f :*: g) a -> (f a, g a)
toPair = uncurryP (,)

fstP :: (f :*: g) a -> f a
fstP = fst . toPair

sndP :: (f :*: g) a -> g a
sndP = snd . toPair
