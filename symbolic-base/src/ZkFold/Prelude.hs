{-# LANGUAGE CPP #-}

module ZkFold.Prelude (module ZkFold.Prelude, module Data.List) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Functor.Rep (Representable, tabulate)
import Data.List (genericIndex)
#if __GLASGOW_HASKELL__ < 912
import           Data.List            (foldl')
#endif
#if __GLASGOW_HASKELL__ >= 910
import           Data.List            (unsnoc)
#endif
import Data.Map (Map, lookup)
import qualified Data.Vector as V
import GHC.Num (Natural, integerToNatural)
import GHC.Stack (HasCallStack)
import Test.QuickCheck (Gen, chooseInteger, elements)
import Prelude hiding (drop, iterate, lookup, readFile, replicate, take, writeFile, (!!))

log2ceiling :: (Integral a, Integral b) => a -> b
log2ceiling = ceiling @Double . logBase 2 . fromIntegral

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

length :: Foldable t => t a -> Natural
length = foldl' (\c _ -> c + 1) 0

take :: HasCallStack => Natural -> [a] -> [a]
take 0 _ = []
take n (x : xs) = x : take (n - 1) xs
take _ [] = error "ZkFold.Prelude.take: empty list"

drop :: Natural -> [a] -> [a]
drop 0 xs = xs
drop n (_ : xs) = drop (n - 1) xs
drop _ [] = error "ZkFold.Prelude.drop: empty list"

splitAt :: Natural -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

iterate' :: (a -> a) -> a -> [a]
iterate' f x =
  let x' = f x
   in x' `seq` (x : iterate' f x')

-- | Like 'iterate', but with a fixed number of iterations.
-- Produces a list of length n: [x, f x, f^2 x, ..., f^(n-1) x].
iterateN' :: Natural -> (a -> a) -> a -> [a]
iterateN' 0 _ _ = []
iterateN' n f x =
  let x' = f x
   in x' `seq` (x : iterateN' (n - 1) f x')

-- | Like 'iterate', but with a monadic action.
-- For input n, produces a monadic action
-- that applies the function n times.
iterateM :: Monad m => Natural -> (a -> m a) -> a -> m a
iterateM 0 _ x = return x
iterateM n f x = f x >>= iterateM (n - 1) f

replicate :: Natural -> a -> [a]
replicate n x
  | n == 0 = []
  | otherwise = x : replicate (n - 1) x

replicateA :: Applicative f => Natural -> f a -> f [a]
replicateA n fx = sequenceA (replicate n fx)

zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault _ _ _ [] [] = []
zipWithDefault f x _ [] bs = map (f x) bs
zipWithDefault f _ y as [] = map (`f` y) as
zipWithDefault f x y (a : as) (b : bs) = f a b : zipWithDefault f x y as bs

elemIndex :: Eq a => a -> [a] -> Maybe Natural
elemIndex x = go 0
 where
  go _ [] = Nothing
  go i (y : ys)
    | x == y = Just i
    | otherwise = go (i + 1) ys

(!!) :: [a] -> Natural -> a
(!!) = genericIndex

(!) :: Ord k => Map k a -> k -> a
(!) m k = case lookup k m of
  Just x -> x
  Nothing -> error "ZkFold.Prelude.!: key not found"

writeFileJSON :: ToJSON a => FilePath -> a -> IO ()
writeFileJSON file = writeFile file . encode

readFileJSON :: FromJSON a => FilePath -> IO a
readFileJSON file = do
  content <- readFile file
  case decode content of
    Nothing -> error "ZkFold.Prelude.readFileJSON: invalid JSON"
    Just x -> return x

assert :: Show a => Bool -> a -> x -> x
assert statement obj x = if statement then x else error $ show obj

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (lo, hi) = integerToNatural <$> chooseInteger (fromIntegral lo, fromIntegral hi)

elementsRep :: (Representable f, Traversable f) => [a] -> Gen (f a)
elementsRep = sequence . tabulate . const . elements

#if __GLASGOW_HASKELL__ < 910
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc l =  Just (init l, last l)
#endif

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (!x : xs) (!y : ys) =
  let !c = f x y
   in c : zipWith' f xs ys

zipVectorsWithDefault :: a -> (a -> a -> b) -> V.Vector a -> V.Vector a -> V.Vector b
zipVectorsWithDefault d f u v = V.zipWith f u' v'
 where
  lu = V.length u
  lv = V.length v
  u' = if lu < lv then u V.++ V.replicate (lv - lu) d else u
  v' = if lv < lu then v V.++ V.replicate (lu - lv) d else v
