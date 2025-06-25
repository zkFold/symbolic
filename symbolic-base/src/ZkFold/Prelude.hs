{-# LANGUAGE CPP #-}

{- HLINT ignore "Use replicateM" -}

module ZkFold.Prelude where

import Control.Applicative (Applicative)
import Control.Monad (Monad, return, (>>=))
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Bool (Bool, otherwise)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Eq (Eq, (==))
import Data.Foldable (Foldable)
import Data.Function (const, (.))
import Data.Functor ((<$>))
import Data.Functor.Rep (Representable, tabulate)
import qualified Data.List as L
import Data.Map (Map, lookup)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord, (<))
import Data.Traversable (Traversable, sequence, sequenceA)
import qualified Data.Vector as V
import GHC.Num (Natural, integerToNatural)
import GHC.Stack (HasCallStack)
import System.IO (FilePath, IO)
import Test.QuickCheck (Gen, chooseInteger, elements)
import Text.Show (Show, show)
import Prelude (Double, Integral)
import qualified Prelude as P

#if defined(wasm32_HOST_ARCH)
import           GHC.Wasm.Prim
import           System.IO.Unsafe     (unsafePerformIO)
#else
import qualified Debug.Trace as Trace
#endif

log2ceiling :: (Integral a, Integral b) => a -> b
log2ceiling = P.ceiling @Double . P.logBase 2 . P.fromIntegral

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' = L.foldl'

length :: Foldable t => t a -> Natural
length = foldl' (\c _ -> c P.+ 1) 0

take :: HasCallStack => Natural -> [a] -> [a]
take 0 _ = []
take n (x : xs) = x : take (n P.- 1) xs
take _ [] = P.error "ZkFold.Prelude.take: empty list"

drop :: Natural -> [a] -> [a]
drop 0 xs = xs
drop n (_ : xs) = drop (n P.- 1) xs
drop _ [] = P.error "ZkFold.Prelude.drop: empty list"

splitAt :: Natural -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

iterate' :: (a -> a) -> a -> [a]
iterate' f x =
  let x' = f x
   in x' `P.seq` (x : iterate' f x')

-- | Like 'iterate', but with a fixed number of iterations.
-- Produces a list of length n: [x, f x, f^2 x, ..., f^(n-1) x].
iterateN' :: Natural -> (a -> a) -> a -> [a]
iterateN' 0 _ _ = []
iterateN' n f x =
  let x' = f x
   in x' `P.seq` (x : iterateN' (n P.- 1) f x')

-- | Like 'iterate', but with a monadic action.
-- For input n, produces a monadic action
-- that applies the function n times.
iterateM :: Monad m => Natural -> (a -> m a) -> a -> m a
iterateM 0 _ x = return x
iterateM n f x = f x >>= iterateM (n P.- 1) f

replicate :: Natural -> a -> [a]
replicate n x
  | n == 0 = []
  | otherwise = x : replicate (n P.- 1) x

replicateA :: Applicative f => Natural -> f a -> f [a]
replicateA n fx = sequenceA (replicate n fx)

zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault _ _ _ [] [] = []
zipWithDefault f x _ [] bs = L.map (f x) bs
zipWithDefault f _ y as [] = L.map (`f` y) as
zipWithDefault f x y (a : as) (b : bs) = f a b : zipWithDefault f x y as bs

elemIndex :: Eq a => a -> [a] -> Maybe Natural
elemIndex x = go 0
 where
  go _ [] = Nothing
  go i (y : ys)
    | x == y = Just i
    | otherwise = go (i P.+ 1) ys

(!!) :: [a] -> Natural -> a
(!!) = L.genericIndex

(!) :: Ord k => Map k a -> k -> a
(!) m k = case lookup k m of
  Just x -> x
  Nothing -> P.error "ZkFold.Prelude.!: key not found"

writeFileJSON :: ToJSON a => FilePath -> a -> IO ()
writeFileJSON file = writeFile file . encode

readFileJSON :: FromJSON a => FilePath -> IO a
readFileJSON file = do
  content <- readFile file
  case decode content of
    Nothing -> P.error "ZkFold.Prelude.readFileJSON: invalid JSON"
    Just x -> return x

assert :: Show a => Bool -> a -> x -> x
assert statement obj x = if statement then x else P.error (show obj)

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (lo, hi) = integerToNatural <$> chooseInteger (P.fromIntegral lo, P.fromIntegral hi)

elementsRep :: (Representable f, Traversable f) => [a] -> Gen (f a)
elementsRep = sequence . tabulate . const . elements

unsnoc :: [a] -> Maybe ([a], a)
#if __GLASGOW_HASKELL__ < 910
unsnoc [] = Nothing
unsnoc l  = Just (L.init l, L.last l)
#else
unsnoc = L.unsnoc
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
  u' = if lu < lv then u V.++ V.replicate (lv P.- lu) d else u
  v' = if lv < lu then v V.++ V.replicate (lu P.- lv) d else v

-- Backend-specific trace functions

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "console.log($1)"
  js_print :: JSString -> IO ()

-- | traces from Debug.Trace do not work in wasm. For pure debug logs in wasm, these functions must me used.
--
traceWith :: (a -> String) -> String -> a -> a
traceWith f string expr = unsafePerformIO $ do
    js_print $ toJSString (string <> f expr)
    return expr
#else
traceWith :: (a -> String) -> String -> a -> a
traceWith f s a = Trace.trace (s <> f a) a
#endif

traceShow :: Show a => String -> a -> a
traceShow = traceWith show

trace :: String -> a -> a
trace = traceWith (const "")
