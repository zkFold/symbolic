module ZkFold.Prelude where

import           Data.Aeson           (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy (readFile, writeFile)
import           Data.List            (genericIndex)
import           Data.Map             (Map, lookup)
import           GHC.Num              (Natural, integerToNatural)
import           GHC.Stack            (HasCallStack)
import           Prelude              hiding (drop, lookup, readFile, replicate, take, writeFile, (!!))
import           Test.QuickCheck      (Gen, chooseInteger, shuffle)

log2ceiling :: (Integral a, Integral b) => a -> b
log2ceiling = ceiling @Double . logBase 2 . fromIntegral

length :: Foldable t => t a -> Natural
length = foldl' (\c _ -> c + 1) 0

take :: HasCallStack => Natural -> [a] -> [a]
take 0 _      = []
take n (x:xs) = x : take (n - 1) xs
take _ []     = error "ZkFold.Prelude.take: empty list"

drop :: Natural -> [a] -> [a]
drop 0 xs     = xs
drop n (_:xs) = drop (n - 1) xs
drop _ []     = error "ZkFold.Prelude.drop: empty list"

splitAt :: Natural -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

iterateM :: Monad m => Natural -> (a -> m a) -> a -> m a
iterateM 0 _ x = return x
iterateM n f x = f x >>= iterateM (n - 1) f

replicate :: Natural -> a -> [a]
replicate n x
    | n == 0    = []
    | otherwise = x : replicate (n - 1) x

replicateA :: Applicative f => Natural -> f a -> f [a]
replicateA n fx = sequenceA (replicate n fx)

zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault _ _ _ [] []         = []
zipWithDefault f x _ [] bs         = map (f x) bs
zipWithDefault f _ y as []         = map (`f` y) as
zipWithDefault f x y (a:as) (b:bs) = f a b : zipWithDefault f x y as bs

elemIndex :: Eq a => a -> [a] -> Maybe Natural
elemIndex x = go 0
    where
        go _ [] = Nothing
        go i (y:ys)
            | x == y    = Just i
            | otherwise = go (i + 1) ys

(!!) :: [a] -> Natural -> a
(!!) = genericIndex

(!) :: Ord k => Map k a -> k -> a
(!) m k = case lookup k m of
    Just x  -> x
    Nothing -> error "ZkFold.Prelude.!: key not found"

writeFileJSON :: ToJSON a => FilePath -> a -> IO ()
writeFileJSON file = writeFile file . encode

readFileJSON :: FromJSON a => FilePath -> IO a
readFileJSON file = do
    content <- readFile file
    case decode content of
        Nothing -> error "ZkFold.Prelude.readFileJSON: invalid JSON"
        Just x  -> return x

assert :: Show a => Bool -> a -> x -> x
assert statement obj x = if statement then x else error $ show obj

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (lo, hi) = integerToNatural <$> chooseInteger (fromIntegral lo, fromIntegral hi)

-- | Choose a list of length `l` from a list allowing repetitions.
chooseFromList :: Natural -> [a] -> Gen [a]
chooseFromList l as = take l <$> shuffle (concatMap (replicate l) as)
