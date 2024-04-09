{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module ZkFold.Symbolic.Data.UInt
  ( StrictConv (..),
    StrictNum (..),
    UInt (..),
    toConstant,
  )
where

import Control.Applicative ((<*>))
import Control.Monad.State (StateT (..))
import Data.Foldable (foldr, foldrM, for_)
import Data.Functor ((<$>))
import Data.List (map, unfoldr, zip, zipWith)
import Data.Map (fromList, (!))
import Data.Traversable (for, traverse)
import Data.Tuple (swap)
import GHC.Natural (naturalFromInteger)
import GHC.TypeNats (KnownNat, Natural)
import Test.QuickCheck (Arbitrary (..), chooseInteger)
import ZkFold.Base.Algebra.Basic.Class
import ZkFold.Base.Algebra.Basic.Field (Zp, fromZp)
import ZkFold.Prelude (length, replicate, replicateA, splitAt)
import ZkFold.Symbolic.Compiler hiding (forceZero)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Combinators (expansion, splitExpansion)
import ZkFold.Symbolic.Data.Combinators
import Prelude (Integer, error, flip, otherwise, return, ($), (++), (.), (>>=))
import qualified Prelude as Haskell

-- TODO (Issue #18): hide this constructor
data UInt (n :: Natural) a = UInt ![a] !a
  deriving (Haskell.Show, Haskell.Eq)

instance (FromConstant Natural a, Finite a, AdditiveMonoid a, KnownNat n) => FromConstant Natural (UInt n a) where
  fromConstant = Haskell.fst . cast @a @n

instance (FromConstant Natural a, Finite a, AdditiveMonoid a, KnownNat n) => FromConstant Integer (UInt n a) where
  fromConstant = fromConstant . naturalFromInteger . (`Haskell.mod` (2 ^ getNatural @n))

cast :: forall a n. (FromConstant Natural a, Finite a, AdditiveMonoid a, KnownNat n) => Natural -> (UInt n a, [a])
cast n =
  let base = 2 ^ registerSize @a @n
      registers = map fromConstant $ flip unfoldr n $ \case
        0 -> Haskell.Nothing
        x -> Haskell.Just (swap $ x `Haskell.divMod` base)
      r = numberOfRegisters @a @n - 1
   in case greedySplitAt r registers of
        (lo, hi : rest) -> (UInt lo hi, rest)
        (lo, []) -> (UInt (lo ++ replicate (r - length lo) zero) zero, [])
  where
    greedySplitAt 0 xs = ([], xs)
    greedySplitAt _ [] = ([], [])
    greedySplitAt m (x : xs) =
      let (ys, zs) = greedySplitAt (m - 1) xs
       in (x : ys, zs)

--------------------------------------------------------------------------------

instance (KnownNat p, KnownNat n) => ToConstant (UInt n (Zp p)) Natural where
  toConstant (UInt xs x) = foldr (\p y -> fromZp p + base * y) 0 (xs ++ [x])
    where
      base = 2 ^ registerSize @(Zp p) @n

instance (KnownNat p, KnownNat n) => AdditiveSemigroup (UInt n (Zp p)) where
  x + y = fromConstant $ toConstant x + (toConstant @_ @Natural) y

instance (KnownNat p, KnownNat n) => AdditiveMonoid (UInt n (Zp p)) where
  zero = fromConstant (0 :: Natural)

instance (KnownNat p, KnownNat n) => AdditiveGroup (UInt n (Zp p)) where
  x - y = fromConstant $ toConstant x + 2 ^ getNatural @n - (toConstant @_ @Natural) y
  negate x = fromConstant $ 2 ^ getNatural @n - (toConstant @_ @Natural) x

instance (KnownNat p, KnownNat n) => MultiplicativeSemigroup (UInt n (Zp p)) where
  x * y = fromConstant $ toConstant x * (toConstant @_ @Natural) y

instance (KnownNat p, KnownNat n) => MultiplicativeMonoid (UInt n (Zp p)) where
  one = fromConstant (1 :: Natural)

instance (KnownNat p, KnownNat n) => Semiring (UInt n (Zp p))

instance (KnownNat p, KnownNat n) => Ring (UInt n (Zp p))

instance (KnownNat p, KnownNat n) => Arbitrary (UInt n (Zp p)) where
  arbitrary =
    UInt
      <$> replicateA (numberOfRegisters @(Zp p) @n - 1) (toss $ registerSize @(Zp p) @n)
      <*> toss (highRegisterSize @(Zp p) @n)
    where
      toss b = fromConstant <$> chooseInteger (0, 2 ^ b - 1)

--------------------------------------------------------------------------------

instance (Arithmetic a, KnownNat n) => Arithmetizable a (UInt n (ArithmeticCircuit a)) where
  arithmetize (UInt as a) = for (as ++ [a]) runCircuit

  restore as = case splitAt (numberOfRegisters @a @n - 1) as of
    (lo, [hi]) -> UInt lo hi
    (_, _) -> error "UInt: invalid number of values"

  typeSize = numberOfRegisters @a @n

instance (Arithmetic a, KnownNat n) => AdditiveSemigroup (UInt n (ArithmeticCircuit a)) where
  UInt [] x + UInt [] y = UInt [] $ circuit $ do
    (z, _) <- runCircuit (x + y) >>= splitExpansion (highRegisterSize @a @n) 1
    return z
  UInt (x : xs) z + UInt (y : ys) w =
    let solve :: (MonadBlueprint i a m) => m [i]
        solve = do
          (i, j) <- runCircuit (x + y) >>= splitExpansion (registerSize @a @n) 1
          (zs, c) <-
            flip runStateT j $
              traverse StateT $
                zipWith (fullAdder $ registerSize @a @n) xs ys
          (k, _) <- fullAdder (highRegisterSize @a @n) z w c
          return (k : i : zs)
     in case circuits solve of
          (hi : lo) -> UInt lo hi
          [] -> error "UInt: unreachable"
  UInt _ _ + UInt _ _ = error "UInt: unreachable"

instance (Arithmetic a, KnownNat n) => AdditiveMonoid (UInt n (ArithmeticCircuit a)) where
  zero = UInt (replicate (numberOfRegisters @a @n - 1) zero) zero

instance (Arithmetic a, KnownNat n) => AdditiveGroup (UInt n (ArithmeticCircuit a)) where
  UInt [] x - UInt [] y = UInt [] $ circuit $ do
    i <- runCircuit x
    j <- runCircuit y
    z0 <- newAssigned (\v -> v i - v j + ((one + one) ^ registerSize @a @n) `scale` one)
    (z, _) <- splitExpansion (highRegisterSize @a @n) 1 z0
    return z
  UInt (x : xs) z - UInt (y : ys) w =
    let t :: a
        t = (one + one) ^ registerSize @a @n - one

        solve :: (MonadBlueprint i a m) => m [i]
        solve = do
          i <- runCircuit x
          j <- runCircuit y
          s <- newAssigned (\v -> v i - v j + (t + one) `scale` one)
          (k, b0) <- splitExpansion (registerSize @a @n) 1 s
          (zs, b) <- flip runStateT b0 $ traverse StateT (zipWith fullSub xs ys)
          i' <- runCircuit z
          j' <- runCircuit w
          s'0 <- newAssigned (\v -> v i' - v j' + v b + t `scale` one)
          (s', _) <- splitExpansion (highRegisterSize @a @n) 1 s'0
          return (s' : k : zs)

        fullSub :: (MonadBlueprint i a m) => ArithmeticCircuit a -> ArithmeticCircuit a -> i -> m (i, i)
        fullSub xk yk b = do
          i <- runCircuit xk
          j <- runCircuit yk
          s <- newAssigned (\v -> v i - v j + v b + t `scale` one)
          splitExpansion (registerSize @a @n) 1 s
     in case circuits solve of
          (hi : lo) -> UInt lo hi
          [] -> error "UInt: unreachable"
  UInt _ _ - UInt _ _ = error "UInt: unreachable"

  negate (UInt [] x) = UInt [] (negateN (2 ^ highRegisterSize @a @n) x)
  negate (UInt (x : xs) x') =
    let y = negateN (2 ^ registerSize @a @n) x
        ys = map (negateN $ 2 ^ registerSize @a @n - 1) xs
        y' = negateN (2 ^ highRegisterSize @a @n - 1) x'
     in UInt (y : ys) y'

negateN :: (Arithmetic a) => Natural -> ArithmeticCircuit a -> ArithmeticCircuit a
negateN n r = circuit $ do
  i <- runCircuit r
  newAssigned (\v -> fromConstant n - v i)

instance (Arithmetic a, KnownNat n) => MultiplicativeSemigroup (UInt n (ArithmeticCircuit a)) where
  UInt [] x * UInt [] y = UInt [] $ circuit $ do
    (z, _) <- runCircuit (x * y) >>= splitExpansion (highRegisterSize @a @n) (maxOverflow @a @n)
    return z
  UInt (x : xs) z * UInt (y : ys) w =
    let solve :: (MonadBlueprint i a m) => m [i]
        solve = do
          i <- runCircuit x
          j <- runCircuit y
          is <- for xs runCircuit
          js <- for ys runCircuit
          i' <- runCircuit z
          j' <- runCircuit w
          let cs = fromList $ zip [0 ..] (i : is ++ [i'])
              ds = fromList $ zip [0 ..] (j : js ++ [j'])
              r = numberOfRegisters @a @n
          -- single addend for lower register
          q <- newAssigned (\v -> v i * v j)
          -- multiple addends for middle registers
          qs <- for [1 .. r - 2] $ \k ->
            for [0 .. k] $ \l ->
              newAssigned (\v -> v (cs ! l) * v (ds ! (k - l)))
          -- lower register
          (p, c) <- splitExpansion (registerSize @a @n) (registerSize @a @n) q
          -- middle registers
          (ps, c') <-
            flip runStateT c $
              for qs $
                StateT . \rs c' -> do
                  s <- foldrM (\k l -> newAssigned (\v -> v k + v l)) c' rs
                  splitExpansion (registerSize @a @n) (maxOverflow @a @n) s
          -- high register
          p'0 <- foldrM (\k l -> newAssigned (\v -> v l + v (cs ! k) * v (ds ! (r - 1 - k)))) c' [0 .. r - 1]
          (p', _) <- splitExpansion (highRegisterSize @a @n) (maxOverflow @a @n) p'0
          return (p' : p : ps)
     in case circuits solve of
          (hi : lo) -> UInt lo hi
          [] -> error "UInt: unreachable"
  UInt _ _ * UInt _ _ = error "UInt: unreachable"

instance (Arithmetic a, KnownNat n) => MultiplicativeMonoid (UInt n (ArithmeticCircuit a)) where
  one
    | numberOfRegisters @a @n Haskell.== 1 = UInt [] one
    | otherwise = UInt (one : replicate (numberOfRegisters @a @n - 2) zero) zero

instance (Arithmetic a, KnownNat n) => Semiring (UInt n (ArithmeticCircuit a))

instance (Arithmetic a, KnownNat n) => Ring (UInt n (ArithmeticCircuit a))

--------------------------------------------------------------------------------

class StrictConv b a where
  strictConv :: b -> a

instance (FromConstant Natural a, Finite a, AdditiveMonoid a, KnownNat n) => StrictConv Natural (UInt n a) where
  strictConv n = case cast @a @n n of
    (x, []) -> x
    (_, _) -> error "strictConv: overflow"

class StrictNum a where
  strictAdd :: a -> a -> a
  strictSub :: a -> a -> a
  strictMul :: a -> a -> a

instance (KnownNat p, KnownNat n) => StrictNum (UInt n (Zp p)) where
  strictAdd x y = strictConv $ toConstant x + (toConstant @_ @Natural) y
  strictSub x y = strictConv $ toConstant x - (toConstant @_ @Natural) y
  strictMul x y = strictConv $ toConstant x * (toConstant @_ @Natural) y

instance (Arithmetic a, KnownNat n) => StrictNum (UInt n (ArithmeticCircuit a)) where
  strictAdd (UInt [] x) (UInt [] y) = UInt [] $ circuit $ do
    z <- runCircuit (x + y)
    _ <- expansion (highRegisterSize @a @n) z
    return z
  strictAdd (UInt (x : xs) z) (UInt (y : ys) w) =
    let solve :: (MonadBlueprint i a m) => m [i]
        solve = do
          (i, j) <- runCircuit (x + y) >>= splitExpansion (registerSize @a @n) 1
          (zs, c) <-
            flip runStateT j $
              traverse StateT $
                zipWith (fullAdder $ registerSize @a @n) xs ys
          k <- fullAdded z w c
          _ <- expansion (highRegisterSize @a @n) k
          return (k : i : zs)
     in case circuits solve of
          (hi : lo) -> UInt lo hi
          [] -> error "UInt: unreachable"
  strictAdd (UInt _ _) (UInt _ _) = error "UInt: unreachable"

  strictSub (UInt [] x) (UInt [] y) = UInt [] $ circuit $ do
    z <- runCircuit (x - y)
    _ <- expansion (highRegisterSize @a @n) z
    return z
  strictSub (UInt (x : xs) z) (UInt (y : ys) w) =
    let t :: a
        t = (one + one) ^ registerSize @a @n - one

        solve :: (MonadBlueprint i a m) => m [i]
        solve = do
          i <- runCircuit x
          j <- runCircuit y
          s <- newAssigned (\v -> v i - v j + (t + one) `scale` one)
          (k, b0) <- splitExpansion (registerSize @a @n) 1 s
          (zs, b) <- flip runStateT b0 $ traverse StateT (zipWith fullSub xs ys)
          i' <- runCircuit z
          j' <- runCircuit w
          s' <- newAssigned (\v -> v i' - v j' + v b - one)
          _ <- expansion (highRegisterSize @a @n) s'
          return (s' : k : zs)

        fullSub :: (MonadBlueprint i a m) => ArithmeticCircuit a -> ArithmeticCircuit a -> i -> m (i, i)
        fullSub xk yk b = do
          i <- runCircuit xk
          j <- runCircuit yk
          s <- newAssigned (\v -> v i - v j + v b + t `scale` one)
          splitExpansion (registerSize @a @n) 1 s
     in case circuits solve of
          (hi : lo) -> UInt lo hi
          [] -> error "UInt: unreachable"
  strictSub (UInt _ _) (UInt _ _) = error "UInt: unreachable"

  strictMul (UInt [] x) (UInt [] y) = UInt [] $ circuit $ do
    z <- runCircuit (x * y)
    _ <- expansion (highRegisterSize @a @n) z
    return z
  strictMul (UInt (x : xs) z) (UInt (y : ys) w) =
    let solve :: (MonadBlueprint i a m) => m [i]
        solve = do
          i <- runCircuit x
          j <- runCircuit y
          is <- for xs runCircuit
          js <- for ys runCircuit
          i' <- runCircuit z
          j' <- runCircuit w
          let cs = fromList $ zip [0 ..] (i : is ++ [i'])
              ds = fromList $ zip [0 ..] (j : js ++ [j'])
              r = numberOfRegisters @a @n
          -- single addend for lower register
          q <- newAssigned (\v -> v i * v j)
          -- multiple addends for middle registers
          qs <- for [1 .. r - 2] $ \k ->
            for [0 .. k] $ \l ->
              newAssigned (\v -> v (cs ! l) * v (ds ! (k - l)))
          -- lower register
          (p, c) <- splitExpansion (registerSize @a @n) (registerSize @a @n) q
          -- middle registers
          (ps, c') <-
            flip runStateT c $
              for qs $
                StateT . \rs c' -> do
                  s <- foldrM (\k l -> newAssigned (\v -> v k + v l)) c' rs
                  splitExpansion (registerSize @a @n) (maxOverflow @a @n) s
          -- high register
          p' <- foldrM (\k l -> newAssigned (\v -> v l + v (cs ! k) * v (ds ! (r - 1 - k)))) c' [0 .. r - 1]
          _ <- expansion (highRegisterSize @a @n) p'
          -- all addends higher should be zero
          for_ [r .. r * 2 - 2] $ \k ->
            for_ [k - r + 1 .. r - 1] $ \l ->
              constraint (\v -> v (cs ! l) * v (ds ! (k - l)))
          return (p' : p : ps)
     in case circuits solve of
          (hi : lo) -> UInt lo hi
          [] -> error "UInt: unreachable"
  strictMul (UInt _ _) (UInt _ _) = error "UInt: unreachable"

--------------------------------------------------------------------------------

fullAdder :: (MonadBlueprint i a m) => Natural -> ArithmeticCircuit a -> ArithmeticCircuit a -> i -> m (i, i)
fullAdder r xk yk c = fullAdded xk yk c >>= splitExpansion r 1

fullAdded :: (MonadBlueprint i a m) => ArithmeticCircuit a -> ArithmeticCircuit a -> i -> m i
fullAdded xk yk c = do
  i <- runCircuit xk
  j <- runCircuit yk
  newAssigned (\v -> v i + v j + v c)
