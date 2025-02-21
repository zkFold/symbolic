{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Ord (Ord (..), blueprintGE, bitwiseGE, bitwiseGT, getBitsBE) where

import           Control.Monad                    (foldM)
import qualified Data.Bool                        as Haskell
import           Data.Data                        (Proxy (..))
import           Data.Foldable                    (Foldable, concatMap, toList)
import           Data.Function                    ((.))
import           Data.Functor                     (fmap, (<$>))
import           Data.List                        (map, reverse)
import           Data.Traversable                 (Traversable (traverse))
import qualified Data.Zip                         as Z
import           GHC.Generics                     (Par1 (..))
import           Prelude                          (type (~), ($))
import qualified Prelude                          as Haskell

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Symbolic.Class            (Arithmetic, Symbolic (BaseField), symbolic2F, symbolicF)
import           ZkFold.Symbolic.Data.Bool        (Bool (..))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (expansion)
import           ZkFold.Symbolic.Data.Conditional (Conditional (..))
import           ZkFold.Symbolic.MonadCircuit

-- TODO (Issue #23): add `compare`
class Ord b a where
    infix 4 <=
    (<=) :: a -> a -> b

    infix 4 <
    (<) :: a -> a -> b

    infix 4 >=
    (>=) :: a -> a -> b

    infix 4 >
    (>) :: a -> a -> b

    max :: a -> a -> a
    -- max x y = bool @b y x $ x <= y

    min :: a -> a -> a
    -- min x y = bool @b y x $ x >= y

instance Haskell.Ord a => Ord Haskell.Bool a where
    (<=) = (Haskell.<=)

    (<) = (Haskell.<)

    (>=) = (Haskell.>=)

    (>) = (Haskell.>)

    max = Haskell.max

    min = Haskell.min

-- | Every @SymbolicData@ type can be compared lexicographically.
instance (Symbolic c, LayoutFunctor f) => Ord (Bool c) (c f) where

    x <= y = y >= x

    x <  y = y > x

    x >= y = bitwiseGE @1 (getBitsBE x) (getBitsBE y)

    x > y = bitwiseGT @1 (getBitsBE x) (getBitsBE y)

    max x y = bool @(Bool c) x y $ x < y

    min x y = bool @(Bool c) x y $ x > y

getBitsBE ::
  forall c x .
  (SymbolicOutput x, Context x ~ c) =>
  x -> c []
-- ^ @getBitsBE x@ returns a list of circuits computing bits of @x@, eldest to
-- youngest.
getBitsBE x = symbolicF (arithmetize x Proxy)
    (concatMap (reverse . padBits n . map fromConstant . binaryExpansion . toConstant))
    (fmap (concatMap reverse) . traverse (expansion n) . toList)
  where n = numberOfBits @(BaseField c)

bitwiseGE :: forall r c f . (Symbolic c, Z.Zip f, Foldable f, KnownNat r) => c f -> c f -> Bool c
-- ^ Given two lists of bits of equal length, compares them lexicographically.
bitwiseGE xs ys = Bool $
  symbolic2F xs ys
    (\us vs -> Par1 $ Haskell.bool zero one (toList us Haskell.>= toList vs))
    $ \is js -> Par1 <$> blueprintGE @r is js

blueprintGE :: forall r i a w m f . (Arithmetic a, MonadCircuit i a w m, Z.Zip f, Foldable f, KnownNat r) => f i -> f i -> m i
blueprintGE xs ys = do
  (_, hasNegOne) <- circuitDelta @r xs ys
  newAssigned $ \p -> one - p hasNegOne

bitwiseGT :: forall r c f . (Symbolic c, Z.Zip f, Foldable f, KnownNat r) => c f -> c f -> Bool c
-- ^ Given two lists of bits of equal length, compares them lexicographically.
bitwiseGT xs ys = Bool $
  symbolic2F xs ys
    (\us vs -> Par1 $ Haskell.bool zero one (toList us Haskell.> toList vs))
    $ \is js -> do
      (hasOne, hasNegOne) <- circuitDelta @r is js
      Par1 <$> newAssigned (\p -> p hasOne * (one - p hasNegOne))

-- | Compare two sets of r-bit words lexicographically
--
circuitDelta :: forall r i a w m f . (Arithmetic a, MonadCircuit i a w m, Z.Zip f, Foldable f, KnownNat r) => f i -> f i -> m (i, i)
circuitDelta l r = do
    z1 <- newAssigned (Haskell.const zero)
    z2 <- newAssigned (Haskell.const zero)
    foldM update (z1, z2) $ Z.zip l r
        where
            bound = scale ((2 ^ value @r) -! 1) one

            -- | If @z1@ is set, there was an index i where @xs[i] == 1@ and @ys[i] == 0@ and @xs[j] == ys[j]@ for all j < i.
            -- In this case, no matter what bit states are after this index, @z1@ and @z2@ are not updated.
            --
            --   If @z2@ is set, there was an index i where @xs[i] == 0@ and @ys[i] == 1@ and @xs[j] == ys[j]@ for all j < i.
            -- In the same manner, @z1@ and @z2@ won't be updated afterwards.
            update :: (i, i) -> (i, i) -> m (i, i)
            update (z1, z2) (x, y) = do
                -- @f1@ is one if and only if @x > y@ and zero otherwise.
                -- @(y + 1) `div` (x + 1)@ is zero if and only if @y < x@ regardless of whether @x@ is zero.
                -- @x@ and @y@ are expected to be of at most @r@ bits where @r << NumberOfBits a@, so @x + 1@ will not be zero either.
                -- Because of our laws for @finv@, @q // q@ is 1 if @q@ is not zero, and zero otherwise.
                -- This is exactly the opposite of what @f1@ should be.
                f1 <- newRanged one $
                    let q = fromIntegral (toIntegral (at y + one @w) `div` toIntegral (at x + one @w))
                     in one - q // q

                -- f2 is one if and only if y > x and zero otherwise
                f2 <- newRanged one $
                    let q = fromIntegral (toIntegral (at x + one @w) `div` toIntegral (at y + one @w))
                     in one - q // q

                dxy <- newAssigned (\p -> p x - p y)

                d1  <- newAssigned (\p -> p f1 * p dxy - p f1)
                d1' <- newAssigned (\p -> (one - p f1) * negate (p dxy))
                rangeConstraint d1  bound
                rangeConstraint d1' bound

                d2  <- newAssigned (\p -> p f2 * (negate one - p dxy))
                d2' <- newAssigned (\p -> p dxy - p f2 * p dxy)
                rangeConstraint d2  bound
                rangeConstraint d2' bound

                bothZero <- newAssigned $ \p -> (one - p z1) * (one - p z2)

                f1z <- newAssigned $ \p -> p bothZero * p f1
                f2z <- newAssigned $ \p -> p bothZero * p f2

                z1' <- newAssigned $ \p -> p z1 + p f1z
                z2' <- newAssigned $ \p -> p z2 + p f2z

                Haskell.return (z1', z2')
