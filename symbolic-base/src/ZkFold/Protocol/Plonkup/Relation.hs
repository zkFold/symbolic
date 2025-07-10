{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Protocol.Plonkup.Relation where

import Data.Binary (Binary)
import Data.Constraint (withDict)
import Data.Constraint.Nat (timesNat)
import Data.Foldable (Foldable, foldMap, toList)
import Data.Function (flip, id, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (Rep, Representable, tabulate)
import Data.List (map, (++))
import qualified Data.List as L
import Data.Map (elems)
import qualified Data.Map.Monoidal as M
import Data.Maybe (Maybe (..), fromJust)
import Data.Monoid (Sum (..), (<>))
import Data.Ord (Ordering (..), compare, max, (<=))
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Par1 (..), (:*:) (..))
import GHC.IsList (fromList)
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Permutation (Permutation, fromCycles, mkIndexPartition)
import ZkFold.Algebra.Polynomial.Multivariate (evalMonomial, evalPolynomial, var)
import ZkFold.Algebra.Polynomial.Univariate (UnivariateRingPolyVec (..), toPolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (..), acSizeN, witnessGenerator)
import ZkFold.ArithmeticCircuit.Context (CircuitContext (..), lookupFunction)
import ZkFold.ArithmeticCircuit.Lookup (LookupTable (..), LookupType (LookupType))
import ZkFold.ArithmeticCircuit.Var (Var, evalVar, toVar)
import ZkFold.Control.Conditional
import ZkFold.Data.Bool
import ZkFold.Data.Eq
import ZkFold.Prelude (length, replicate, uncurry3)
import ZkFold.Protocol.Plonkup.Internal (PlonkupPermutationSize)
import ZkFold.Protocol.Plonkup.LookupConstraint (LookupConstraint (LookupConstraint))
import ZkFold.Protocol.Plonkup.PlonkConstraint (PlonkConstraint (..), toPlonkConstraint)
import ZkFold.Protocol.Plonkup.PlonkupConstraint
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.MonadCircuit (ResidueField (..))
import qualified Prelude as P

-- Here `n` is the total number of constraints, `i` is the number of inputs to the circuit, and `a` is the field type.
data PlonkupRelation i o n a pv = PlonkupRelation
  { qM :: !(pv n)
  , qL :: !(pv n)
  , qR :: !(pv n)
  , qO :: !(pv n)
  , qC :: !(pv n)
  , qK :: !(pv n)
  , t1 :: !(pv n)
  , t2 :: !(pv n)
  , t3 :: !(pv n)
  , sigma :: !(Permutation (3 * n))
  , witness :: i a -> (pv n, pv n, pv n)
  , pubInput :: i a -> [a]
  , prvNum :: !Natural
  -- ^ The number of private inputs.
  }

instance (Show a, Show (pv n)) => Show (PlonkupRelation i o n a pv) where
  show PlonkupRelation {..} =
    "Plonkup Relation: "
      ++ show qM
      ++ " "
      ++ show qL
      ++ " "
      ++ show qR
      ++ " "
      ++ show qO
      ++ " "
      ++ show qC
      ++ " "
      ++ show qK
      ++ " "
      ++ show t1
      ++ " "
      ++ show t2
      ++ " "
      ++ show t3
      ++ " "
      ++ show sigma

instance
  ( KnownNat n
  , UnivariateRingPolyVec a pv
  , KnownNat (PlonkupPermutationSize n)
  , Representable i
  , Representable o
  , Foldable o
  , Arithmetic a
  , Binary (Rep i)
  , Arbitrary (ArithmeticCircuit a i o)
  )
  => Arbitrary (PlonkupRelation i o n a pv)
  where
  arbitrary = fromJust . toPlonkupRelation @i @o @n @a @pv <$> arbitrary

instance {-# INCOHERENT #-} FromConstant c a => FromConstant c (Vector a) where
  fromConstant = V.singleton . fromConstant

instance {-# INCOHERENT #-} Scale c a => Scale c (Vector a) where
  scale = fmap . scale

instance Exponent a b => Exponent (Vector a) b where
  x ^ e = fmap (^ e) x

instance AdditiveSemigroup a => AdditiveSemigroup (Vector a) where
  (+) = zipLongest (+)

instance AdditiveMonoid a => AdditiveMonoid (Vector a) where
  zero = V.singleton zero

instance AdditiveGroup a => AdditiveGroup (Vector a) where
  negate = fmap negate

instance MultiplicativeSemigroup a => MultiplicativeSemigroup (Vector a) where
  (*) = zipLongest (*)

instance MultiplicativeMonoid a => MultiplicativeMonoid (Vector a) where
  one = V.singleton one

instance Semiring a => Semiring (Vector a)

instance Ring a => Ring (Vector a)

instance Eq a => Eq (Vector a) where
  type BooleanOf (Vector a) = BooleanOf a
  u == v = and $ zipLongest (==) u v
  u /= v = or $ zipLongest (/=) u v

instance (Field a, Conditional (BooleanOf a) (Vector a)) => Field (Vector a) where
  finv = fmap finv

instance SemiEuclidean a => SemiEuclidean (Vector a) where
  div = zipLongest div
  mod = zipLongest mod

instance Euclidean a => Euclidean (Vector a) where
  gcd = zipLongest gcd
  bezoutL = zipLongest bezoutL
  bezoutR = zipLongest bezoutR

instance Finite a => Finite (Vector a) where
  type Order (Vector a) = Order a

instance
  (ResidueField a, Conditional (BooleanOf a) (Vector a), Conditional (BooleanOf (IntegralOf a)) (Vector (IntegralOf a)))
  => ResidueField (Vector a)
  where
  type IntegralOf (Vector a) = Vector (IntegralOf a)
  fromIntegral = fmap fromIntegral
  toIntegral = fmap toIntegral

zipLongest :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipLongest !f !xs !ys =
  let (!xn, !yn) = (V.length xs, V.length ys)
   in case xn `compare` yn of
        LT -> V.zipWith f (xs <> V.replicate (yn P.- xn) (V.last xs)) ys
        EQ -> V.zipWith f xs ys
        GT -> V.zipWith f xs (ys <> V.replicate (xn P.- yn) (V.last ys))

toPlonkupRelation
  :: forall i o n a pv
   . ( KnownNat n
     , Arithmetic a
     , Binary (Rep i)
     , UnivariateRingPolyVec a pv
     , Representable i
     , Representable o
     , Foldable o
     )
  => ArithmeticCircuit a i o
  -> Maybe (PlonkupRelation i o n a pv)
toPlonkupRelation !ac =
  let !n = value @n

      !xPub = acOutput (acContext ac)
      !pubInputConstraints = L.map var (toList xPub)
      !plonkConstraints = L.map (evalPolynomial evalMonomial (var . toVar)) (elems (acSystem (acContext ac)))

      toTriple :: [t] -> (t, t, t)
      toTriple [!x] = (x, x, x)
      toTriple [!x, !y] = (x, x, y)
      toTriple [!x, !y, !z] = (x, y, z)
      toTriple ws = P.error ("Expected list of length 1-3, got " ++ show (length ws))

      unfold :: LookupTable a f -> (Natural, (Vector a -> Vector a) -> f (Vector a))
      unfold (Ranges !rs) =
        let !segs = S.toList rs
         in ( sum [toConstant (hi - lo + one) | (lo, hi) <- segs]
            , Par1 . ($ V.concat [V.enumFromTo lo hi | (lo, hi) <- segs])
            )
      unfold (Product !t !u) =
        let (!m, ts) = unfold t
            (!k, us) = unfold u
         in ( m * k
            , \f ->
                ts (f . V.concatMap (V.replicate (P.fromIntegral k)))
                  :*: us (f . V.concat . L.replicate (P.fromIntegral m))
            )
      unfold (Plot !g !t) =
        let (!k, ts) = unfold t
            g' = lookupFunction (acLookupFunction (acContext ac)) g
         in (k, \f -> let !ts' = ts id in f <$> (ts' :*: g' ts'))

      lkup
        :: Foldable f
        => LookupTable a f
        -> [[Var a]]
        -> ([LookupConstraint i a], Sum Natural, (Vector a, Vector a, Vector a))
      lkup !lt !vs =
        let (!k, !ts) = unfold lt
         in ( L.map (uncurry3 LookupConstraint . toTriple) vs
            , Sum k
            , toTriple $ toList (ts id)
            )

      -- Lookup queries.
      (!xLookup, Sum !nLookup, (!xs, !ys, !zs)) = case M.assocs (acLookup $ acContext ac) of
        [] -> ([], 0, (V.empty, V.empty, V.empty))
        [(LookupType lt, vs)] -> lkup lt [L.map toVar v | v <- S.toList vs]
        asscs -> flip foldMap (L.zip [(0 :: Natural) ..] asscs) $
          -- \^ Folding concatenates tuples pointwise, so:
          -- \* lists of constraints get concatenated
          -- \* vectors of values get concatenated, too
          -- Just as planned!
          \(fromConstant -> i, (LookupType lt, vs)) ->
            lkup
              (Ranges (S.singleton (i, i)) `Product` lt)
              [fromConstant i : L.map toVar v | v <- S.toList vs]
      -- NOTE we use constant variables
      -- to encode № of a lookup table

      !t1 = toPolyVec xs
      !t2 = toPolyVec ys
      !t3 = toPolyVec zs

      !cNum = acSizeN ac + length (tabulate @o id) + length xLookup

      !plonkupSystem =
        fromList $
          L.concat
            [ L.map (ConsPlonk . toPlonkConstraint) (pubInputConstraints ++ plonkConstraints)
            , ConsLookup <$> xLookup
            , replicate (n -! cNum) ConsExtra
            ]

      !qM = toPolyVec $ fmap (qm . getPlonkConstraint) plonkupSystem
      !qL = toPolyVec $ fmap (ql . getPlonkConstraint) plonkupSystem
      !qR = toPolyVec $ fmap (qr . getPlonkConstraint) plonkupSystem
      !qO = toPolyVec $ fmap (qo . getPlonkConstraint) plonkupSystem
      !qC = toPolyVec $ fmap (qc . getPlonkConstraint) plonkupSystem
      !qK = toPolyVec $ fmap isLookupConstraint plonkupSystem

      !a = fmap getA plonkupSystem
      !b = fmap getB plonkupSystem
      !c = fmap getC plonkupSystem
      -- TODO: Permutation code is not particularly safe. We rely on the list being of length 3*n.
      !sigma = withDict (timesNat @3 @n) (fromCycles @(3 * n) $ mkIndexPartition $ V.concat [a, b, c])

      eval = evalVar . witnessGenerator ac
      w1 i = toPolyVec $ fmap (eval i) a
      w2 i = toPolyVec $ fmap (eval i) b
      w3 i = toPolyVec $ fmap (eval i) c
      witness i = (w1 i, w2 i, w3 i)
      pubInput i = map (eval i) $ toList xPub

      prvNum = 0
   in if max cNum nLookup <= n
        then Just $ PlonkupRelation {..}
        else Nothing
