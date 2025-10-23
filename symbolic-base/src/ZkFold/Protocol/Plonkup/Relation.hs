{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.Protocol.Plonkup.Relation where

import Control.Applicative (pure)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary)
import Data.Constraint (withDict)
import Data.Constraint.Nat (timesNat)
import Data.Foldable (Foldable, foldMap, toList)
import Data.Function (flip, id, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (Rep, Representable, tabulate)
import Data.List (map, (++))
import qualified Data.List as L
import Data.Map (elems, (!))
import qualified Data.Map.Monoidal as M
import Data.Maybe (Maybe (..), fromJust)
import Data.Monoid (Sum (..))
import Data.Semigroup ((<>))
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.IsList (fromList)
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show (..))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Permutation (Permutation, fromCycles, mkIndexPartition)
import ZkFold.Algebra.Polynomial.Multivariate (evalMonomial, evalPolynomial, var)
import ZkFold.Algebra.Polynomial.Univariate (UnivariateRingPolyVec (..), toPolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (..), acSizeN, desugarRanges, witnessGenerator)
import ZkFold.ArithmeticCircuit.Context (CircuitContext (..), LookupFunction (..), LookupType (..))
import ZkFold.ArithmeticCircuit.Var (Var, evalVar, toVar)
import ZkFold.Prelude (length, replicate, uncurry3)
import ZkFold.Protocol.Plonkup.Internal (PlonkupPermutationSize)
import ZkFold.Protocol.Plonkup.LookupConstraint (LookupConstraint (LookupConstraint))
import ZkFold.Protocol.Plonkup.PlonkConstraint (PlonkConstraint (..), toPlonkConstraint)
import ZkFold.Protocol.Plonkup.PlonkupConstraint
import ZkFold.Protocol.Plonkup.Relation.LookupVector (fromVector, toVector)
import ZkFold.Symbolic.Class (Arithmetic)

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

instance Aeson.ToJSON (pv n) => Aeson.ToJSON (PlonkupRelation i o n a pv) where
  toJSON PlonkupRelation {..} =
    Aeson.object
      [ "qM" .= qM
      , "qL" .= qL
      , "qR" .= qR
      , "qO" .= qO
      , "qC" .= qC
      , "qK" .= qK
      , "t1" .= t1
      , "t2" .= t2
      , "t3" .= t3
      , "sigma" .= sigma
      , -- TODO serialize witness & pub input as well
        "prvNum" .= prvNum
      ]

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
  , Binary a
  , Binary (Rep i)
  , Arbitrary (ArithmeticCircuit a i o)
  )
  => Arbitrary (PlonkupRelation i o n a pv)
  where
  arbitrary = fromJust . toPlonkupRelation @i @o @n @a @pv <$> arbitrary

regSize :: Natural
regSize = 2 ^ (16 :: Natural) -! 1

toPlonkupRelation
  :: forall i o n a pv
   . ( KnownNat n
     , Arithmetic a
     , Binary a
     , Binary (Rep i)
     , UnivariateRingPolyVec a pv
     , Representable i
     , Representable o
     , Foldable o
     )
  => ArithmeticCircuit a i o
  -> Maybe (PlonkupRelation i o n a pv)
toPlonkupRelation (desugarRanges (Just regSize) -> !ac) =
  let !n = value @n

      !xPub = acOutput (acContext ac)
      !pubInputConstraints = L.map var (toList xPub)
      !plonkConstraints = L.map (evalPolynomial evalMonomial (var . toVar)) (elems (acSystem (acContext ac)))

      toTriple :: [t] -> (t, t, t)
      toTriple [!x] = (x, x, x)
      toTriple [!x, !y] = (x, x, y)
      toTriple [!x, !y, !z] = (x, y, z)
      toTriple ws = P.error ("Expected list of length 1-3, got " ++ show (length ws))

      unfold :: LookupType a -> (Natural, (Vector a -> Vector a) -> [Vector a])
      unfold (LTRanges !rs) =
        let !segs = S.toList rs
         in ( sum [toConstant (hi - lo + one) | (lo, hi) <- segs]
            , pure . ($ V.concat [V.enumFromTo lo hi | (lo, hi) <- segs])
            )
      unfold (LTProduct !t !u) =
        let (!m, ts) = unfold t
            (!k, us) = unfold u
         in ( m * k
            , \f ->
                ts (f . V.concatMap (V.replicate (P.fromIntegral k)))
                  <> us (f . V.concat . L.replicate (P.fromIntegral m))
            )
      unfold (LTPlot !g !t) =
        let (!k, ts) = unfold t
            LookupFunction g' = acLookupFunction (acContext ac) ! g
         in ( k
            , \f ->
                let !ts' = ts id
                 in f <$> (ts' <> fmap (toVector k) (g' $ fromVector <$> ts'))
            )

      lkup
        :: LookupType a
        -> [[Var a]]
        -> ([LookupConstraint i a], Sum Natural, (Vector a, Vector a, Vector a))
      lkup !lt !vs =
        let (!k, !ts) = unfold lt
         in ( L.map (uncurry3 LookupConstraint . toTriple) vs
            , Sum k
            , toTriple (ts id)
            )

      -- Lookup queries.
      (!xLookup, Sum !nLookup, (!xs, !ys, !zs)) = case M.assocs (acLookup $ acContext ac) of
        [] -> ([], 0, (V.empty, V.empty, V.empty))
        [(lt, vs)] -> lkup lt [L.map toVar v | v <- S.toList vs]
        asscs -> flip foldMap (L.zip [(0 :: Natural) ..] asscs) $
          -- \^ Folding concatenates tuples pointwise, so:
          -- \* lists of constraints get concatenated
          -- \* vectors of values get concatenated, too
          -- Just as planned!
          \(fromConstant -> i, (lt, vs)) ->
            lkup
              (LTRanges (S.singleton (i, i)) `LTProduct` lt)
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
   in if P.max cNum nLookup P.<= n
        then Just $ PlonkupRelation {..}
        else Nothing
