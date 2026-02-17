{-# LANGUAGE ImportQualifiedPost #-}

module ZkFold.Protocol.Plonkup.PlonkConstraint where

import Control.Monad (guard, replicateM, return)
import Data.Binary (Binary)
import Data.Containers.ListUtils (nubOrd)
import Data.Eq (Eq (..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (all, concatMap, filter, find, head, map, permutations, sort, tails, (!!), (++))
import Data.Maybe (Maybe (..), fromMaybe, mapMaybe)
import Data.Ord (Ord)
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.IsList (IsList (..))
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)
import Prelude ((<>), (>))
import Prelude qualified as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate (
  Mono,
  Poly,
  degM,
  evalMonomial,
  evalPolynomial,
  mono,
  poly,
  var,
  variables,
 )
import ZkFold.Algebra.Polynomial.Multivariate.Monomial (mapVar)
import ZkFold.Algebra.Polynomial.Multivariate.Monomial qualified as Mon
import ZkFold.ArithmeticCircuit.Var (LinVar (..), NewVar (..), Var, toVar)
import ZkFold.Data.Binary (toByteString)
import ZkFold.Prelude (length, take)

data PlonkConstraint i a = PlonkConstraint
  { qm :: a
  , ql :: a
  , qr :: a
  , qo :: a
  , qc :: a
  , x1 :: Var a
  , x2 :: Var a
  , x3 :: Var a
  }
  deriving (Eq, Show)

instance (Ord a, Arbitrary a, Binary a, Semiring a) => Arbitrary (PlonkConstraint i a) where
  arbitrary = do
    qm <- arbitrary
    ql <- arbitrary
    qr <- arbitrary
    qo <- arbitrary
    qc <- arbitrary
    let arbitraryNewVar = toVar . EqVar . toByteString @a <$> arbitrary
    xs <- sort <$> replicateM 3 arbitraryNewVar
    let x1 = head xs; x2 = xs !! 1; x3 = xs !! 2
    return $ PlonkConstraint qm ql qr qo qc x1 x2 x3

toPlonkConstraint :: forall a i. (Ord a, FiniteField a, HasCallStack) => Poly a (Var a) Natural -> PlonkConstraint i a
toPlonkConstraint p = PlonkConstraint qm ql qr qo qc va vb vc
 where
  fail :: P.String -> x
  fail s =
    P.error $
      s
        <> ": not a plonk constraint. Monomials of the following degrees were encountered: "
        <> P.show (fmap (degM . P.snd) $ toList p)

  monomials :: [(a, Mono (Var a) Natural)]
  monomials = toList p

  d2 :: Maybe (a, Mono (Var a) Natural)
  d2 = case filter ((== 2) . degM . P.snd) monomials of
    [] -> Nothing
    [m] -> Just m
    _ -> fail "d2"

  d1', d1, d1Ext :: [(a, Mono (Var a) Natural)]
  d1' = filter ((== 1) . degM . P.snd) monomials
  d1 = if length d1' > 3 then fail "d1" else d1'
  d1Ext = d1 <> [(zero, one), (zero, one), (zero, one)]

  d1VarStats = P.show $ (\m -> fmap (\md -> S.disjoint (Mon.variables $ P.snd md) (Mon.variables $ P.snd m)) d1) <$> d2

  d0 :: Maybe (a, Mono (Var a) Natural)
  d0 = case filter ((== 0) . degM . P.snd) monomials of
    [] -> Nothing
    [m] -> Just m
    _ -> fail "d0"

  qm = fromMaybe zero (P.fst <$> d2)
  qc = fromMaybe zero (P.fst <$> d0)

  d1Coef i = P.fst $ d1Ext !! i

  d1Var i = case S.toList . Mon.variables . P.snd $ d1Ext !! i of
    [] -> ConstVar one
    [v] -> v
    _ -> fail $ "d1Var " <> P.show i

  d1VarCoef v s =
    case filter (S.member v . Mon.variables . P.snd) d1 of
      [] -> zero
      [(coef, _)] -> coef
      _ -> fail s

  d1OutCoef vs s =
    case filter ((\vars -> all (P.flip S.notMember vars) vs) . Mon.variables . P.snd) d1 of
      [] -> (zero, ConstVar one)
      [(coef, m)] -> (coef, head . S.toList . Mon.variables $ m)
      lst ->
        fail
          ( s
              <> ". "
              <> P.show (length lst)
              <> " coefficients after filtering. "
              <> P.show (length d1)
              <> " monomials of degree 1. Var relations: "
              <> d1VarStats
          )

  (ql, qr, qo, va, vb, vc) =
    case d2 of
      -- The polynomial is of the form ql * a + qr * b + qo * c + qc
      Nothing -> (d1Coef 0, d1Coef 1, d1Coef 2, d1Var 0, d1Var 1, d1Var 2)
      Just (_, m) ->
        case S.toList (Mon.variables m) of
          -- The polynomial is of the form qm * a^2 + ql * a + qo * c + qc
          [a] ->
            let ql' = d1VarCoef a "[a] ql'"
                (qo', vc') = d1OutCoef [a] "[a] (qo', vc')"
             in (ql', zero, qo', a, a, vc')
          -- The polynomial is of the form qm * a * b + ql * a + qr * b + qo * c + qc
          [a, b] ->
            let ql' = d1VarCoef a "[a, b] ql'"
                qr' = d1VarCoef b "[a, b] qr'"
                (qo', vc') = d1OutCoef [a, b] "[a, b] (qo', vc')"
             in (ql', qr', qo', a, b, vc')
          _ -> fail "(ql, qr, qo)"

{--
-- | All subsets of three elements of a list without repetitions
nChoose3 :: [a] -> [[a]]
nChoose3 (x:xs) =
    [ [x,y,z] | (y:ys) <- tails xs
              , z      <- ys ]
  ++ nChoose3 xs
nChoose3 _ = []

toPlonkConstraint :: forall a i. (Ord a, FiniteField a) => Poly a (Var a) Natural -> PlonkConstraint i a
toPlonkConstraint p =
  let xs = Just <$> toList (variables p)
      perms = case xs of
        []  -> [[Nothing, Nothing, Nothing]]
        [x] -> [ [Nothing, Nothing, x]
               , [Nothing, x, Nothing]
               , [x, Nothing, Nothing]
               , [x, x, Nothing]
               , [x, Nothing, x]
               , [Nothing, x, x]
               ]
        [_,_] -> concatMap permutations $ nubOrd $ nChoose3 ([Nothing] ++ xs ++ xs)
        _ -> concatMap permutations $ nubOrd $ nChoose3 (xs ++ xs)

      polyCoefs :: [(a, Mono (Maybe (Var a)) Natural)]
      polyCoefs = fmap (mapVar Just) <$> toList p

      getCoef :: Mono (Maybe (Var a)) Natural -> a
      getCoef m = case find ((m ==) . P.snd) polyCoefs of
        Just (c, _) -> c
        _ -> zero

      getCoefs :: [Maybe (Var a)] -> Maybe (PlonkConstraint i a)
      getCoefs [a, b, c] = do
        let xa = mono $ fromList [(a, 1)]
            xb = mono $ fromList [(b, 1)]
            xc = mono $ fromList [(c, 1)]
            xaxb = mono $ fromList [(a, 1), (b, 1)]

            qm = getCoef xaxb
            ql = getCoef xa
            qr = getCoef xb
            qo = getCoef xc
            qc = getCoef one
        guard $
          evalPolynomial evalMonomial (var . Just) p
            - poly
              [ (qm, xaxb)
              , (ql, xa)
              , (qr, xb)
              , (qo, xc)
              , (qc, one)
              ]
            == zero
        let va = fromMaybe (ConstVar one) a
            vb = fromMaybe (ConstVar one) b
            vc = fromMaybe (ConstVar one) c
        return $ PlonkConstraint qm ql qr qo qc va vb vc
      getCoefs _ = Nothing
   in case mapMaybe getCoefs perms of
        [] -> trace ("ZERO CONS" ++ show (fmap (degM . P.snd) $ toList p) ++ "\n") $ toPlonkConstraint zero
        (x:_) -> trace ("VALID CONS" ++ show (fmap (degM . P.snd) $ toList p) ++ "\n") $ x

--}

fromPlonkConstraint :: (Ord a, Field a) => PlonkConstraint i a -> Poly a (Var a) Natural
fromPlonkConstraint (PlonkConstraint qm ql qr qo qc a b c) =
  let xa = var a
      xb = var b
      xc = var c
      xaxb = xa * xb
   in scale qm xaxb
        + scale ql xa
        + scale qr xb
        + scale qo xc
        + fromConstant qc
