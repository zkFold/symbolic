module ZkFold.Protocol.Plonkup.PlonkConstraint where

import Control.Monad (guard, replicateM, return)
import Data.Binary (Binary)
import Data.Containers.ListUtils (nubOrd)
import Data.Eq (Eq (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (find, head, map, permutations, sort, (!!), (++))
import Data.Maybe (Maybe (..), fromMaybe, mapMaybe)
import Data.Ord (Ord)
import GHC.IsList (IsList (..))
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))
import Text.Show (Show)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate (
  Mono,
  Poly,
  evalMonomial,
  evalPolynomial,
  mono,
  poly,
  var,
  variables,
 )
import ZkFold.Algebra.Polynomial.Multivariate.Monomial (mapVar)
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

toPlonkConstraint :: forall a i. (Ord a, FiniteField a) => Poly a (Var a) Natural -> PlonkConstraint i a
toPlonkConstraint p =
  let xs = Just <$> toList (variables p)
      perms = nubOrd $ map (take 3) $ permutations $ case length xs of
        0 -> [Nothing, Nothing, Nothing]
        1 -> [Nothing, Nothing, head xs, head xs]
        2 -> [Nothing] ++ xs ++ xs
        _ -> xs ++ xs

      getCoef :: Mono (Maybe (Var a)) Natural -> a
      getCoef m = case find (\(_, as) -> m == mapVar Just as) (toList p) of
        Just (c, _) -> c
        _ -> zero

      getCoefs :: [Maybe (Var a)] -> Maybe (PlonkConstraint i a)
      getCoefs [a, b, c] = do
        let xa = [(a, 1)]
            xb = [(b, 1)]
            xc = [(c, 1)]
            xaxb = xa ++ xb

            qm = getCoef $ mono $ fromList xaxb
            ql = getCoef $ mono $ fromList xa
            qr = getCoef $ mono $ fromList xb
            qo = getCoef $ mono $ fromList xc
            qc = getCoef one
        guard $
          evalPolynomial evalMonomial (var . Just) p
            - poly
              [ (qm, mono $ fromList xaxb)
              , (ql, mono $ fromList xa)
              , (qr, mono $ fromList xb)
              , (qo, mono $ fromList xc)
              , (qc, one)
              ]
            == zero
        let va = fromMaybe (ConstVar one) a
            vb = fromMaybe (ConstVar one) b
            vc = fromMaybe (ConstVar one) c
        return $ PlonkConstraint qm ql qr qo qc va vb vc
      getCoefs _ = Nothing
   in case mapMaybe getCoefs perms of
        [] -> toPlonkConstraint zero
        _ -> head $ mapMaybe getCoefs perms

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
