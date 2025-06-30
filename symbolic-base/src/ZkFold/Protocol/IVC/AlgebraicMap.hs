{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.AlgebraicMap (algebraicMap) where

import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Functor.Rep (Representable (..))
import Data.List (foldl')
import Data.Map.Strict (Map, keys)
import qualified Data.Map.Strict as M
import Data.Maybe (Maybe (..))
import Prelude (fmap, zip, ($), (.), (<$>))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Multivariate
import qualified ZkFold.Algebra.Polynomial.Multivariate as PM
import ZkFold.Data.ByteString (Binary, fromByteString)
import ZkFold.Data.Vector (Vector)
import qualified ZkFold.Data.Vector as V
import ZkFold.Protocol.IVC.Predicate (Predicate (..))
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (ArithmeticCircuit (acContext))
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (acSystem, acWitness)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var (NewVar (..))
import ZkFold.Data.Eq

-- | Algebraic map of @a@.
-- It calculates a system of equations defining @a@ in some way.
-- The inputs are polymorphic in a ring element @f@.
-- The main application is to define the verifier's algebraic map in the NARK protocol.
algebraicMap
  :: forall d k a i p f
   . ( KnownNat (d + 1)
     , Representable i
     , Binary (Rep i)
     , Binary (Rep p)
     , Ring f
     , Scale a f
     )
  => Predicate a i p
  -> i f
  -> Vector k [f]
  -> Vector (k - 1) f
  -> f
  -> [f]
algebraicMap Predicate {..} pi pm _ pad = padDecomposition pad f_sps_uni
 where
  sys :: [PM.Poly a NewVar Natural]
  sys = M.elems (acSystem $ acContext predicateCircuit)

  witness :: Map ByteString f
  witness = M.fromList $ zip (keys $ acWitness $ acContext predicateCircuit) (V.head pm)

  asInput :: ByteString -> f
  asInput v = case fromByteString v :: Maybe (Either (Rep i) (Either (Rep p) (Rep i))) of
    Just (Left inV) -> index pi inV
    Just (Right (Left _)) -> P.error "constraints should not depend on payload"
    Just (Right (Right _)) -> zero
    Nothing -> P.error "unknown variable"

  varMap :: NewVar -> f
  varMap (EqVar newV) = M.findWithDefault (asInput newV) newV witness
  varMap (FoldLVar _ _) = P.error "unexpected FOLD constraint"
  varMap (FoldPVar _ _) = P.error "unexpected FOLD constraint"

  f_sps :: Vector (d + 1) [PM.Poly a NewVar Natural]
  f_sps = degreeDecomposition @d $ sys

  f_sps_uni :: Vector (d + 1) [f]
  f_sps_uni = fmap (PM.evalPolynomial PM.evalMonomial varMap) <$> f_sps

padDecomposition
  :: forall d f
   . ( MultiplicativeMonoid f
     , AdditiveMonoid f
     , KnownNat (d + 1)
     )
  => f -> V.Vector (d + 1) [f] -> [f]
padDecomposition pad = foldl' (P.zipWith (+)) (P.repeat zero) . V.mapWithIx (\j p -> ((pad ^ (d -! j)) *) <$> p)
 where
  d = value @(d + 1) -! 1

-- | Decomposes an algebraic map into homogenous degree-j maps for j from 0 to @d@
degreeDecomposition :: forall d f v. KnownNat (d + 1) => [Poly f v Natural] -> V.Vector (d + 1) [Poly f v Natural]
degreeDecomposition lmap = tabulate (degree_j . toConstant)
 where
  degree_j :: Natural -> [Poly f v Natural]
  degree_j j = P.fmap (leaveDeg j) lmap

  leaveDeg :: Natural -> PM.Poly f v Natural -> PM.Poly f v Natural
  leaveDeg j (PM.P monomials) = PM.P $ P.filter (\(_, m) -> deg m == j) monomials

deg :: PM.Mono v Natural -> Natural
deg (PM.M m) = sum m
