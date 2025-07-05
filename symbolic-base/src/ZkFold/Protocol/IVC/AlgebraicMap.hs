{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.AlgebraicMap (algebraicMap) where

import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Foldable (Foldable, toList)
import Data.Functor.Rep (Representable (..))
import Data.List (foldl', (++))
import Data.Map.Strict (Map, keys, union)
import qualified Data.Map.Strict as M
import GHC.Generics ((:*:))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Multivariate
import qualified ZkFold.Algebra.Polynomial.Multivariate as PM
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (acContext))
import ZkFold.ArithmeticCircuit.Context (acSystem, acWitness)
import ZkFold.ArithmeticCircuit.Var (NewVar (..))
import ZkFold.Data.ByteString (Binary, toByteString)
import ZkFold.Data.Vector (Vector)
import qualified ZkFold.Data.Vector as V
import ZkFold.Protocol.IVC.Predicate (Predicate (..))
import Prelude (fmap, zip, ($), (.), (<$>))
import qualified Prelude as P

-- | Algebraic map of @a@.
-- It calculates a system of equations defining @a@ in some way.
-- The inputs are polymorphic in a ring element @f@.
-- The main application is to define the verifier's algebraic map in the NARK protocol.
algebraicMap
  :: forall d k a i p f
   . ( KnownNat (d + 1)
     , Representable i
     , Foldable i
     , Representable p
     , Foldable p
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
  witness =
    M.fromList $
      zip
        ( toList (tabulate @(i :*: p :*: i) toByteString)
            ++ keys (acWitness $ acContext predicateCircuit)
        )
        (V.head pm)

  pubInput :: Map ByteString f
  pubInput =
    M.fromList $
      zip (toList (tabulate @i $ toByteString @(Either (Rep i) (Either (Rep p) (Rep i))) . Right . Right)) (toList pi)

  varMap :: NewVar -> f
  varMap (EqVar newV) = M.findWithDefault (P.error "unknown variable") newV $ pubInput `union` witness
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
  => f
  -> V.Vector (d + 1) [f]
  -> [f]
padDecomposition pad = foldl' (P.zipWith (+)) (P.repeat zero) . V.mapWithIx (\j p -> ((pad ^ (d -! j)) *) <$> p)
 where
  d = value @(d + 1) -! 1

-- | Decomposes an algebraic map into homogenous degree-j maps for j from 0 to @d@
degreeDecomposition
  :: forall d f v. KnownNat (d + 1) => [Poly f v Natural] -> V.Vector (d + 1) [Poly f v Natural]
degreeDecomposition lmap = tabulate (degree_j . toConstant)
 where
  degree_j :: Natural -> [Poly f v Natural]
  degree_j j = P.fmap (homogenous j) lmap
