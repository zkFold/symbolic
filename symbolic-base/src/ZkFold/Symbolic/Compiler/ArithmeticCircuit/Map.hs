{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Map (
        mapVarArithmeticCircuit,
    ) where

import           Data.Functor                                        ((<&>))
import           Data.Functor.Rep                                    (Representable (..))
import           Data.Map                                            hiding (drop, foldl, foldr, fromList, map, null,
                                                                      splitAt, take, toList)
import qualified Data.Map                                            as Map
import qualified Data.Set                                            as Set
import           GHC.IsList                                          (IsList (..))
import           Numeric.Natural                                     (Natural)
import           Prelude                                             hiding (Num (..), drop, length, product, splitAt,
                                                                      sum, take, (!!), (^))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Polynomials.Multivariate
import           ZkFold.Base.Data.ByteString                         (toByteString)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal (ArithmeticCircuit (..), SysVar (..), Var (..),
                                                                      VarField, WitVar (..), getAllVars)

-- This module contains functions for mapping variables in arithmetic circuits.

mapVarArithmeticCircuit ::
  (Field a, Eq a, Functor o, Ord (Rep i), Representable i, Foldable i) =>
  ArithmeticCircuit a p i o -> ArithmeticCircuit a p i o
mapVarArithmeticCircuit ac =
    let vars = [v | NewVar v <- getAllVars ac]
        asc = [ toByteString @VarField (fromConstant @Natural x) | x <- [0..] ]
        forward = Map.fromAscList $ zip vars asc
        backward = Map.fromAscList $ zip asc vars
        varF (InVar v)  = InVar v
        varF (NewVar v) = NewVar (forward ! v)
        witF (WSysVar v) = WSysVar (varF v)
        witF (WExVar v)  = WExVar v
     in ArithmeticCircuit
          { acRange   = Set.map varF <$> acRange ac
          , acSystem  = fromList $ zip asc $ evalPolynomial evalMonomial (var . varF) <$> elems (acSystem ac)
          , acWitness = (`Map.compose` backward) $ fmap witF <$> acWitness ac
          , acOutput  = acOutput ac <&> \case
              SysVar v -> SysVar (varF v)
              ConstVar c -> ConstVar c
          }
