module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Map (
        mapVarArithmeticCircuit,
    ) where

import           Data.Bifunctor                                      (bimap)
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
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal

-- This module contains functions for mapping variables in arithmetic circuits.

mapVarArithmeticCircuit ::
  (Field a, Eq a, Functor o, Ord (Rep i), Representable i, Foldable i) =>
  ArithmeticCircuit a i o -> ArithmeticCircuit a i o
mapVarArithmeticCircuit ac =
    let vars = [v | NewVar (EqVar v) <- getAllVars ac]
        asc = [ toByteString @VarField (fromConstant @Natural x) | x <- [0..] ]
        forward = Map.fromAscList $ zip vars asc
        backward = Map.fromAscList $ zip asc vars
        varF (InVar v)                     = InVar v
        varF (NewVar (EqVar v))            = NewVar (EqVar (forward ! v))
        -- | TODO: compress fold ids, too
        varF (NewVar (FoldLVar fldId fldV)) = NewVar (FoldLVar fldId fldV)
        varF (NewVar (FoldPVar fldId fldV)) = NewVar (FoldPVar fldId fldV)
        oVarF (LinVar k v b) = LinVar k (varF v) b
        oVarF (ConstVar c)   = ConstVar c
     in ArithmeticCircuit
          { acLookup   = Set.map (map varF) <$> acLookup ac
          , acLookupFunction = acLookupFunction ac
          , acSystem  =
              fromList $ zip asc $ evalPolynomial evalMonomial (var . varF)
              <$> elems (acSystem ac)
          , acWitness = (fmap varF <$> acWitness ac) `Map.compose` backward
          , acFold = bimap oVarF (fmap varF) <$> acFold ac
          , acOutput  = oVarF <$> acOutput ac
          }
