{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Relation where

import           Data.Binary                                         (Binary)
import           Data.Bool                                           (bool)
import           Data.Constraint                                     (withDict)
import           Data.Constraint.Nat                                 (timesNat)
import           Data.Foldable                                       (toList)
import           Data.Functor.Rep                                    (Rep, Representable, tabulate)
import           Data.Map                                            (elems)
import qualified Data.Map.Monoidal                                   as M
import           Data.Maybe                                          (fromJust, mapMaybe)
import qualified Data.Set                                            as S
import qualified Data.Vector                                         as V
import           GHC.IsList                                          (fromList)
import           Prelude                                             hiding (Num (..), drop, length, replicate, sum,
                                                                      take, (!!), (/), (^))
import           Test.QuickCheck                                     (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.Basic.Permutations              (Permutation, fromCycles, mkIndexPartition)
import           ZkFold.Base.Algebra.Polynomials.Multivariate        (evalMonomial, evalPolynomial, var)
import           ZkFold.Base.Algebra.Polynomials.Univariate          (UnivariateRingPolyVec (..), toPolyVec)
import           ZkFold.Base.Protocol.Plonkup.Internal               (PlonkupPermutationSize)
import           ZkFold.Base.Protocol.Plonkup.LookupConstraint       (LookupConstraint (..))
import           ZkFold.Base.Protocol.Plonkup.PlonkConstraint        (PlonkConstraint (..), toPlonkConstraint)
import           ZkFold.Base.Protocol.Plonkup.PlonkupConstraint
import           ZkFold.Prelude                                      (length, replicate)
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var      (toVar)
import GHC.Generics ((:*:) (..))

-- Here `n` is the total number of constraints, `i` is the number of inputs to the circuit, and `a` is the field type.
data PlonkupRelation i n w l a pv = PlonkupRelation
    { qM       :: pv n
    , qL       :: pv n
    , qR       :: pv n
    , qO       :: pv n
    , qC       :: pv n
    , qK       :: pv n
    , t        :: pv n
    , sigma    :: Permutation (3 * n)
    , witness  :: i a -> (pv n, pv n, pv n)
    , prvInput :: i a -> w a
    , pubInput :: i a -> l a
    }

instance (Show a, Show (pv n)) => Show (PlonkupRelation i n w l a pv) where
    show PlonkupRelation {..} =
        "Plonkup Relation: "
        ++ show qM ++ " "
        ++ show qL ++ " "
        ++ show qR ++ " "
        ++ show qO ++ " "
        ++ show qC ++ " "
        ++ show qK ++ " "
        ++ show t ++ " "
        ++ show sigma

instance
        ( KnownNat n
        , UnivariateRingPolyVec a pv
        , KnownNat (PlonkupPermutationSize n)
        , Representable i
        , Representable w
        , Representable l
        , Foldable w
        , Foldable l
        , Ord (Rep i)
        , Arithmetic a
        , Binary a
        , Arbitrary (ArithmeticCircuit a i (w :*: l))
        ) => Arbitrary (PlonkupRelation i n w l a pv) where
    arbitrary = fromJust . toPlonkupRelation @i @n @w @l @a @pv <$> arbitrary

toPlonkupRelation ::
  forall i n w l a pv .
  ( KnownNat n, Arithmetic a, Binary a, Ord (Rep i), UnivariateRingPolyVec a pv
  , Representable i, Representable w, Representable l, Foldable w, Foldable l
  ) => ArithmeticCircuit a i (w :*: l) -> Maybe (PlonkupRelation i n w l a pv)
toPlonkupRelation ac =
    let xPrv :*: xPub       = acOutput ac
        prvInputConstraints = map var (toList xPrv)
        pubInputConstraints = map var (toList xPub)
        plonkConstraints    = map (evalPolynomial evalMonomial (var . toVar)) (elems (acSystem ac))
        rs :: [Natural] = concat . mapMaybe (\rc -> bool Nothing (Just . toList . S.map (toConstant . snd) $ fromRange rc) (isRange rc)) . M.keys $ acLookup ac
        -- TODO: We are expecting at most one range.
        t = toPolyVec $ fromList $ map fromConstant $ bool [] (replicate (value @n -! length rs + 1) 0 ++ [ 0 .. head rs ]) (not $ null rs)
        -- Number of elements in the set `t`.
        nLookup = bool 0 (head rs + 1) (not $ null rs)
        -- Lookup queries.
        xLookup :: [SysVar i] = concat . concatMap S.toList $ M.elems (acLookup ac)

        -- The total number of constraints in the relation.
        n'      = acSizeN ac + length (tabulate @l id) + length xLookup

        plonkupSystem = fromList $ concat
            [ map (ConsPlonk . toPlonkConstraint) (prvInputConstraints ++ pubInputConstraints ++ plonkConstraints)
            , ConsLookup . LookupConstraint <$> xLookup
            , replicate (value @n -! n') ConsExtra
            ]

        qM = toPolyVec $ fmap (qm . getPlonkConstraint) plonkupSystem
        qL = toPolyVec $ fmap (ql . getPlonkConstraint) plonkupSystem
        qR = toPolyVec $ fmap (qr . getPlonkConstraint) plonkupSystem
        qO = toPolyVec $ fmap (qo . getPlonkConstraint) plonkupSystem
        qC = toPolyVec $ fmap (qc . getPlonkConstraint) plonkupSystem
        qK = toPolyVec $ fmap isLookupConstraint plonkupSystem

        a  = fmap getA plonkupSystem
        b  = fmap getB plonkupSystem
        c  = fmap getC plonkupSystem
        -- TODO: Permutation code is not particularly safe. We rely on the list being of length 3*n.
        sigma = withDict (timesNat @3 @n) (fromCycles @(3*n) $ mkIndexPartition $ V.concat [a, b, c])

        w1 i = toPolyVec $ fmap (indexW ac i) a
        w2 i = toPolyVec $ fmap (indexW ac i) b
        w3 i = toPolyVec $ fmap (indexW ac i) c
        witness i  = (w1 i, w2 i, w3 i)
        prvInput i = fmap (indexW ac i) xPrv
        pubInput i = fmap (indexW ac i) xPub

    in if max n' nLookup <= value @n
        then Just $ PlonkupRelation {..}
        else Nothing
