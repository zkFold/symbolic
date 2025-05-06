{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Relation where

import           Data.Binary                                         (Binary)
import           Data.Bool                                           (bool)
import           Data.Constraint                                     (withDict)
import           Data.Constraint.Nat                                 (timesNat)
import           Data.Foldable                                       (toList)
import           Data.Functor.Rep                                    (Rep, Representable, tabulate)
import           Data.Kind                                           (Type)
import           Data.Map                                            (elems)
import qualified Data.Map.Monoidal                                   as M
import           Data.Maybe                                          (fromJust, mapMaybe)
import qualified Data.Set                                            as S
import qualified Data.Vector                                         as V
import           GHC.Generics                                        ((:*:) (..))
import           GHC.IsList                                          (fromList)
import           Prelude                                             hiding (Num (..), drop, length, replicate, sum,
                                                                      take, (!!), (/), (^))
import           Test.QuickCheck                                     (Arbitrary (..))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Permutation                          (Permutation, fromCycles, mkIndexPartition)
import           ZkFold.Algebra.Polynomial.Multivariate              (evalMonomial, evalPolynomial, var)
import           ZkFold.Algebra.Polynomial.Univariate                (UnivariateRingPolyVec (..), toPolyVec)
import           ZkFold.Prelude                                      (iterateN', length, replicate)
import           ZkFold.Protocol.Plonkup.Internal                    (PlonkupPermutationSize)
import           ZkFold.Protocol.Plonkup.LookupConstraint            (LookupConstraint (..))
import           ZkFold.Protocol.Plonkup.PlonkConstraint             (PlonkConstraint (..), toPlonkConstraint)
import           ZkFold.Protocol.Plonkup.PlonkupConstraint
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var      (toVar)

-- Here `n` is the total number of constraints, `i` is the number of inputs to the circuit, and `a` is the field type.
data PlonkupRelation i o (p :: Type -> Type) n a pv = PlonkupRelation
    { qM       :: pv n
    , qL       :: pv n
    , qR       :: pv n
    , qO       :: pv n
    , qC       :: pv n
    , qK       :: pv n
    , t1       :: pv n
    , t2       :: pv n
    , t3       :: pv n
    , sigma    :: Permutation (3 * n)
    , witness  :: i a -> (pv n, pv n, pv n)
    , pubInput :: i a -> o a
    , cNum     :: Natural
    -- ^ The actual number of constraints in the relation.
    }

instance (Show a, Show (pv n)) => Show (PlonkupRelation i o p n a pv) where
    show PlonkupRelation {..} =
        "Plonkup Relation: "
        ++ show qM ++ " "
        ++ show qL ++ " "
        ++ show qR ++ " "
        ++ show qO ++ " "
        ++ show qC ++ " "
        ++ show qK ++ " "
        ++ show t1 ++ " "
        ++ show t2 ++ " "
        ++ show t3 ++ " "
        ++ show sigma

instance
        ( KnownNat n
        , UnivariateRingPolyVec a pv
        , KnownNat (PlonkupPermutationSize n)
        , Representable i
        , Representable o
        , Foldable o
        , Foldable p
        , Ord (Rep i)
        , Arithmetic a
        , Binary a
        , Arbitrary (ArithmeticCircuit a i (o :*: p))
        ) => Arbitrary (PlonkupRelation i o p n a pv) where
    arbitrary = fromJust . toPlonkupRelation @i @o @p @n @a @pv <$> arbitrary

toPlonkupRelation ::
  forall i o p n a pv .
  ( KnownNat n, Arithmetic a, Binary a, Ord (Rep i), UnivariateRingPolyVec a pv
  , Representable i, Representable o, Foldable o, Foldable p
  ) => ArithmeticCircuit a i (o :*: p) -> Maybe (PlonkupRelation i o p n a pv)
toPlonkupRelation ac =
    let n = value @n

        (xPub :*: xPrv)     = acOutput ac
        pubInputConstraints = map var (toList xPub)
        prvInputConstraints = map var (toList xPrv)
        plonkConstraints    = map (evalPolynomial evalMonomial (var . toVar)) (elems (acSystem ac))
        rs :: [a] = concat . mapMaybe (\rc -> bool Nothing (Just . toList . S.map snd $ fromRange rc) (isRange rc)) . M.keys $ acLookup ac
        -- Number of elements in the set `t`.
        nLookup = toConstant $ bool zero (head rs + one) (not $ null rs)

        -- TODO: We are expecting at most one range.
        t1 = toPolyVec $ fromList $ replicate (value @n -! nLookup) zero ++ iterateN' nLookup ((+) one) zero
        t2 = toPolyVec $ fromList []
        t3 = toPolyVec $ fromList []

        -- Lookup queries.
        xLookup :: [SysVar i] = concat . concatMap S.toList $ M.elems (acLookup ac)

        cNum = acSizeN ac + length (tabulate @o id) + length xLookup

        plonkupSystem = fromList $ concat
            [ map (ConsPlonk . toPlonkConstraint) (pubInputConstraints ++ plonkConstraints)
            , ConsLookup . LookupConstraint <$> xLookup
            , replicate (n -! cNum) ConsExtra
            ]

        qM = toPolyVec $ fmap (qm . getPlonkConstraint) plonkupSystem
        qL = toPolyVec $ fmap (ql . getPlonkConstraint) plonkupSystem
        qR = toPolyVec $ fmap (qr . getPlonkConstraint) plonkupSystem
        qO = toPolyVec $ fmap (qo . getPlonkConstraint) plonkupSystem
        qC = toPolyVec $ fmap (qc . getPlonkConstraint) plonkupSystem
        qK = toPolyVec $ fmap isLookupConstraint plonkupSystem

        cNum' = cNum + length prvInputConstraints
        plonkupSystem' = fromList $ concat
            [ map (ConsPlonk . toPlonkConstraint) (pubInputConstraints ++ plonkConstraints)
            , ConsLookup . LookupConstraint <$> xLookup
            , map (ConsPlonk . toPlonkConstraint) prvInputConstraints
            , replicate (n -! cNum') ConsExtra
            ]

        a  = fmap getA plonkupSystem'
        b  = fmap getB plonkupSystem'
        c  = fmap getC plonkupSystem'
        -- TODO: Permutation code is not particularly safe. We rely on the list being of length 3*n.
        sigma = withDict (timesNat @3 @n) (fromCycles @(3*n) $ mkIndexPartition $ V.concat [a, b, c])

        w1 i = toPolyVec $ fmap (indexW ac i) a
        w2 i = toPolyVec $ fmap (indexW ac i) b
        w3 i = toPolyVec $ fmap (indexW ac i) c
        witness i  = (w1 i, w2 i, w3 i)
        pubInput i = fmap (indexW ac i) xPub

    in if max cNum' nLookup <= n
        then Just $ PlonkupRelation {..}
        else Nothing
