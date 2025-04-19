module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Optimization where

import           Data.Bifunctor                                          (bimap)
import           Data.Binary                                             (Binary)
import           Data.Bool                                               (bool)
import           Data.ByteString                                         (ByteString)
import           Data.Functor.Rep                                        (Representable (..))
import           Data.Map                                                hiding (drop, foldl, foldr, map, null, splitAt,
                                                                          take)
import qualified Data.Map.Internal                                       as M
import qualified Data.Map.Monoidal                                       as MM
import qualified Data.Set                                                as S
import           Prelude                                                 hiding (Num (..), drop, length, product,
                                                                          splitAt, sum, take, (!!), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Multivariate            (evalMonomial)
import           ZkFold.Algebra.Polynomial.Multivariate.Monomial   (Mono (..), oneM)
import           ZkFold.Algebra.Polynomial.Multivariate.Internal (Poly (..), evalPolynomial, var)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Instance     ()
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup

--------------------------------- High-level functions --------------------------------

-- | Replaces linear polynoms of the form
-- @(fromConstant k) * (NewVar nV) + (fromConstant c)@
-- with a constant variable @fromConstant $ negate c // k@ in an arithmetic circuit
-- and replaces variable with a constant in witness
--
optimize :: forall a i o.
  (Arithmetic a, Ord (Rep i), Functor o, Binary (Rep i), Binary a) =>
  ArithmeticCircuit a i o -> ArithmeticCircuit a i o
optimize (ArithmeticCircuit s lf r w f o) = ArithmeticCircuit {
    acSystem = addInVarConstraints newS,
    acLookupFunction = lf,
    acLookup = optRanges vs r,
    acWitness = (>>= optSysVar) <$>
      M.filterWithKey (\k _ -> notMember (NewVar (EqVar k)) vs) w,
    acFold = optimizeFold . bimap varF (>>= optSysVar) <$> f,
    acOutput = varF <$> o
  }
  where
    (newS, vs) = varsToReplace (s, M.empty)

    isInVar (InVar _) = True
    isInVar _         = False

    addInVarConstraints :: Map ByteString (Poly a (SysVar i) Natural) -> Map ByteString (Poly a (SysVar i) Natural)
    addInVarConstraints p = p <> fromList [(polyId, poly) | (inVar, v) <- assocs $ filterWithKey (const . isInVar) vs,
                                                            let poly = var inVar - fromConstant v,
                                                            let polyId = witToVar @a @i (pure inVar - fromConstant v)]

    optRanges :: Map (SysVar i) a -> MM.MonoidalMap (LookupType a) (S.Set [SysVar i]) -> MM.MonoidalMap (LookupType a) (S.Set [SysVar i])
    optRanges m = MM.mapMaybeWithKey (\k' v -> bool Nothing (maybeSet v $ fromRange k') (isRange k'))
      where
        maybeSet :: S.Set [SysVar i] -> S.Set (a, a) -> Maybe (S.Set [SysVar i])
        maybeSet v k = bool (error "range constraint less then value")
                            (let t = S.difference v (S.map (: []) (keysSet m))
                              in if null t then Nothing else Just t) (all (inInterval k) $ restrictKeys (mapKeys (: []) m :: Map [SysVar i] a) v)

    inInterval :: S.Set (a, a) -> a -> Bool
    inInterval si v = and $ S.map (\(l', r') -> ((v >= l') && (v <= r')) :: Bool) si

    optimizeFold CircuitFold {..} =
      CircuitFold { foldStep = optimize foldStep, .. }

    optSysVar sV = maybe (pure sV) fromConstant (M.lookup sV vs)

    varF lv@(LinVar k sV b) = maybe lv (ConstVar . (\t -> k * t + b)) (M.lookup sV vs)
    varF (ConstVar c)       = ConstVar c


varsToReplace :: (Arithmetic a, Ord (Rep i)) => (Map ByteString (Constraint a i) , Map (SysVar i) a) -> (Map ByteString (Constraint a i) , Map (SysVar i) a)
varsToReplace (s, l) = if newVars == M.empty then (s, l) else varsToReplace (M.filter (/= zero) $ optimizeSystems newVars s, M.union newVars l)
  where
    newVars = M.fromList . M.elems $ mapMaybe toConstVar s

    optimizeSystems :: (Arithmetic a, Ord (Rep i)) => Map (SysVar i) a -> Map ByteString (Constraint a i) -> Map ByteString (Constraint a i)
    optimizeSystems m as = bool (error "unsatisfiable constraint") ns (all checkZero ns)
      where
        ns = evalPolynomial evalMonomial varF <$> as
        varF p = maybe (var p) fromConstant (M.lookup p m)
        checkZero (P [(c, mx)]) = (c == zero) && oneM mx || not (oneM mx)
        checkZero _             = True

    toConstVar :: Arithmetic a => Constraint a i -> Maybe (SysVar i, a)
    toConstVar = \case
      P [(_, M m1)] -> case toList m1 of
        [(m1var, 1)] -> Just (m1var, zero)
        _            -> Nothing
      P [(c, M m1), (k, M m2)] ->
        if oneM (M m1)
          then case toList m2 of
            [(m2var, 1)] -> Just (m2var, negate c // k)
            _            -> Nothing
          else if oneM (M m2)
            then case toList m1 of
              [(m1var, 1)] -> Just (m1var, negate k // c)
              _            -> Nothing
            else Nothing
      _ -> Nothing
