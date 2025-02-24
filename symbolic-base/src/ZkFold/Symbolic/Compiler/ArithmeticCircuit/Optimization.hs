
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

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.Polynomials.Multivariate            (evalMonomial)
import           ZkFold.Base.Algebra.Polynomials.Multivariate.Monomial   (Mono (..), oneM)
import           ZkFold.Base.Algebra.Polynomials.Multivariate.Polynomial (Poly (..), evalPolynomial, var)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Instance     ()
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Witness      (WitnessF (..))

--------------------------------- High-level functions --------------------------------

-- | Replaces linear polynoms of the form
-- @(fromConstant k) * (NewVar nV) + (fromConstant c)@
-- with a constant variable @fromConstant $ negate c // k@ in an arithmetic circuit
-- and replaces variable with a constant in witness
--
optimize :: forall a p i o.
  (Arithmetic a, Ord (Rep i), Functor o, Binary (Rep i), Binary a, Binary (Rep p)) =>
  ArithmeticCircuit a p i o -> ArithmeticCircuit a p i o
optimize (ArithmeticCircuit s lf r w f o) = ArithmeticCircuit {
    acSystem = addInVarConstraints newS,
    acLookupFunction = lf,
    acLookup = optRanges vs r,
    acWitness = (>>= optWitVar vs) <$>
      M.filterWithKey (\k _ -> notMember (NewVar (EqVar k)) vs) w,
    acFold = optimizeFold . bimap varF (>>= optWitVar vs) <$> f,
    acOutput = varF <$> o
  }
  where
    (newS, vs) = varsToReplace (s, M.empty)

    isInVar (InVar _) = True
    isInVar _         = False

    addInVarConstraints :: Map ByteString (Poly a (SysVar i) Natural) -> Map ByteString (Poly a (SysVar i) Natural)
    addInVarConstraints p = p <> fromList [(polyId, poly) | (inVar, v) <- assocs $ filterWithKey (const . isInVar) vs,
                                                            let poly = var inVar - fromConstant v,
                                                            let polyId = witToVar @a @p @i (pure (WSysVar inVar) - fromConstant v)]

    optRanges :: Map (SysVar i) a -> MM.MonoidalMap (LookupType a) (S.Set [SysVar i]) -> MM.MonoidalMap (LookupType a) (S.Set [SysVar i])
    optRanges m = MM.mapMaybeWithKey (\k' v -> bool Nothing (maybeSet v $ fromRange k') (isRange k'))
      where
        maybeSet :: S.Set [SysVar i] -> S.Set (a, a) -> Maybe (S.Set [SysVar i])
        maybeSet v k = bool (error "range constraint less then value")
                            (let t = S.difference v (S.map (: []) (keysSet m))
                              in if null t then Nothing else Just t) (all (inInterval k) $ restrictKeys (mapKeys (: []) m :: Map [SysVar i] a) v)

    inInterval :: S.Set (a, a) -> a -> Bool
    inInterval si v = and $ S.map (\(l', r') -> ((v >= l') && (v <= r')) :: Bool) si

    optWitVar :: Map (SysVar i) a -> WitVar p i -> WitnessF a (WitVar p i)
    optWitVar m = \case
      (WSysVar sv) ->
        case M.lookup sv m of
          Just k  -> fromConstant k
          Nothing -> pure $ WSysVar sv
      we  -> pure we

    optimizeFold CircuitFold {..} =
      CircuitFold { foldStep = optimize foldStep, .. }

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
