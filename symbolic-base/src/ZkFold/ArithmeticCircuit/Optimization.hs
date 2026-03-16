{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.ArithmeticCircuit.Optimization (optimize, isInputVar) where

import Control.Applicative (pure)
import Control.Monad (Monad, (>>=))
import Data.Binary (Binary)
import Data.Bool (Bool (..), bool, not, otherwise, (&&), (||))
import Data.ByteString (ByteString)
import Data.Eq ((/=), (==))
import Data.Foldable (Foldable, all, any, foldMap, foldl')
import Data.Function (const, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Functor.Rep (Rep)
import Data.List (filter, length)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.Maybe (Maybe (..), isJust, maybe)
import Data.Ord ((<), (<=), (>))
import Data.Semigroup ((<>))
import Data.Set (Set, findMin)
import qualified Data.Set as S
import Data.Tuple (fst, snd)
import GHC.Generics ((:*:))
import Prelude (error)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate (degM, degP, evalMonomial, evalPolynomial, homogenous, lt, poly, var, variables)
import ZkFold.ArithmeticCircuit.Context (
  CircuitContext (..),
  CircuitFold (..),
  Constraint,
  LookupType,
  asRange,
  witToVar,
 )
import ZkFold.ArithmeticCircuit.Var (NewVar (..), Var, CircuitWitness, LinVar (..), (.+))
import ZkFold.Data.Binary (fromByteString)
import ZkFold.Symbolic.Class (Arithmetic)

-- | @optimize keep ctx@ eliminates variables from the constraint system.
--
-- Pass 1: resolves constraints of the form @k * x + c == 0@ (single-variable).
-- Pass 2: resolves degree-1 constraints with any number of variables,
-- expressing one variable as a linear combination of the others.
-- For output variables, only 2-variable constraints are used (LinVar limitation).
--
-- For an example of a suitable @keep@ predicate, take a look at 'isInputVar'.
optimize
  :: forall a o
   . (Arithmetic a, Binary a, Foldable o, Functor o)
  => (NewVar -> Bool)
  -> CircuitContext a o
  -> CircuitContext a o
optimize keep = optimizeLinear keep . optimizeConst keep

-- | Pass 1: Eliminate variables that equal a constant (k * x + c == 0).
optimizeConst
  :: forall a o
   . (Arithmetic a, Binary a, Functor o)
  => (NewVar -> Bool)
  -> CircuitContext a o
  -> CircuitContext a o
optimizeConst keep (CircuitContext s lf lc w f o) =
  let (newSystem, consts) = constVarsToReplace (s, M.empty)
      prune :: (Monad e, FromConstant a (e NewVar)) => e NewVar -> e NewVar
      prune = (>>= \v -> maybe (pure v) fromConstant (consts M.!? v))
   in CircuitContext
        { acSystem = newSystem <> constInputConstraints @a keep consts
        , acLookupFunction = lf
        , acLookup = optRanges consts lc
        , acWitness =
            prune <$> filterKeys ((`M.notMember` consts) . EqVar) w
        , acFold = optimizeFoldConst <$> f
        , acOutput = prune <$> o
        }
 where
  optRanges
    :: Map NewVar a
    -> MM.MonoidalMap (LookupType a) (Set [NewVar])
    -> MM.MonoidalMap (LookupType a) (Set [NewVar])
  -- FIXME removes all constraints except range constraints
  optRanges m = MM.mapMaybeWithKey (\k' v -> asRange k' >>= maybeSet v)
   where
    maybeSet :: Set [NewVar] -> Set (a, a) -> Maybe (Set [NewVar])
    maybeSet v k =
      bool
        (error "range constraint less then value")
        ( let t = S.difference v (S.map (: []) (M.keysSet m))
           in if S.null t then Nothing else Just t
        )
        (all (inInterval k) $ M.restrictKeys (M.mapKeys (: []) m) v)

  inInterval :: Set (a, a) -> a -> Bool
  inInterval si v = any (\(l', r') -> (l' <= v) && (v <= r')) si

  optimizeFoldConst :: CircuitFold a -> CircuitFold a
  optimizeFoldConst CircuitFold {..} =
    CircuitFold
      { foldStep = \(i :: (p :*: s :*: j) NewVar) ->
          let (ctx, pl) = foldStep i
           in (optimize (isInputVar @(p :*: s :*: j)) ctx, pl)
      , ..
      }

-- | Pass 2: Eliminate variables from degree-1 constraints with N variables.
--
-- For constraints with 2 variables: can eliminate any non-kept variable,
-- including output variables (LinVar can represent k*y + c).
--
-- For constraints with 3+ variables: can only eliminate non-kept,
-- non-output variables (LinVar cannot represent multi-variable expressions).
--
-- This eliminates:
-- * y = c*x (MDS constant multiplication, 2-var)
-- * y = x + c (round constant addition, 2-var)
-- * z = a*x + b*y + c (MDS accumulation, 3-var) — the big win
optimizeLinear
  :: forall a o
   . (Arithmetic a, Binary a, Foldable o, Functor o)
  => (NewVar -> Bool)
  -> CircuitContext a o
  -> CircuitContext a o
optimizeLinear keep (CircuitContext s lf lc w f o) =
  let outVars = outputVarSet o
      (newSystem, repls) = genLinVarsToReplace keep outVars (s, M.empty)
      -- Prune witnesses: WitnessF supports arbitrary polynomial expressions
      pruneW :: CircuitWitness a -> CircuitWitness a
      pruneW = (>>= \v -> case repls M.!? v of
        Nothing -> pure v
        Just (terms, c) ->
          foldl' (\acc (k, y) -> acc + fromConstant k * pure y) (fromConstant c) terms)
      -- Prune outputs: LinVar only supports k*y + c (single variable)
      -- N-var replacements should never reach here (output vars are protected)
      pruneO :: Var a -> Var a
      pruneO = (>>= \v -> case repls M.!? v of
        Nothing -> pure v
        Just ([(k, y)], c) -> c .+ scale k (pure y)
        Just _ -> pure v)
   in CircuitContext
        { acSystem = newSystem <> genLinInputConstraints @a keep repls
        , acLookupFunction = lf
        , acLookup = lc
        , acWitness =
            pruneW <$> filterKeys ((`M.notMember` repls) . EqVar) w
        , acFold = optimizeFoldLinear <$> f
        , acOutput = pruneO <$> o
        }
 where
  optimizeFoldLinear :: CircuitFold a -> CircuitFold a
  optimizeFoldLinear CircuitFold {..} =
    CircuitFold
      { foldStep = \(i :: (p :*: s :*: j) NewVar) ->
          let (ctx, pl) = foldStep i
           in (optimize (isInputVar @(p :*: s :*: j)) ctx, pl)
      , ..
      }

isInputVar :: forall i. Binary (Rep i) => NewVar -> Bool
isInputVar (EqVar v) = isJust (fromByteString v :: Maybe (Rep i))
isInputVar _ = False

filterKeys :: (k -> Bool) -> Map k v -> Map k v
filterKeys f = M.filterWithKey (const . f)

-- | Collect all variables referenced by the circuit output.
outputVarSet :: Foldable o => o (Var a) -> Set NewVar
outputVarSet = foldMap go
 where
  go (LinVar _ v _) = S.singleton v
  go (ConstVar _) = S.empty

---------------------------------------------------------------------
-- Pass 1: Constant variable elimination (k * x + c == 0)
---------------------------------------------------------------------

constVarsToReplace
  :: forall a
   . Arithmetic a
  => (Map ByteString (Constraint a), Map NewVar a)
  -> (Map ByteString (Constraint a), Map NewVar a)
constVarsToReplace (s, l)
  | newVars == M.empty = (s, l)
  | otherwise =
      constVarsToReplace
        (M.filter (/= zero) (optimizeSystemsConst newVars s), newVars <> l)
 where
  newVars =
    M.fromList [assoc | (toConstVar -> Just assoc) <- M.elems s]

  optimizeSystemsConst
    :: Map NewVar a
    -> Map ByteString (Constraint a)
    -> Map ByteString (Constraint a)
  optimizeSystemsConst m as
    | all checkZero ns = ns
    | otherwise = error "unsatisfiable constraint"
   where
    ns = evalPolynomial evalMonomial varF <$> as
    varF p = maybe (var p) fromConstant (M.lookup p m)
    checkZero p = degP p > zero || fst (lt p) == zero

  toConstVar :: Constraint a -> Maybe (NewVar, a)
  toConstVar p =
    let (c, m) = lt p
        p' = p - poly [lt p]
     in if c /= zero && degM m == one && degP p' == zero
          then Just (findMin $ variables p, negate $ fst (lt p') // c)
          else Nothing

constInputConstraints
  :: forall a
   . (Arithmetic a, Binary a)
  => (NewVar -> Bool)
  -> Map NewVar a
  -> Map ByteString (Constraint a)
constInputConstraints keep vs =
  M.fromList
    [ (pId, p)
    | (inVar, v) <- M.assocs $ filterKeys keep vs
    , let p = var inVar - fromConstant v
    , let pId = witToVar @a (pure inVar - fromConstant v)
    ]

---------------------------------------------------------------------
-- Pass 2: General N-variable linear elimination
---------------------------------------------------------------------

-- | Map from eliminated variable to ([(coeff, otherVar)], constant),
-- meaning: eliminatedVar = sum(coeff_i * var_i) + constant.
type GenLinRepl a = Map NewVar ([(a, NewVar)], a)

-- | Iteratively find and eliminate variables from degree-1 constraints.
genLinVarsToReplace
  :: forall a
   . Arithmetic a
  => (NewVar -> Bool)
  -> Set NewVar
  -> (Map ByteString (Constraint a), GenLinRepl a)
  -> (Map ByteString (Constraint a), GenLinRepl a)
genLinVarsToReplace keep outVars (s, l)
  | M.null newVars = (s, l)
  | otherwise =
      genLinVarsToReplace keep outVars
        (M.filter (/= zero) (optimizeSystemsGenLin newVars s), newVars <> l)
 where
  -- Variables appearing in any degree-2 monomial across the constraint system.
  -- These cannot receive replacements: even single-term replacements with
  -- non-zero constant create extra degree-1 monomials from the degree-2 part.
  d2vars :: Set NewVar
  d2vars = foldMap (\p -> variables (homogenous (one + one) p)) s

  -- Reverse index: variable -> list of constraint IDs containing it.
  -- Used to check Plonk gate format safety before elimination.
  varIndex :: Map NewVar [ByteString]
  varIndex = M.foldlWithKey' (\acc cId c ->
    foldl' (\m v -> M.insertWith (<>) v [cId] m) acc (S.toList (variables c))
    ) M.empty s

  -- Build replacement map, then sanitize: no replacement may reference
  -- another eliminated variable. This prevents exponential blowup when
  -- substituting into degree-2 constraints and avoids dangling variable
  -- references in witnesses.
  newVars :: GenLinRepl a
  newVars = sanitize $
    M.fromList [assoc | (cId, c) <- M.assocs s
                      , Just assoc <- [toGenLinVar cId c]]

  sanitize :: GenLinRepl a -> GenLinRepl a
  sanitize m = M.filter (\(terms, _) -> all ((`M.notMember` m) . snd) terms) m

  -- | Check that substituting a 2-term replacement for v won't create > 3
  -- degree-1 monomials in any constraint (except source).
  -- A 2-term replacement replaces 1 degree-1 monomial with 2, so each
  -- target constraint must have at most 2 degree-1 monomials.
  plonkSafe :: NewVar -> ByteString -> Bool
  plonkSafe v srcId = case M.lookup v varIndex of
    Nothing -> True
    Just cIds -> all (\cId -> cId == srcId ||
      case M.lookup cId s of
        Nothing -> True
        Just c -> S.size (variables (homogenous one c)) <= 2
      ) cIds

  -- | Extract a variable to eliminate from a degree-1 constraint.
  -- Restrictions:
  -- * Variable must not be kept (input variable).
  -- * For 2-var constraints (single-term replacement): d2vars excluded because
  --   replacing v with k*y+c in a degree-2 monomial v*u produces k*y*u+c*u,
  --   adding an extra degree-1 term (c*u) that could exceed Plonk's gate limit.
  --   Output vars OK (replacement fits in LinVar).
  -- * For 3-var constraints (2-term replacement): must not be in d2vars
  --   (would create 2 degree-2 monomials), must not be output var,
  --   and all other constraints containing the variable must have ≤ 2
  --   degree-1 monomials (Plonk gate format: max 3 degree-1 monomials).
  -- * For 4+ var constraints: skip (replacement would exceed Plonk limits).
  toGenLinVar :: ByteString -> Constraint a -> Maybe (NewVar, ([(a, NewVar)], a))
  toGenLinVar cId p
    | degP p /= one = Nothing
    | nVars < 2 = Nothing
    | nVars == 2 =
        case filter (\(_, v) -> canElim2 v) terms of
          [] -> Nothing
          ((cv, v) : _) -> mkRepl cv v
    | nVars == 3 =
        case filter (\(_, v) -> canElim3 v) terms of
          [] -> Nothing
          ((cv, v) : _) -> mkRepl cv v
    | otherwise = Nothing
   where
    (terms, c0) = extractLinearTerms p
    nVars = length terms
    canElim2 v = not (keep v) && not (S.member v d2vars)
    canElim3 v = not (keep v) && not (S.member v d2vars)
      && not (S.member v outVars) && plonkSafe v cId
    mkRepl cv v =
      let others = filter ((/= v) . snd) terms
          repl = ([(negate ck // cv, vk) | (ck, vk) <- others], negate c0 // cv)
       in Just (v, repl)

  -- | Substitute general linear replacements into the constraint system.
  optimizeSystemsGenLin
    :: GenLinRepl a
    -> Map ByteString (Constraint a)
    -> Map ByteString (Constraint a)
  optimizeSystemsGenLin m as
    | all checkZero ns = ns
    | otherwise = error "unsatisfiable constraint (linear)"
   where
    ns = evalPolynomial evalMonomial varF <$> as
    varF p = case M.lookup p m of
      Nothing -> var p
      Just (terms, c) ->
        foldl' (\acc (k, y) -> acc + fromConstant k * var y) (fromConstant c) terms
    checkZero p = degP p > zero || fst (lt p) == zero

-- | Extract all linear terms from a degree-1 polynomial.
-- Returns ([(coefficient, variable)], constant) where terms are in
-- polynomial ordering (leading/largest variable first).
extractLinearTerms :: Arithmetic a => Constraint a -> ([(a, NewVar)], a)
extractLinearTerms p =
  let deg1 = homogenous one p
      c0 = fst (lt (homogenous zero p))
   in (go deg1, c0)
 where
  go q
    | q == zero = []
    | otherwise =
        let (c, m) = lt q
            v = findMin (variables q)
            rest = q - poly [(c, m)]
         in (c, v) : go rest

-- | Re-add constraints for kept (input) variables that were eliminated.
genLinInputConstraints
  :: forall a
   . (Arithmetic a, Binary a)
  => (NewVar -> Bool)
  -> GenLinRepl a
  -> Map ByteString (Constraint a)
genLinInputConstraints keep vs =
  M.fromList
    [ (pId, p)
    | (inVar, (terms, c)) <- M.assocs $ filterKeys keep vs
    , let p = var inVar
              - foldl' (\acc (k, y) -> acc + fromConstant k * var y) (fromConstant c) terms
    , let pId = witToVar @a
              ( pure inVar
              - foldl' (\acc (k, y) -> acc + fromConstant k * pure y) (fromConstant c) terms
              )
    ]
