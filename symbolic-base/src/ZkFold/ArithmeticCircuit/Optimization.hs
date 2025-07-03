{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.ArithmeticCircuit.Optimization (optimize, isInputVar) where

import Control.Applicative (pure)
import Control.Monad (Monad, (>>=))
import Data.Binary (Binary)
import Data.Bool (Bool (..), bool, otherwise, (&&))
import Data.ByteString (ByteString)
import Data.Eq ((/=), (==))
import Data.Foldable (all, any)
import Data.Function (const, ($), (.))
import Data.Functor (Functor, (<$>))
import Data.Functor.Rep (Rep)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.Maybe (Maybe (..), isJust, maybe)
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.Set (Set, findMin)
import qualified Data.Set as S
import Data.Tuple (fst)
import GHC.Generics ((:*:))
import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate (degM, degP, evalMonomial, evalPolynomial, lt, poly, var, variables)
import ZkFold.ArithmeticCircuit.Context (
  CircuitContext (..),
  CircuitFold (..),
  Constraint,
  witToVar,
 )
import ZkFold.ArithmeticCircuit.Lookup (LookupType, asRange)
import ZkFold.ArithmeticCircuit.Var (NewVar (..))
import ZkFold.Data.ByteString (fromByteString)
import ZkFold.Symbolic.Class (Arithmetic)
import Prelude (error)

-- | @optimize keep ctx@ resolves constraints of the form @k * x + c == 0@
-- by dropping such variables @x@ from the @ctx@
-- and replacing @x@ with @negate c // k@ in the system and in the witnesses,
-- excluding variables for which @keep@ returns true.
--
-- For an example of a suitable @keep@ predicate, take a look at 'isInputVar'.
optimize
  :: forall a o
   . (Arithmetic a, Binary a, Functor o)
  => (NewVar -> Bool)
  -> CircuitContext a o
  -> CircuitContext a o
optimize keep (CircuitContext s lf lc w f o) =
  let (newSystem, consts) = varsToReplace (s, M.empty)
      prune :: (Monad e, FromConstant a (e NewVar)) => e NewVar -> e NewVar
      prune = (>>= \v -> maybe (pure v) fromConstant (consts M.!? v))
   in CircuitContext
        { acSystem = newSystem <> inputConstraints consts
        , acLookupFunction = lf
        , acLookup = optRanges consts lc
        , acWitness =
            prune <$> filterKeys ((`M.notMember` consts) . EqVar) w
        , acFold = optimizeFold <$> f
        , acOutput = prune <$> o
        }
 where
  inputConstraints :: Map NewVar a -> Map ByteString (Constraint a)
  inputConstraints vs =
    M.fromList
      [ (pId, p)
      | (inVar, v) <- M.assocs $ filterKeys keep vs
      , let p = var inVar - fromConstant v
      , let pId = witToVar @a (pure inVar - fromConstant v)
      ]

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

  optimizeFold CircuitFold {..} =
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

varsToReplace
  :: forall a
   . Arithmetic a
  => (Map ByteString (Constraint a), Map NewVar a)
  -> (Map ByteString (Constraint a), Map NewVar a)
varsToReplace (s, l)
  | newVars == M.empty = (s, l)
  | otherwise =
      varsToReplace
        (M.filter (/= zero) (optimizeSystems newVars s), newVars <> l)
 where
  newVars =
    M.fromList [assoc | (toConstVar -> Just assoc) <- M.elems s]

  optimizeSystems
    :: Map NewVar a
    -> Map ByteString (Constraint a)
    -> Map ByteString (Constraint a)
  optimizeSystems m as = ns
   where
    ns = evalPolynomial evalMonomial varF <$> as
    varF p = maybe (var p) fromConstant (M.lookup p m)

  toConstVar :: Constraint a -> Maybe (NewVar, a)
  toConstVar p =
    let (c, m) = lt p
        p' = p - poly [lt p]
     in if c /= zero && degM m == one && degP p' == zero
          then Just (findMin $ variables p, negate $ fst (lt p') // c)
          else Nothing
