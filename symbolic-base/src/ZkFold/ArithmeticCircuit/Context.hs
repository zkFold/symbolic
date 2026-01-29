{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.ArithmeticCircuit.Context where

import Control.Applicative (liftA2, pure, (<*>))
import Control.DeepSeq (NFData, rnf, rwhnf)
import Control.Monad.State (State, modify, runState, state)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (bimap)
import Data.Binary (Binary)
import Data.Bool ((&&))
import Data.ByteString (ByteString)
import Data.Eq (Eq, (==))
import Data.Foldable (Foldable, fold, for_, toList)
import Data.Function (flip, ($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Functor.Classes (Show1, liftShowsPrec)
import Data.Functor.Rep
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MM
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid, mempty)
import Data.Ord (Ord)
import Data.Semialign (unzipDefault)
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set (Set, singleton)
import qualified Data.Set as S
import Data.Traversable (Traversable, traverse)
import Data.Tuple (uncurry)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Par1 (..), U1 (..), (:*:) (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Optics (over, set, zoom)
import Text.Show
import Prelude (error)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Multivariate (Poly, var)
import ZkFold.ArithmeticCircuit.MerkleHash (MerkleHash (..), merkleHash, runHash)
import ZkFold.ArithmeticCircuit.Var
import ZkFold.ArithmeticCircuit.Witness (WitnessF (..))
import ZkFold.ArithmeticCircuit.WitnessEstimation (Partial (..), UVar (..))
import ZkFold.Data.FromList (FromList, fromList)
import ZkFold.Symbolic.Class (Arithmetic, LookupTable (..))

-- | The type that represents a constraint in the arithmetic circuit.
type Constraint a = Poly a NewVar Natural

newtype LookupFunction a = LookupFunction
  { runLookupFunction
      :: forall x. (PrimeField x, 3 <= Order x, Algebra a x) => [x] -> [x]
  }

instance NFData (LookupFunction a) where
  rnf = rwhnf

type FunctionRegistry a = Map ByteString (LookupFunction a)

appendFunction
  :: forall f g a
   . (Representable f, FromList f, Binary (Rep f))
  => (Foldable g, Arithmetic a, Binary a)
  => (forall x. (PrimeField x, 3 <= Order x, Algebra a x) => f x -> g x)
  -> FunctionRegistry a
  -> (ByteString, FunctionRegistry a)
appendFunction f r =
  let functionId = runHash @(Just (Order a)) $ sum (f $ tabulate merkleHash)
   in (functionId, M.insert functionId (LookupFunction (toList . \x -> f $ fromList x)) r)

data LookupType a
  = LTRanges (Set (a, a))
  | LTProduct (LookupType a) (LookupType a)
  | LTPlot ByteString (LookupType a)
  deriving
    ( Aeson.FromJSON
    , Aeson.FromJSONKey
    , Aeson.ToJSON
    , Aeson.ToJSONKey
    , Eq
    , Generic
    , NFData
    , Ord
    , Show
    )

asRange :: LookupType a -> Maybe (Set (a, a))
asRange (LTRanges rs) = Just rs
asRange _ = Nothing

-- | Circuit context in the form of a system of polynomial constraints.
data CircuitContext a o = CircuitContext
  { acSystem :: Map ByteString (Constraint a)
  -- ^ The system of polynomial constraints
  , acLookupFunction :: FunctionRegistry a
  -- ^ The set of lookup functions
  , acLookup :: MonoidalMap (LookupType a) (Set [NewVar])
  -- ^ The lookup constraints for the selected variables
  , acWitness :: Map ByteString (CircuitWitness a)
  -- ^ The witness generation functions
  , acOutput :: o (Var a)
  -- ^ The output variables
  }
  deriving Generic

deriving via
  (GenericSemigroupMonoid (CircuitContext a o))
  instance
    (Ord a, o ~ U1) => Semigroup (CircuitContext a o)

deriving via
  (GenericSemigroupMonoid (CircuitContext a o))
  instance
    (Ord a, o ~ U1) => Monoid (CircuitContext a o)

instance (NFData a, NFData (o (Var a))) => NFData (CircuitContext a o)

instance (Show a, Show1 o) => Show (CircuitContext a o) where
  showsPrec _ r =
    showString "CircuitContext "
      . showString "{ acSystem = "
      . shows (acSystem r)
      . showString "\n, acLookup = "
      . shows (acLookup r)
      . showString "\n, acOutput = "
      . liftShowsPrec showsPrec showList 0 (acOutput r)
      . showString " }"

-- TODO: add witness generation info to the JSON object
instance
  (Aeson.ToJSON a, Aeson.ToJSONKey a, Aeson.ToJSON1 o)
  => Aeson.ToJSON (CircuitContext a o)
  where
  toJSON CircuitContext {..} =
    Aeson.object
      [ "system" .= acSystem
      , "lookup" .= acLookup
      , Aeson.explicitToField Aeson.toJSON1 "output" acOutput
      ]

-- TODO: properly restore the witness generation function
instance
  (Ord a, Aeson.FromJSON a, Aeson.FromJSONKey a, Aeson.FromJSON1 o)
  => Aeson.FromJSON (CircuitContext a o)
  where
  parseJSON =
    Aeson.withObject "ArithmeticCircuit" $ \v -> do
      acSystem <- v .: "system"
      acLookup <- v .: "lookup"
      acOutput <- Aeson.explicitParseField Aeson.parseJSON1 v "output"
      let acWitness = M.empty
          acLookupFunction = M.empty
      pure CircuitContext {..}

---------------------------- Context constructors ------------------------------

emptyContext :: CircuitContext a U1
emptyContext = CircuitContext M.empty M.empty MM.empty M.empty U1

crown :: CircuitContext a g -> f (Var a) -> CircuitContext a f
crown = flip (set #acOutput)

fool :: (Functor f, Semiring a) => f NewVar -> CircuitContext a f
fool = crown emptyContext . fmap toVar

behead :: CircuitContext a f -> (CircuitContext a U1, f (Var a))
behead = liftA2 (,) (set #acOutput U1) acOutput

--------------------------------- Variables ------------------------------------

getAllVars :: CircuitContext a o -> [NewVar]
getAllVars ac = M.keys (acWitness ac)

--------------------------------- Evaluation -----------------------------------

allWitnesses
  :: Arithmetic a => CircuitContext a o -> (ByteString -> a) -> (NewVar -> a)
allWitnesses ctx inputs =
  let evNewVar eqV = M.findWithDefault (inputs eqV) eqV eqVars
      evWitness k = runWitnessF k evNewVar
      eqVars = evWitness <$> acWitness ctx
   in evNewVar

--------------------------- Symbolic compiler context --------------------------

hmap :: (forall i. f i -> g i) -> CircuitContext a f -> CircuitContext a g
hmap = over #acOutput

hpure :: Ord a => (forall i. f i) -> CircuitContext a f
hpure = crown mempty

hliftA2
  :: Ord a
  => (forall i. f i -> g i -> h i)
  -> CircuitContext a f
  -> CircuitContext a g
  -> CircuitContext a h
hliftA2 f (behead -> (c, o)) (behead -> (d, p)) = crown (c <> d) (f o p)

unpackWith
  :: Functor g
  => (forall i. f i -> g (h i)) -> CircuitContext a f -> g (CircuitContext a h)
unpackWith f (behead -> (c, o)) = crown c <$> f o

packWith
  :: (Ord a, Foldable f, Functor f)
  => (forall i. f (g i) -> h i) -> f (CircuitContext a g) -> CircuitContext a h
packWith f (unzipDefault . fmap behead -> (cs, os)) = crown (fold cs) (f os)

witnessF :: Functor o => CircuitContext a o -> o (CircuitWitness a)
witnessF (behead -> (_, o)) = at <$> o

fromCircuitF
  :: CircuitContext a i
  -> (i (Var a) -> State (CircuitContext a U1) (o (Var a)))
  -> CircuitContext a o
fromCircuitF (behead -> (c, o)) f = uncurry (set #acOutput) (runState (f o) c)

-------------------------------- Compiler API ----------------------------------

guessOutput
  :: (Arithmetic a, Binary a, Representable o, Foldable o)
  => (i NewVar -> CircuitContext a o)
  -> (i :*: o) NewVar
  -> CircuitContext a U1
guessOutput f (i :*: o) =
  fromCircuitF (hliftA2 (:*:) (f i) (fool o)) \(o1 :*: o2) -> do
    for_ (mzipRep o1 o2) \(j, k) -> constraint (\x -> x j - x k)
    pure U1

----------------------------- MonadCircuit instance ----------------------------

unconstrained
  :: forall a o
   . (Arithmetic a, Binary a)
  => CircuitWitness a -> State (CircuitContext a o) (Var a)
unconstrained wf = case runWitnessF wf (\sV -> LinUVar one sV zero) of
  ConstUVar c -> pure (ConstVar c)
  LinUVar k x b -> pure (LinVar k x b)
  More -> do
    let v = witToVar @a wf
    -- TODO: forbid reassignment of variables
    zoom #acWitness $ modify (M.insert v wf)
    pure (toVar v)

constraint
  :: (Arithmetic a, Binary a, HasCallStack)
  => (forall b. Algebra a b => (Var a -> b) -> b)
  -> State (CircuitContext a o) ()
constraint p =
  let evalMaybe = \case
        ConstVar cV -> Known cV
        _ -> Unknown
   in case p evalMaybe of
        Known c ->
          if c == zero
            then pure ()
            else
              error
                ( "The constraint is non-zero at\n"
                    <> prettyCallStack callStack
                )
        Unknown ->
          zoom #acSystem . modify $ M.insert (witToVar (p at)) (p $ evalVar var)

newConstrained
  :: (Arithmetic a, Binary a, HasCallStack)
  => (forall b. Algebra a b => (Var a -> b) -> Var a -> b)
  -> CircuitWitness a
  -> State (CircuitContext a o) (Var a)
newConstrained p w = do
  i <- unconstrained w
  constraint (`p` i)
  pure i

newAssigned
  :: (Arithmetic a, Binary a, HasCallStack)
  => (forall b. Algebra a b => (Var a -> b) -> b)
  -> State (CircuitContext a o) (Var a)
newAssigned p = newConstrained (\x i -> p x - x i) (p at)

lookupConstraint
  :: forall f a o
   . (Arithmetic a, Binary a, Traversable f)
  => f (Var a) -> LookupTable f -> State (CircuitContext a o) ()
lookupConstraint vars ltable = do
  vs <- traverse prepare (toList vars)
  lt <- lookupType ltable
  zoom #acLookup . modify $ MM.insertWith S.union lt (S.singleton vs)
  pure ()
 where
  prepare (LinVar k x b) | k == one && b == zero = pure x
  prepare src = do
    let w = at src
        v = witToVar @a w
    zoom #acWitness $ modify (M.insert v w)
    constraint (($ toVar v) - ($ src))
    pure v

rangeConstraint
  :: (Arithmetic a, Binary a)
  => Var a -> Natural -> State (CircuitContext a o) ()
rangeConstraint v 1 = constraint \w -> w v * (one - w v)
rangeConstraint v upperBound =
  lookupConstraint (Par1 v) . Ranges $ singleton (zero, upperBound)

newRanged
  :: (Arithmetic a, Binary a)
  => Natural -> CircuitWitness a -> State (CircuitContext a o) (Var a)
newRanged upperBound witness = do
  v <- unconstrained witness
  rangeConstraint v upperBound
  pure v

-- | Translates a lookup table into a lookup type,
-- storing all lookup functions in a circuit.
lookupType
  :: (Arithmetic a, Binary a)
  => LookupTable f -> State (CircuitContext a o) (LookupType a)
lookupType (Ranges rs) = pure $ LTRanges (S.map (bimap fromConstant fromConstant) rs)
lookupType (Product t u) = LTProduct <$> lookupType t <*> lookupType u
lookupType (Plot f t) = do
  funcId <- zoom #acLookupFunction $ state (appendFunction f)
  LTPlot funcId <$> lookupType t

-- | Generates new variable index given a witness for it.
--
-- It is a root hash (sha256) of a Merkle tree which is obtained from witness:
--
-- 1. Due to parametricity, the only operations inside witness are
--    operations from 'WitnessField' interface;
--
-- 2. Thus witness can be viewed as an AST of a 'WitnessField' "language" where:
--
--     * leafs are 'fromConstant' calls and variables;
--     * nodes are algebraic operations;
--     * root is the witness value for new variable.
--
-- 3. To inspect this AST, we instantiate witness with a special inspector type
--    whose 'WitnessField' instances perform inspection.
--
-- 4. Inspector type used here, 'MerkleHash', treats AST as a Merkle tree and
--    performs the calculation of hashes for it.
--
-- 5. Thus the result of running the witness with 'MerkleHash' as a
--    'WitnessField' is a root hash of a Merkle tree for a witness.
witToVar
  :: forall a
   . (Finite a, Prime (Order a), Binary a)
  => WitnessF a NewVar -> ByteString
witToVar (WitnessF w) = runHash @(Just (Order a)) (w M)
