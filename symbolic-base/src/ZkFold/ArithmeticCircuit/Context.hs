{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.ArithmeticCircuit.Context where

import Control.Applicative (liftA2, pure)
import Control.DeepSeq (NFData, NFData1, liftRnf, rnf, rwhnf)
import Control.Monad.State (State, modify, runState, state)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.Binary (Binary)
import Data.Bool (Bool (..), otherwise, (&&))
import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Eq ((==))
import Data.Foldable (Foldable, fold, foldl', for_, toList)
import Data.Function (flip, ($), (.))
import Data.Functor (Functor, fmap, (<$>), (<&>))
import Data.Functor.Classes (Show1, liftShowList, liftShowsPrec)
import Data.Functor.Rep
import Data.Kind (Type)
import Data.List.Infinite (Infinite)
import qualified Data.List.Infinite as I
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MM
import Data.Maybe (Maybe (..), fromJust)
import Data.Monoid (Monoid, mempty)
import Data.Ord (Ord)
import Data.Semialign (unzipDefault)
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (Traversable, traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Type.Equality (type (~))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Par1 (..), U1 (..), (:*:) (..))
import Optics (over, set, zoom)
import Text.Show
import qualified Type.Reflection as R
import Prelude (error, seq)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Multivariate (Poly, var)
import ZkFold.ArithmeticCircuit.Lookup (FunctionId (..), LookupType (..))
import ZkFold.ArithmeticCircuit.MerkleHash (MerkleHash (..), merkleHash, runHash)
import ZkFold.ArithmeticCircuit.Var
import ZkFold.ArithmeticCircuit.Witness (WitnessF (..))
import ZkFold.ArithmeticCircuit.WitnessEstimation (Partial (..), UVar (..))
import ZkFold.Control.HApplicative (HApplicative, hliftA2, hpure)
import ZkFold.Data.Binary (fromByteString, toByteString)
import ZkFold.Data.HFunctor (HFunctor, hmap)
import ZkFold.Data.HFunctor.Classes
import ZkFold.Data.Package (Package, packWith, unpackWith)
import ZkFold.Prelude (take)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Fold (SymbolicFold, sfoldl)
import ZkFold.Symbolic.MonadCircuit

-- | The type that represents a constraint in the arithmetic circuit.
type Constraint a = Poly a NewVar Natural

data CircuitFold a
  = forall p s j.
  ( Binary (Rep p)
  , Representable p
  , Traversable s
  , Representable s
  , NFData1 s
  , Binary (Rep s)
  , Representable j
  , Binary (Rep j)
  ) =>
  CircuitFold
  { foldStep
      :: (p :*: s :*: j) NewVar
      -> (CircuitContext a s, p (CircuitWitness a))
  , foldSeed :: s (Var a)
  , foldSeedP :: p (CircuitWitness a)
  , foldStream :: Infinite (j (CircuitWitness a))
  , foldCount :: Var a
  }

instance NFData a => NFData (CircuitFold a) where
  rnf CircuitFold {..} = rnf foldCount `seq` liftRnf rnf foldSeed

data LookupFunction a
  = forall f g.
    (Representable f, Traversable g, Typeable f, Typeable g, Binary (Rep f)) =>
    LookupFunction (forall x. ResidueField x => f x -> g x)

instance NFData (LookupFunction a) where
  rnf = rwhnf

type FunctionRegistry a = Map ByteString (LookupFunction a)

appendFunction
  :: forall f g a
   . (Representable f, Typeable f, Binary (Rep f))
  => (Traversable g, Typeable g, Arithmetic a)
  => (forall x. ResidueField x => f x -> g x)
  -> FunctionRegistry a
  -> (FunctionId (f a -> g a), FunctionRegistry a)
appendFunction f r =
  let functionId = runHash @(Just (Order a)) $ sum (f $ tabulate merkleHash)
   in (FunctionId functionId, M.insert functionId (LookupFunction f) r)

lookupFunction
  :: forall f g (a :: Type)
   . (Typeable f, Typeable g)
  => FunctionRegistry a
  -> FunctionId (f a -> g a)
  -> (forall x. ResidueField x => f x -> g x)
lookupFunction m (FunctionId i) = case m M.! i of
  LookupFunction f -> cast1 . f . cast1
 where
  cast1 :: forall h k b. (Typeable h, Typeable k) => h b -> k b
  cast1 x
    | Just R.HRefl <- th `R.eqTypeRep` tk = x
    | otherwise = error "types are not equal"
   where
    th = R.typeRep :: R.TypeRep h
    tk = R.typeRep :: R.TypeRep k

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
  , acFold :: Map ByteString (CircuitFold a)
  -- ^ The set of folding operations
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

instance (NFData a, NFData1 o) => NFData (CircuitContext a o) where
  rnf = hliftRnf liftRnf

instance NFData a => HNFData (CircuitContext a) where
  hliftRnf r (CircuitContext s lf l w f o) =
    rnf (s, lf, l, w, f) `seq` r rnf o

instance (Show a, Show1 o) => Show (CircuitContext a o) where
  showsPrec = hliftShowsPrec liftShowsPrec liftShowList

-- TODO: make it more readable
instance Show a => HShow (CircuitContext a) where
  hliftShowsPrec f _ _ r =
    showString "CircuitContext "
      . showString "{ acSystem = "
      . shows (acSystem r)
      . showString "\n, acLookup = "
      . shows (acLookup r)
      . showString "\n, acOutput = "
      . f showsPrec showList 0 (acOutput r)
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
          acFold = M.empty
          acLookupFunction = M.empty
      pure CircuitContext {..}

---------------------------- Context constructors ------------------------------

emptyContext :: CircuitContext a U1
emptyContext = CircuitContext M.empty M.empty MM.empty M.empty M.empty U1

crown :: CircuitContext a g -> f (Var a) -> CircuitContext a f
crown = flip (set #acOutput)

fool :: (Functor f, Semiring a) => f NewVar -> CircuitContext a f
fool = crown emptyContext . fmap toVar

behead :: CircuitContext a f -> (CircuitContext a U1, f (Var a))
behead = liftA2 (,) (set #acOutput U1) acOutput

--------------------------------- Variables ------------------------------------

getAllVars :: CircuitContext a o -> [NewVar]
getAllVars ac =
  fmap EqVar (M.keys $ acWitness ac)
    <> M.foldMapWithKey ((. keys) . fmap . FoldLVar) (acFold ac)
 where
  keys :: CircuitFold a -> [ByteString]
  keys CircuitFold {..} =
    toList $ imapRep (\r _ -> toByteString r) foldSeed

--------------------------------- Evaluation -----------------------------------

allWitnesses
  :: Arithmetic a => CircuitContext a o -> (ByteString -> a) -> (NewVar -> a)
allWitnesses ctx inputs =
  let evNewVar = \case
        EqVar eqV -> M.findWithDefault (inputs eqV) eqV eqVars
        FoldLVar fldID fldV -> fst (foldVars M.! fldID) M.! fldV
        FoldPVar fldID fldV -> snd (foldVars M.! fldID) fldV
      evVar = evalVar evNewVar
      evWitness k = runWitnessF k evNewVar
      fromBS :: Binary b => ByteString -> b
      fromBS = fromJust . fromByteString
      eqVars = evWitness <$> acWitness ctx
      foldVars =
        acFold ctx <&> \CircuitFold {..} ->
          let foldCnt = toConstant (evVar foldCount)
              foldList = take foldCnt (I.toList foldStream)
              (resultL, resultP) =
                foldl'
                  ( \(xs, xp) y ->
                      let (stepL, stepP) =
                            foldStep $
                              tabulate (EqVar . toByteString)
                          xj = evWitness <$> y
                          getW = index (xp :*: xs :*: xj)
                          wg = allWitnesses stepL (getW . fromBS)
                       in ( evalVar wg <$> acOutput stepL
                          , stepP <&> flip runWitnessF wg
                          )
                  )
                  (evVar <$> foldSeed, evWitness <$> foldSeedP)
                  foldList
           in ( M.fromList $ toList $ mzipRep (tabulate toByteString) resultL
              , index resultP . fromBS
              )
   in evNewVar

--------------------------- Symbolic compiler context --------------------------

instance HFunctor (CircuitContext a) where
  hmap = over #acOutput

instance Ord a => HApplicative (CircuitContext a) where
  hpure = crown mempty
  hliftA2 f (behead -> (c, o)) (behead -> (d, p)) = crown (c <> d) (f o p)

instance Ord a => Package (CircuitContext a) where
  unpackWith f (behead -> (c, o)) = crown c <$> f o
  packWith f (unzipDefault . fmap behead -> (cs, os)) = crown (fold cs) (f os)

instance (Arithmetic a, Binary a) => Symbolic (CircuitContext a) where
  type BaseField (CircuitContext a) = a
  type WitnessField (CircuitContext a) = CircuitWitness a
  witnessF (behead -> (_, o)) = at <$> o
  fromCircuitF (behead -> (c, o)) f =
    uncurry (set #acOutput) (runState (f o) c)

instance (Arithmetic a, Binary a) => SymbolicFold (CircuitContext a) where
  sfoldl
    fun
    (behead -> (sc, foldSeed))
    foldSeedP
    streamHash
    foldStream
    (behead -> (cc, Par1 foldCount)) =
      let foldStep (p :*: f :*: g) = fun (fool f) (fmap pure p) (fool g)
          fldID =
            runHash $
              merkleHash
                ( acOutput . fst . foldStep $ tabulate (EqVar . toByteString)
                , foldSeed
                , acOutput streamHash
                , foldCount
                )
          (resultC :*: resultP) =
            tabulate
              ( \case
                  Left v -> LinVar one (FoldLVar fldID (toByteString v)) zero
                  Right v -> LinVar one (FoldPVar fldID (toByteString v)) zero
              )
          fc = emptyContext {acFold = M.singleton fldID CircuitFold {..}}
       in ((sc <> cc <> fc) {acOutput = resultC}, at <$> resultP)

-------------------------------- Compiler API ----------------------------------

guessOutput
  :: (Arithmetic a, Binary a, Representable o, Foldable o)
  => (i NewVar -> CircuitContext a o)
  -> (i :*: o) NewVar
  -> CircuitContext a U1
guessOutput f (i :*: o) = fromCircuit2F (f i) (fool o) \o1 o2 -> do
  for_ (mzipRep o1 o2) \(j, k) -> constraint (\x -> x j - x k)
  pure U1

----------------------------- MonadCircuit instance ----------------------------

instance
  (Arithmetic a, Binary a)
  => MonadCircuit (Var a) a (CircuitWitness a) (State (CircuitContext a o))
  where
  unconstrained wf = case runWitnessF wf (\sV -> LinUVar one sV zero) of
    ConstUVar c -> pure (ConstVar c)
    LinUVar k x b -> pure (LinVar k x b)
    More -> do
      let v = witToVar @a wf
      -- TODO: forbid reassignment of variables
      zoom #acWitness $ modify (M.insert v wf)
      pure $ toVar (EqVar v)

  constraint p =
    let evalMaybe = \case
          ConstVar cV -> Known cV
          _ -> Unknown
     in case p evalMaybe of
          Known c ->
            if c == zero
              then pure ()
              else error "The constraint is non-zero"
          Unknown ->
            zoom #acSystem . modify $
              M.insert (witToVar (p at)) (p $ evalVar var)

  lookupConstraint vars lt = do
    vs <- traverse prepare (toList vars)
    zoom #acLookup . modify $
      MM.insertWith S.union (LookupType lt) (S.singleton vs)
    pure ()
   where
    prepare (LinVar k x b) | k == one && b == zero = pure x
    prepare src = do
      let w = at src
          b = witToVar @a w
          v = EqVar b
      zoom #acWitness $ modify (M.insert b w)
      constraint (($ toVar v) - ($ src))
      pure v

  registerFunction f = zoom #acLookupFunction $ state (appendFunction f)

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
witToVar :: forall a. (Finite a, Binary a) => WitnessF a NewVar -> ByteString
witToVar (WitnessF w) = runHash @(Just (Order a)) $ w $ \case
  EqVar eqV -> M eqV
  FoldLVar fldID fldV -> merkleHash (fldID, False, fldV)
  FoldPVar fldID fldV -> merkleHash (fldID, True, fldV)
