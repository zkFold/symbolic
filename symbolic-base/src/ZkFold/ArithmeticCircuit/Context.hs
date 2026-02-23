{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module ZkFold.ArithmeticCircuit.Context where

import Control.Applicative (liftA2, pure, (<*>))
import Control.DeepSeq (NFData, NFData1, liftRnf, rnf, rwhnf)
import Control.Monad.State (State, modify, runState, state)
import Data.Aeson ((.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (bimap)
import Data.Binary (Binary)
import Data.Bool (Bool (..), (&&))
import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Eq (Eq, (==))
import Data.Foldable (Foldable, fold, foldl', for_, toList)
import Data.Function (flip, ($), (.))
import Data.Functor (Functor, fmap, (<$>), (<&>))
import Data.Functor.Classes (Show1, liftShowList, liftShowsPrec)
import Data.Functor.Rep
import Data.List.Infinite (Infinite)
import Data.List.Infinite qualified as I
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MM
import Data.Maybe (Maybe (..), fromJust)
import Data.Monoid (Monoid, mempty)
import Data.Ord (Ord)
import Data.Semialign (unzipDefault)
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Traversable (Traversable, traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Type.Equality (type (~))
import GHC.Generics (Generic, Par1 (..), U1 (..), (:*:) (..))
import GHC.IsList qualified as List
import GHC.Stack (callStack, prettyCallStack)
import Optics (over, set, zoom)
import Text.Show
import Prelude (error, filter, id, length, seq, (>))
import Prelude qualified as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Multivariate (Poly, var)
import ZkFold.Algebra.Polynomial.Multivariate.Monomial qualified as Mon
import ZkFold.ArithmeticCircuit.MerkleHash (MerkleHash (..), merkleHash, runHash)
import ZkFold.ArithmeticCircuit.Var
import ZkFold.ArithmeticCircuit.Witness (WitnessF (..))
import ZkFold.ArithmeticCircuit.WitnessEstimation (Partial (..), UVar (..))
import ZkFold.Control.HApplicative (HApplicative, hliftA2, hpure)
import ZkFold.Data.Binary (fromByteString, toByteString)
import ZkFold.Data.FromList (FromList, fromList)
import ZkFold.Data.HFunctor (HFunctor, hmap)
import ZkFold.Data.HFunctor.Classes
import ZkFold.Data.Package (Package, packWith, unpackWith)
import ZkFold.Prelude (take)
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Fold (SymbolicFold, sfoldl)
import ZkFold.Symbolic.MonadCircuit
import ZkFold.Symbolic.V2 (LookupTable (..))

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

newtype LookupFunction a = LookupFunction
  {runLookupFunction :: forall x. (PrimeField x, Algebra a x) => [x] -> [x]}

instance NFData (LookupFunction a) where
  rnf = rwhnf

type FunctionRegistry a = Map ByteString (LookupFunction a)

appendFunction
  :: forall f g a
   . (Representable f, FromList f, Binary (Rep f))
  => (Foldable g, Arithmetic a, Binary a)
  => (forall x. (PrimeField x, Algebra a x) => f x -> g x)
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

-- | The function below checks that the polynomial is in the form of Plonk constraint
-- It means:
--
-- No more than one monomial of degree 2 (d2)
-- No more than 3 monomials of degree 1 (d1)
-- If there is a monomial of degree 2, there should be no more than one monomial of degree 1 with a variable that is not present in d2
--   (e.g. no cases as  qm * a * a + ql * a + **qr * b** + qo * c + qc -- notice that in d2 there is a^2, not a * b)
decomposePolynomial
  :: forall a v
   . FiniteField a
  => Eq a
  => Ord v
  => Poly a v Natural
  -> ( Maybe (a, Mon.Mono v Natural)
     , [(a, Mon.Mono v Natural)]
     , Maybe (a, Mon.Mono v Natural)
     , Maybe [Bool]
     )
decomposePolynomial p = (d2, d1, d0, d1VarStats)
 where
  fail :: P.String -> x
  fail s =
    P.error $
      s
        <> ": not a plonk constraint. Mon.Monomials of the following degrees were encountered: "
        <> P.show (fmap (Mon.degM . P.snd) $ List.toList p)

  monomials :: [(a, Mon.Mono v Natural)]
  monomials = List.toList p

  -- No more than one monomial of degree 2
  d2 :: Maybe (a, Mon.Mono v Natural)
  d2 = case filter ((== 2) . Mon.degM . P.snd) monomials of
    [] -> Nothing
    [m] -> Just m
    _ -> fail "d2"

  -- No more than three monomials of degree 1
  d1', d1 :: [(a, Mon.Mono v Natural)]
  d1' = filter ((== 1) . Mon.degM . P.snd) monomials
  d1 = if length d1' > 3 then fail "d1" else d1'

  -- No more than one monomial of degree 0
  d0 :: Maybe (a, Mon.Mono v Natural)
  d0 = case filter ((== 0) . Mon.degM . P.snd) monomials of
    [] -> Nothing
    [m] -> Just m
    _ -> fail "d0"

  d1VarStats = (\m -> fmap (\md -> S.disjoint (Mon.variables $ P.snd md) (Mon.variables $ P.snd m)) d1) <$> d2

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

        -- FIXME: These checks are valid for Plonk constraints only.
        -- If we plan to support multiple provers for the same function, we need to remove them.
        -- See https://github.com/zkFold/symbolic/issues/805
        cons :: Constraint a
        cons =
          if varDisjoint > Just 1
            then error $ "Not a plonk constraint: variable relations are " <> show varRelations
            else p $ evalVar var

        (_, _, _, varRelations) = decomposePolynomial @a $ p (evalVar var)

        varDisjoint = fmap (length . filter id) varRelations
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
            zoom #acSystem . modify $
              M.insert (witToVar (p at)) cons

  lookupConstraint vars ltable = do
    vs <- traverse prepare (toList vars)
    lt <- lookupType ltable
    zoom #acLookup . modify $ MM.insertWith S.union lt (S.singleton vs)
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
witToVar (WitnessF w) = runHash @(Just (Order a)) $ w $ \case
  EqVar eqV -> M eqV
  FoldLVar fldID fldV -> merkleHash (fldID, False, fldV)
  FoldPVar fldID fldV -> merkleHash (fldID, True, fldV)
