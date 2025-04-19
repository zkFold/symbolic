{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal (
        ArithmeticCircuit(..),
        CircuitFold (..),
        Var (..),
        SysVar (..),
        NewVar (..),
        VarField,
        Arithmetic,
        Constraint,
        -- circuit constructors
        emptyCircuit,
        naturalCircuit,
        idCircuit,
        -- variable getters and setters
        acInput,
        getAllVars,
        crown,
        -- input mapping
        hlmap,
        -- evaluation functions
        witnessGenerator,
        eval,
        eval1,
        exec,
        exec1,
        apply,
        indexW,
        witToVar
    ) where

import           Control.DeepSeq                                              (NFData (..), NFData1 (..), rwhnf)
import           Control.Monad.State                                          (State, modify, runState)
import           Data.Bifunctor                                               (Bifunctor (..))
import           Data.Binary                                                  (Binary)
import           Data.ByteString                                              (ByteString)
import           Data.Foldable                                                (fold, toList)
import           Data.Functor.Classes                                         (Show1 (liftShowList, liftShowsPrec))
import           Data.Functor.Rep
#if __GLASGOW_HASKELL__ < 912
import           Data.List                                                    (foldl')
#endif
import           Data.List.Infinite                                           (Infinite)
import qualified Data.List.Infinite                                           as I
import           Data.Map.Monoidal                                            (MonoidalMap)
import qualified Data.Map.Monoidal                                            as MM
import           Data.Map.Strict                                              (Map)
import qualified Data.Map.Strict                                              as M
import           Data.Maybe                                                   (catMaybes, fromJust, fromMaybe, mapMaybe)
import           Data.Semialign                                               (unzipDefault)
import           Data.Semigroup.Generic                                       (GenericSemigroupMonoid (..))
import qualified Data.Set                                                     as S
import           Data.Traversable                                             (for)
import           GHC.Generics                                                 (Generic, Par1 (..), U1 (..), (:*:) (..))
import           Optics                                                       hiding (at)
import           Prelude                                                      hiding (Num (..), drop, length, product,
                                                                               splitAt, sum, take, (!!), (^))

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Field                                         (Zp)
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Multivariate                      (Poly, evalMonomial, evalPolynomial, mapVars, var)
import           ZkFold.Control.HApplicative
import           ZkFold.Data.ByteString                                       (fromByteString, toByteString)
import           ZkFold.Data.HFunctor
import           ZkFold.Data.HFunctor.Classes                                 (HNFData (..), HShow (..))
import           ZkFold.Data.Package
import           ZkFold.Data.Product
import           ZkFold.Prelude                                               (take)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.MerkleHash
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Witness
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.WitnessEstimation
import           ZkFold.Symbolic.Fold
import           ZkFold.Symbolic.MonadCircuit

-- | The type that represents a constraint in the arithmetic circuit.
type Constraint c i = Poly c (SysVar i) Natural

data CircuitFold a v w =
  forall p s j.
  ( Binary (Rep p), NFData (Rep p), Ord (Rep p)
  , Representable p, Traversable s, Representable s, NFData1 s
  , Binary (Rep s), NFData (Rep s), Ord (Rep s)
  , Representable j, Binary (Rep j), NFData (Rep j), Ord (Rep j)) =>
    CircuitFold
      { foldStep   :: ArithmeticCircuit a (p :*: s :*: j) s
      , foldStepP  :: p (CircuitWitness a (p :*: s :*: j))
      , foldSeed   :: s v
      , foldSeedP  :: p w
      , foldStream :: Infinite (j w)
      , foldCount  :: v
      }

instance Functor (CircuitFold a v) where
  fmap = second

instance Bifunctor (CircuitFold a) where
  bimap f g CircuitFold {..} = CircuitFold
    { foldStep = foldStep
    , foldStepP = foldStepP
    , foldSeed = f <$> foldSeed
    , foldSeedP = g <$> foldSeedP
    , foldStream = fmap g <$> foldStream
    , foldCount = f foldCount
    }

instance (NFData a, NFData v) => NFData (CircuitFold a v w) where
  rnf CircuitFold {..} = rnf (foldStep, foldCount) `seq` liftRnf rnf foldSeed

data LookupFunction a =
  forall f g. LookupFunction (forall x. (ResidueField x, Representable f, Traversable g) => f x -> g x)

instance NFData (LookupFunction a) where
  rnf = rwhnf

-- | Arithmetic circuit in the form of a system of polynomial constraints.
data ArithmeticCircuit a i o = ArithmeticCircuit
    {
        acSystem         :: Map ByteString (Constraint a i),
        -- ^ The system of polynomial constraints
        acLookupFunction :: Map ByteString (LookupFunction a),
        -- ^ The system of lookup functions
        acLookup         :: MonoidalMap (LookupType a) (S.Set [SysVar i]),
        -- ^ The range constraints [0, a] for the selected variables
        acWitness        :: Map ByteString (CircuitWitness a i),
        -- ^ The witness generation functions
        acFold           :: Map ByteString (CircuitFold a (Var a i) (CircuitWitness a i)),
        -- ^ The set of folding operations
        acOutput         :: o (Var a i)
        -- ^ The output variables
    } deriving Generic

deriving via (GenericSemigroupMonoid (ArithmeticCircuit a i o))
  instance (Ord a, Ord (Rep i), o ~ U1) => Semigroup (ArithmeticCircuit a i o)

deriving via (GenericSemigroupMonoid (ArithmeticCircuit a i o))
  instance (Ord a, Ord (Rep i), o ~ U1) => Monoid (ArithmeticCircuit a i o)

instance (Show a, Show (Rep i), Ord (Rep i), Show1 o) =>
    Show (ArithmeticCircuit a i o) where
  showsPrec = hliftShowsPrec liftShowsPrec liftShowList

-- TODO: make it more readable
instance (Show a, Show (Rep i), Ord (Rep i)) =>
    HShow (ArithmeticCircuit a i) where
  hliftShowsPrec f _ _ r =
    showString "ArithmeticCircuit "
    . showString "{ acSystem = " . shows (acSystem r)
    . showString "\n, acRange = " . shows (acLookup r)
    . showString "\n, acOutput = " . f showsPrec showList 0 (acOutput r)
    . showString " }"

instance (NFData a, NFData (Rep i), NFData1 o) =>
    NFData (ArithmeticCircuit a i o) where
  rnf = hliftRnf liftRnf

instance (NFData a, NFData (Rep i)) => HNFData (ArithmeticCircuit a i) where
  hliftRnf r (ArithmeticCircuit s lf l w f o) =
    rnf (s, lf, l, w, f) `seq` r rnf o

-- | Variables are SHA256 digests (32 bytes)
type VarField = Zp (2 ^ (32 * 8))

----------------------------- Circuit constructors -----------------------------

emptyCircuit :: ArithmeticCircuit a i U1
emptyCircuit = ArithmeticCircuit M.empty M.empty MM.empty M.empty M.empty U1

-- | Given a natural transformation
-- from payload @p@ and input @i@ to output @o@,
-- returns a corresponding arithmetic circuit
-- where outputs computing the payload are unconstrained.
naturalCircuit ::
  ( Arithmetic a, Representable i, Traversable o
  , Binary a, Binary (Rep i), Ord (Rep i)) =>
  (forall x. i x -> o x) -> ArithmeticCircuit a i o
naturalCircuit f = uncurry (set #acOutput) $ flip runState emptyCircuit $
  for (f $ tabulate id) $ unconstrained . pure . InVar

-- | Identity circuit which returns its input @i@ and doesn't use the payload.
idCircuit :: (Representable i, Semiring a) => ArithmeticCircuit a i i
idCircuit = emptyCircuit { acOutput = acInput }

---------------------------------- Variables -----------------------------------

acInput :: (Representable i, Semiring a) => i (Var a i)
acInput = fmapRep (toVar . InVar) (tabulate id)

getAllVars ::
  forall a i o. (Representable i, Foldable i) =>
  ArithmeticCircuit a i o -> [SysVar i]
getAllVars ac =
  toList acInput0
  ++ map (NewVar . EqVar) (M.keys $ acWitness ac)
  ++ map NewVar (M.foldMapWithKey (\fi -> map (FoldLVar fi) . keys) $ acFold ac)
  where
    acInput0 :: i (SysVar i)
    acInput0 = tabulate InVar
    keys :: CircuitFold a v w -> [ByteString]
    keys CircuitFold {..} = toList $ imapRep (\r _ -> toByteString r) foldSeed

indexW ::
  (Arithmetic a, Binary a, Representable i) =>
  ArithmeticCircuit a i o -> i a -> Var a i -> a
indexW circuit inputs =
  indexG (witnessGenerator circuit inputs) inputs

indexG :: (Representable i, Arithmetic a) => Map NewVar a -> i a -> Var a i -> a
indexG witGen inputs = \case
  LinVar k (InVar inV) b -> (\t -> k * t + b) $ index inputs inV
  LinVar k (NewVar newV) b -> (\t -> k * t + b) $ fromMaybe
    (error ("no such NewVar: " <> show newV))
    (witGen M.!? newV)
  ConstVar cV -> cV

-------------------------------- "HProfunctor" ---------------------------------

hlmap ::
  (Representable i, Representable j, Ord (Rep j), Functor o) =>
  (forall x . j x -> i x) -> ArithmeticCircuit a i o -> ArithmeticCircuit a j o
hlmap f (ArithmeticCircuit s lf l w d o) = ArithmeticCircuit
  { acSystem = mapVars (imapSysVar f) <$> s
  , acLookupFunction = lf
  , acLookup = S.map (map $ imapSysVar f) <$> l
  , acWitness = fmap (imapSysVar f) <$> w
  , acFold = bimap (imapVar f) (imapSysVar f <$>) <$> d
  , acOutput = imapVar f <$> o
  }

--------------------------- Symbolic compiler context --------------------------

crown :: ArithmeticCircuit a i g -> f (Var a i) -> ArithmeticCircuit a i f
crown = flip (set #acOutput)

behead :: ArithmeticCircuit a i f -> (ArithmeticCircuit a i U1, f (Var a i))
behead = liftA2 (,) (set #acOutput U1) acOutput

instance HFunctor (ArithmeticCircuit a i) where
    hmap = over #acOutput

instance (Ord (Rep i), Ord a) => HApplicative (ArithmeticCircuit a i) where
    hpure = crown mempty
    hliftA2 f (behead -> (c, o)) (behead -> (d, p)) = crown (c <> d) (f o p)

instance (Ord (Rep i), Ord a) => Package (ArithmeticCircuit a i) where
    unpackWith f (behead -> (c, o)) = crown c <$> f o
    packWith f (unzipDefault . fmap behead -> (cs, os)) = crown (fold cs) (f os)

instance
  (Arithmetic a, Binary a, Binary (Rep i), Ord (Rep i), NFData (Rep i), Representable i) =>
  Symbolic (ArithmeticCircuit a i) where
    type BaseField (ArithmeticCircuit a i) = a
    type WitnessField (ArithmeticCircuit a i) = CircuitWitness a i
    witnessF (behead -> (_, o)) = at <$> o
    fromCircuitF (behead -> (c, o)) f = uncurry (set #acOutput) (runState (f o) c)

instance
  (Arithmetic a, Binary a, Binary (Rep i), Ord (Rep i), NFData (Rep i), Representable i) =>
  SymbolicFold (ArithmeticCircuit a i) where
    sfoldl fun (behead -> (sc, foldSeed)) foldSeedP streamHash
           foldStream (behead -> (cc, Par1 foldCount)) =
        let (foldStep, foldStepP) =
              fun (hmap (fstP . sndP) idCircuit)
                  (tabulate $ pure . InVar . Left)
                  (hmap (sndP . sndP) idCircuit)
            fldID = runHash $ merkleHash
              (acOutput foldStep, foldSeed, acOutput streamHash, foldCount)
            (resultC :*: resultP) = tabulate (\case
                Left v -> LinVar one (NewVar (FoldLVar fldID (toByteString v))) zero
                Right v -> LinVar one (NewVar (FoldPVar fldID (toByteString v))) zero
              )
            fc = emptyCircuit { acFold = M.singleton fldID CircuitFold {..} }
        in ((sc <> cc <> fc) { acOutput = resultC }, at <$> resultP)

----------------------------- MonadCircuit instance ----------------------------

instance
  (Arithmetic a, Binary a, Binary (Rep i), Ord (Rep i), Representable i, o ~ U1)
  => MonadCircuit (Var a i) a (CircuitWitness a i) (State (ArithmeticCircuit a i o)) where

    unconstrained wf = case runWitnessF wf (\sV -> LinUVar one sV zero) of
        ConstUVar c -> return (ConstVar c)
        LinUVar k x b -> return (LinVar k x b)
        _ -> do
          let v = witToVar @a wf
          -- TODO: forbid reassignment of variables
          zoom #acWitness $ modify (M.insert v wf)
          return $ toVar (NewVar (EqVar v))

    constraint p =
      let evalConstVar = \case
            LinVar k sysV b -> fromConstant k * var sysV + fromConstant b
            ConstVar cV -> fromConstant cV
          evalMaybe = \case
            ConstVar cV -> Just cV
            _ -> Nothing
      in case p evalMaybe of
        Just c -> if c == zero
                    then return ()
                    else error "The constraint is non-zero"
        Nothing -> zoom #acSystem . modify $ M.insert (witToVar (p at)) (p evalConstVar)

    rangeConstraint (LinVar k x b) upperBound = do
      v <- preparedVar
      zoom #acLookup . modify $ MM.insertWith S.union (LookupType $ Ranges (S.singleton (zero, upperBound))) (S.singleton [v])
      where
        preparedVar = if k == one && b == zero || k == negate one && b == upperBound
          then return x
          else do
            let
              wf = at $ LinVar k x b
              v = witToVar @a wf
            -- TODO: forbid reassignment of variables
            zoom #acWitness $ modify (M.insert v wf)
            return (NewVar (EqVar v))

    rangeConstraint (ConstVar c) upperBound =
      if c <= upperBound
        then return ()
        else error "The constant does not belong to the interval"

    registerFunction f = do
      let b = runHash @(Just (Order a)) $ sum (f $ tabulate merkleHash)
      zoom #acLookupFunction $ modify (M.insert b $ LookupFunction f)
      return $ FunctionId b

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
witToVar ::
  forall a i. (Finite a, Binary a, Binary (Rep i)) =>
  WitnessF a (SysVar i) -> ByteString
witToVar (WitnessF w) = runHash @(Just (Order a)) $ w $ \case
  InVar inV -> merkleHash inV
  NewVar (EqVar eqV) -> M eqV
  NewVar (FoldLVar fldID fldV) -> merkleHash (fldID, False, fldV)
  NewVar (FoldPVar fldID fldV) -> merkleHash (fldID, True, fldV)

----------------------------- Evaluation functions -----------------------------

witnessGenerator ::
  (Arithmetic a, Binary a, Representable i) =>
  ArithmeticCircuit a i o -> i a -> Map NewVar a
witnessGenerator circ i = fst (allWitnesses circ i)
  where
    allWitnesses ::
      (Arithmetic b, Binary b, Representable j) =>
      ArithmeticCircuit b j n -> j b ->
      (Map NewVar b, Map ByteString (ByteString -> b))
    allWitnesses circuit inputs =
      let evalSysVar = \case
            InVar iV -> index inputs iV
            NewVar (EqVar eqV) -> eqVars M.! eqV
            NewVar (FoldLVar fldID fldV) -> fst (foldVars M.! fldID) M.! fldV
            NewVar (FoldPVar fldID fldV) -> snd (foldVars M.! fldID) fldV
          evalVar = \case
            LinVar k sV b -> k * evalSysVar sV + b
            ConstVar c -> c
          evalWitness k = runWitnessF k evalSysVar
          eqVars = evalWitness <$> acWitness circuit
          foldVars = acFold circuit <&> \CircuitFold {..} ->
            let foldList =
                  take (toConstant $ evalVar foldCount) (I.toList foldStream)
                (resultL, resultP) =
                  foldl' (\(xc, xp) y ->
                    let input = xc :*: fmap evalWitness y
                        (wg, pg) = allWitnesses foldStep (xp :*: input)
                    in (indexG wg (xp :*: input) <$> acOutput foldStep
                        , foldStepP <&> flip runWitnessF \case
                            InVar (Left pV) -> index xp pV
                            InVar (Right inV) -> index input inV
                            NewVar (FoldPVar fldID fV) -> (pg M.! fldID) fV
                            NewVar nV -> wg M.! nV
                        ))
                     (evalVar <$> foldSeed, evalWitness <$> foldSeedP)
                     foldList
            in (M.fromList $ toList $ mzipRep (tabulate toByteString) resultL,
                index resultP . fromJust . fromByteString)
        in (M.mapKeysMonotonic EqVar eqVars
            <> M.unions (M.mapWithKey ((. fst) . M.mapKeysMonotonic . FoldLVar) foldVars),
            snd <$> foldVars)

-- | Evaluates the arithmetic circuit with one output using the supplied input map.
eval1 ::
  (Arithmetic a, Binary a, Representable i) =>
  ArithmeticCircuit a i Par1 -> i a -> a
eval1 ctx i = unPar1 (eval ctx i)

-- | Evaluates the arithmetic circuit using the supplied input map.
eval ::
  (Arithmetic a, Binary a, Representable i, Functor o) =>
  ArithmeticCircuit a i o -> i a -> o a
eval ctx i = indexW ctx i <$> acOutput ctx

-- | Evaluates the arithmetic circuit with no inputs and one output.
exec1 :: (Arithmetic a, Binary a) => ArithmeticCircuit a U1 Par1 -> a
exec1 ac = eval1 ac U1

-- | Evaluates the arithmetic circuit with no inputs.
exec ::
  (Arithmetic a, Binary a, Functor o) => ArithmeticCircuit a U1 o -> o a
exec ac = eval ac U1

-- | Applies the values of the first couple of inputs to the arithmetic circuit.
apply ::
  (Eq a, Field a, Ord (Rep j), Representable i, Functor o) =>
  i a -> ArithmeticCircuit a (i :*: j) o -> ArithmeticCircuit a j o
apply xs ac = ac
  { acSystem = fmap (evalPolynomial evalMonomial varF) (acSystem ac)
  , acLookup = S.fromList . catMaybes . toList . filterSet <$> acLookup ac
  , acWitness = (>>= witF) <$> acWitness ac
  , acFold = bimap outF (>>= witF) <$> acFold ac
  , acOutput = outF <$> acOutput ac
  }
  where
    outF (LinVar k (InVar (Left v)) b)  = ConstVar (k * index xs v + b)
    outF (LinVar k (InVar (Right v)) b) = LinVar k (InVar v) b
    outF (LinVar k (NewVar v) b)        = LinVar k (NewVar v) b
    outF (ConstVar a)                   = ConstVar a

    varF (InVar (Left v))  = fromConstant (index xs v)
    varF (InVar (Right v)) = var (InVar v)
    varF (NewVar v)        = var (NewVar v)

    witF (InVar (Left v))  = WitnessF $ const $ fromConstant (index xs v)
    witF (InVar (Right v)) = pure (InVar v)
    witF (NewVar v)        = pure (NewVar v)

    filterSet = S.map (Just . mapMaybe (\case
                    NewVar v        -> Just (NewVar v)
                    InVar (Right v) -> Just (InVar v)
                    _               -> Nothing))

-- TODO: Add proper symbolic application functions

-- applySymOne :: ArithmeticCircuit a -> State (ArithmeticCircuit a) ()
-- applySymOne x = modify (\(f :: ArithmeticCircuit a) ->
--     let ins = acInput f
--     in f
--     {
--         acInput = tail ins,
--         acWitness = acWitness f . (singleton (head ins) (eval x empty)  `union`)
--     })

-- applySym :: [ArithmeticCircuit a] -> State (ArithmeticCircuit a) ()
-- applySym = foldr ((>>) . applySymOne) (return ())

-- applySymArgs :: ArithmeticCircuit a -> [ArithmeticCircuit a] -> ArithmeticCircuit a
-- applySymArgs x xs = execState (applySym xs) x
