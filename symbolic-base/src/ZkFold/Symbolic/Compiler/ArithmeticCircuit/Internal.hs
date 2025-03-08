{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
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
        WitVar (..),
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
        hpmap,
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
import           Data.Functor.Rep
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

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field                              (Zp)
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Algebra.Polynomials.Multivariate                 (Poly, evalMonomial, evalPolynomial,
                                                                               mapVars, var)
import           ZkFold.Base.Control.HApplicative
import           ZkFold.Base.Data.ByteString                                  (fromByteString, toByteString)
import           ZkFold.Base.Data.HFunctor
import           ZkFold.Base.Data.Package
import           ZkFold.Base.Data.Product
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

type CircuitWitness a p i = WitnessF a (WitVar p i)

data CircuitFold a v w =
  forall p s j.
  ( Binary (Rep p), Representable p
  , Traversable s, Representable s, NFData1 s
  , Binary (Rep s), NFData (Rep s), Ord (Rep s)
  , Representable j, Binary (Rep j), NFData (Rep j), Ord (Rep j)) =>
    CircuitFold
      { foldStep   :: ArithmeticCircuit a p (s :*: j) s
      , foldStepP  :: p (CircuitWitness a p (s :*: j))
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
data ArithmeticCircuit a p i o = ArithmeticCircuit
    {
        acSystem         :: Map ByteString (Constraint a i),
        -- ^ The system of polynomial constraints
        acLookupFunction :: Map ByteString (LookupFunction a),
        -- ^ The system of lookup functions
        acLookup         :: MonoidalMap (LookupType a) (S.Set [SysVar i]),
        -- ^ The range constraints [0, a] for the selected variables
        acWitness        :: Map ByteString (CircuitWitness a p i),
        -- ^ The witness generation functions
        acFold           :: Map ByteString (CircuitFold a (Var a i) (CircuitWitness a p i)),
        -- ^ The set of folding operations
        acOutput         :: o (Var a i)
        -- ^ The output variables
    } deriving Generic

deriving via (GenericSemigroupMonoid (ArithmeticCircuit a p i o))
  instance (Ord a, Ord (Rep i), o ~ U1) => Semigroup (ArithmeticCircuit a p i o)

deriving via (GenericSemigroupMonoid (ArithmeticCircuit a p i o))
  instance (Ord a, Ord (Rep i), o ~ U1) => Monoid (ArithmeticCircuit a p i o)

instance (NFData a, NFData1 o, NFData (Rep i))
    => NFData (ArithmeticCircuit a p i o) where
  rnf (ArithmeticCircuit s lf l w f o) = rnf (s, lf, l, w, f) `seq` liftRnf rnf o

-- | Variables are SHA256 digests (32 bytes)
type VarField = Zp (2 ^ (32 * 8))

data WitVar p i
  = WExVar (Rep p)
  | WFoldVar ByteString ByteString
  | WSysVar (SysVar i)

imapWitVar ::
  (Representable i, Representable j) =>
  (forall x. j x -> i x) -> WitVar p i -> WitVar p j
imapWitVar _ (WExVar r)     = WExVar r
imapWitVar _ (WFoldVar b c) = WFoldVar b c
imapWitVar f (WSysVar v)    = WSysVar (imapSysVar f v)

pmapWitVar ::
  (Representable p, Representable q) =>
  (forall x. q x -> p x) -> WitVar p i -> WitVar q i
pmapWitVar f (WExVar r)     = index (f (tabulate WExVar)) r
pmapWitVar _ (WFoldVar b c) = WFoldVar b c
pmapWitVar _ (WSysVar v)    = WSysVar v

----------------------------- Circuit constructors -----------------------------

emptyCircuit :: ArithmeticCircuit a p i U1
emptyCircuit = ArithmeticCircuit M.empty M.empty MM.empty M.empty M.empty U1

-- | Given a natural transformation
-- from payload @p@ and input @i@ to output @o@,
-- returns a corresponding arithmetic circuit
-- where outputs computing the payload are unconstrained.
naturalCircuit ::
  ( Arithmetic a, Representable p, Representable i, Traversable o
  , Binary a, Binary (Rep p), Binary (Rep i), Ord (Rep i)) =>
  (forall x. p x -> i x -> o x) -> ArithmeticCircuit a p i o
naturalCircuit f = uncurry (set #acOutput) $ flip runState emptyCircuit $
  for (f (tabulate Left) (tabulate Right)) $
    either (unconstrained . pure . WExVar) (return . toVar . InVar)

-- | Identity circuit which returns its input @i@ and doesn't use the payload.
idCircuit :: (Representable i, Semiring a) => ArithmeticCircuit a p i i
idCircuit = emptyCircuit { acOutput = acInput }

---------------------------------- Variables -----------------------------------

acInput :: (Representable i, Semiring a) => i (Var a i)
acInput = fmapRep (toVar . InVar) (tabulate id)

getAllVars ::
  forall a p i o. (Representable i, Foldable i) =>
  ArithmeticCircuit a p i o -> [SysVar i]
getAllVars ac =
  toList acInput0
  ++ map (NewVar . EqVar) (M.keys $ acWitness ac)
  ++ map NewVar (M.foldMapWithKey (\fi -> map (FoldVar fi) . keys) $ acFold ac)
  where
    acInput0 :: i (SysVar i)
    acInput0 = tabulate InVar
    keys :: CircuitFold a v w -> [ByteString]
    keys CircuitFold {..} = toList $ imapRep (\r _ -> toByteString r) foldSeed

indexW ::
  (Arithmetic a, Binary a, Representable p, Representable i) =>
  ArithmeticCircuit a p i o -> p a -> i a -> Var a i -> a
indexW circuit payload inputs =
  indexG (witnessGenerator circuit payload inputs) inputs

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
  (forall x . j x -> i x) -> ArithmeticCircuit a p i o -> ArithmeticCircuit a p j o
hlmap f (ArithmeticCircuit s lf l w d o) = ArithmeticCircuit
  { acSystem = mapVars (imapSysVar f) <$> s
  , acLookupFunction = lf
  , acLookup = S.map (map $ imapSysVar f) <$> l
  , acWitness = fmap (imapWitVar f) <$> w
  , acFold = bimap (imapVar f) (imapWitVar f <$>) <$> d
  , acOutput = imapVar f <$> o
  }

hpmap ::
  (Representable p, Representable q) => (forall x. q x -> p x) ->
  ArithmeticCircuit a p i o -> ArithmeticCircuit a q i o
hpmap f ac = ac
  { acWitness = fmap (pmapWitVar f) <$> acWitness ac
  , acFold = fmap (pmapWitVar f <$>) <$> acFold ac
  }

--------------------------- Symbolic compiler context --------------------------

crown :: ArithmeticCircuit a p i g -> f (Var a i) -> ArithmeticCircuit a p i f
crown = flip (set #acOutput)

behead :: ArithmeticCircuit a p i f -> (ArithmeticCircuit a p i U1, f (Var a i))
behead = liftA2 (,) (set #acOutput U1) acOutput

instance HFunctor (ArithmeticCircuit a p i) where
    hmap = over #acOutput

instance (Ord (Rep i), Ord a) => HApplicative (ArithmeticCircuit a p i) where
    hpure = crown mempty
    hliftA2 f (behead -> (c, o)) (behead -> (d, p)) = crown (c <> d) (f o p)

instance (Ord (Rep i), Ord a) => Package (ArithmeticCircuit a p i) where
    unpackWith f (behead -> (c, o)) = crown c <$> f o
    packWith f (unzipDefault . fmap behead -> (cs, os)) = crown (fold cs) (f os)

instance
  (Arithmetic a, Binary a, Binary (Rep p), Binary (Rep i), Ord (Rep i), NFData (Rep i)) =>
  Symbolic (ArithmeticCircuit a p i) where
    type BaseField (ArithmeticCircuit a p i) = a
    type WitnessField (ArithmeticCircuit a p i) = WitnessF a (WitVar p i)
    witnessF (behead -> (_, o)) = at <$> o
    fromCircuitF (behead -> (c, o)) f = uncurry (set #acOutput) (runState (f o) c)

instance
  (Arithmetic a, Binary a, Binary (Rep p), Binary (Rep i), Ord (Rep i), NFData (Rep i)) =>
  SymbolicFold (ArithmeticCircuit a p i) where
    sfoldl fun (behead -> (sc, foldSeed)) foldSeedP streamHash
           foldStream (behead -> (cc, Par1 foldCount)) =
        let (foldStep, foldStepP) =
              fun (hmap fstP idCircuit)
                  (tabulate $ pure . WExVar)
                  (hmap sndP idCircuit)
            fldID = runHash $ merkleHash
              (acOutput foldStep, foldSeed, acOutput streamHash, foldCount)
            (resultC :*: resultP) = tabulate (\r -> LinVar one (NewVar (FoldVar fldID (toByteString r))) zero)
            fc = emptyCircuit { acFold = M.singleton fldID CircuitFold {..} }
        in ((sc <> cc <> fc) { acOutput = resultC }, at <$> resultP)

----------------------------- MonadCircuit instance ----------------------------

instance Finite a => Witness (Var a i) (CircuitWitness a p i) where
  at (ConstVar cV)   = fromConstant cV
  at (LinVar k sV b) = fromConstant k * pure (WSysVar sV) + fromConstant b

instance
  (Arithmetic a, Binary a, Binary (Rep p), Binary (Rep i), Ord (Rep i), o ~ U1)
  => MonadCircuit (Var a i) a (CircuitWitness a p i) (State (ArithmeticCircuit a p i o)) where

    unconstrained wf = case runWitnessF wf $ \case
      WSysVar sV -> LinUVar one sV zero
      _          -> More of
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
        Nothing -> zoom #acSystem . modify $ M.insert (witToVar @_ @p (p at)) (p evalConstVar)

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
  forall a p i. (Finite a, Binary a, Binary (Rep p), Binary (Rep i)) =>
  WitnessF a (WitVar p i) -> ByteString
witToVar (WitnessF w) = runHash @(Just (Order a)) $ w $ \case
  WExVar exV -> merkleHash exV
  WFoldVar fldID fldV -> merkleHash (fldID, True, fldV)
  WSysVar (InVar inV) -> merkleHash inV
  WSysVar (NewVar (EqVar eqV)) -> M eqV
  WSysVar (NewVar (FoldVar fldID fldV)) -> merkleHash (fldID, False, fldV)

----------------------------- Evaluation functions -----------------------------

witnessGenerator ::
  (Arithmetic a, Binary a, Representable p, Representable i) =>
  ArithmeticCircuit a p i o -> p a -> i a -> Map NewVar a
witnessGenerator circ p i = fst (allWitnesses circ p i)
  where
    allWitnesses ::
      (Arithmetic b, Binary b, Representable q, Representable j) =>
      ArithmeticCircuit b q j n -> q b -> j b ->
      (Map NewVar b, Map ByteString (ByteString -> b))
    allWitnesses circuit payload inputs =
      let evalSysVar = \case
            InVar iV -> index inputs iV
            NewVar (EqVar eqV) -> eqVars M.! eqV
            NewVar (FoldVar fldID fldV) -> fst (foldVars M.! fldID) M.! fldV
          evalVar = \case
            LinVar k sV b -> k * evalSysVar sV + b
            ConstVar c -> c
          evalWitness k = runWitnessF k \case
            WExVar eV -> index payload eV
            WFoldVar fldID fldV -> snd (foldVars M.! fldID) fldV
            WSysVar sV -> evalSysVar sV
          eqVars = evalWitness <$> acWitness circuit
          foldVars = acFold circuit <&> \CircuitFold {..} ->
            let foldList =
                  take (toConstant $ evalVar foldCount) (I.toList foldStream)
                (resultL, resultP) =
                  foldl' (\(xc, xp) y ->
                    let input = xc :*: fmap evalWitness y
                        (wg, pg) = allWitnesses foldStep xp input
                    in (indexG wg input <$> acOutput foldStep
                        , foldStepP <&> flip runWitnessF \case
                            WExVar pV -> index xp pV
                            WFoldVar fldID fldV -> (pg M.! fldID) fldV
                            WSysVar (InVar inV) -> index input inV
                            WSysVar (NewVar nV) -> wg M.! nV
                        ))
                     (evalVar <$> foldSeed, evalWitness <$> foldSeedP)
                     foldList
            in (M.fromList $ toList $ mzipRep (tabulate toByteString) resultL,
                index resultP . fromJust . fromByteString)
        in (M.mapKeysMonotonic EqVar eqVars
            <> M.unions (M.mapWithKey ((. fst) . M.mapKeysMonotonic . FoldVar) foldVars),
            snd <$> foldVars)

-- | Evaluates the arithmetic circuit with one output using the supplied input map.
eval1 ::
  (Arithmetic a, Binary a, Representable p, Representable i) =>
  ArithmeticCircuit a p i Par1 -> p a -> i a -> a
eval1 ctx p i = unPar1 (eval ctx p i)

-- | Evaluates the arithmetic circuit using the supplied input map.
eval ::
  (Arithmetic a, Binary a, Representable p, Representable i, Functor o) =>
  ArithmeticCircuit a p i o -> p a -> i a -> o a
eval ctx p i = indexW ctx p i <$> acOutput ctx

-- | Evaluates the arithmetic circuit with no inputs and one output.
exec1 :: (Arithmetic a, Binary a) => ArithmeticCircuit a U1 U1 Par1 -> a
exec1 ac = eval1 ac U1 U1

-- | Evaluates the arithmetic circuit with no inputs.
exec ::
  (Arithmetic a, Binary a, Functor o) => ArithmeticCircuit a U1 U1 o -> o a
exec ac = eval ac U1 U1

-- | Applies the values of the first couple of inputs to the arithmetic circuit.
apply ::
  (Eq a, Field a, Ord (Rep j), Representable i, Functor o) =>
  i a -> ArithmeticCircuit a p (i :*: j) o -> ArithmeticCircuit a p j o
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

    witF (WSysVar (InVar (Left v)))  = WitnessF $ const $ fromConstant (index xs v)
    witF (WSysVar (InVar (Right v))) = pure $ WSysVar (InVar v)
    witF (WSysVar (NewVar v))        = pure $ WSysVar (NewVar v)
    witF (WFoldVar i v)              = pure (WFoldVar i v)
    witF (WExVar v)                  = pure (WExVar v)

    filterSet :: Ord (Rep j) => S.Set [SysVar (i :*: j)] ->  S.Set (Maybe [SysVar j])
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
