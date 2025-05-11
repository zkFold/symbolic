{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE NoStarIsType  #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context where

import           Control.DeepSeq                                   (NFData, NFData1, liftRnf, rnf, rwhnf)
import           Data.Binary                                       (Binary)
import           Data.Bool                                         (otherwise, (&&), Bool (..))
import           Data.ByteString                                   (ByteString)
import           Data.Foldable                                     (toList, fold)
import           Data.Function                                     (($), (.), flip)
import           Data.Functor                                      (fmap, (<$>), Functor)
import           Data.Functor.Classes                              (Show1, liftShowList, liftShowsPrec)
import           Data.Functor.Rep                                  (Rep, Representable, imapRep, tabulate)
import           Data.Kind                                         (Type)
import           Data.List.Infinite                                (Infinite)
import           Data.Map                                          (Map)
import qualified Data.Map                                          as M
import           Data.Map.Monoidal                                 (MonoidalMap)
import qualified Data.Map.Monoidal                                 as MM
import           Data.Maybe                                        (Maybe (..))
import Data.Monoid ( Monoid, mempty )
import           Data.Ord                                          (Ord)
import           Data.Semigroup                                    (Semigroup, (<>))
import           Data.Semigroup.Generic                            (GenericSemigroupMonoid (..))
import           Data.Set                                          (Set)
import           Data.Traversable                                  (Traversable, traverse)
import           Data.Type.Equality                                (type (~))
import           Data.Typeable                                     (Typeable)
import           GHC.Generics                                      (Generic, U1 (..), (:*:) (..), Par1 (..))
import           Prelude                                           (error, seq)
import           Text.Show                                         (Show, showList, showString, shows, showsPrec)
import qualified Type.Reflection                                   as R

import           ZkFold.Algebra.Field                              (Zp)
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Multivariate            (Poly, var)
import           ZkFold.Data.ByteString                            (toByteString)
import           ZkFold.Data.HFunctor.Classes                      (HNFData, HShow, hliftRnf, hliftShowsPrec)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup (FunctionId (..), LookupType (..))
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var    (CircuitWitness, NewVar (..), Var (..), toVar)
import           ZkFold.Symbolic.MonadCircuit
import ZkFold.Data.HFunctor (HFunctor, hmap)
import ZkFold.Control.HApplicative (HApplicative, hpure, hliftA2)
import ZkFold.Data.Package (Package, unpackWith, packWith)
import ZkFold.Symbolic.Class (Arithmetic, BaseField, WitnessField, Symbolic, witnessF, fromCircuitF)
import ZkFold.Symbolic.Fold (SymbolicFold, sfoldl)
import ZkFold.Algebra.Class
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Witness (WitnessF (..))
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.WitnessEstimation (UVar(..))
import Control.Monad.State (State, modify, runState)
import Data.Either (Either(..))
import Optics (zoom, set, over)
import Control.Applicative (liftA2, pure)
import Data.Semialign (unzipDefault)
import Data.Tuple (uncurry, fst)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.MerkleHash (merkleHash, runHash, MerkleHash (..))
import Data.Eq ((==))
import qualified Data.Set as S

-- | The type that represents a constraint in the arithmetic circuit.
type Constraint a = Poly a NewVar Natural

data CircuitFold a =
    forall p s j.
    ( Binary (Rep p), NFData (Rep p), Ord (Rep p)
    , Representable p, Traversable s, Representable s, NFData1 s
    , Binary (Rep s), NFData (Rep s), Ord (Rep s)
    , Representable j, Binary (Rep j), NFData (Rep j), Ord (Rep j)) =>
        CircuitFold
        { foldStep   :: (p :*: s :*: j) NewVar ->
                (CircuitContext a s, p (CircuitWitness a))
        , foldSeed   :: s (Var a)
        , foldSeedP  :: p (CircuitWitness a)
        , foldStream :: Infinite (j (CircuitWitness a))
        , foldCount  :: Var a
        }

instance NFData a => NFData (CircuitFold a) where
    rnf CircuitFold {..} = rnf foldCount `seq` liftRnf rnf foldSeed

data LookupFunction a =
    forall f g. (Representable f, Traversable g, Typeable f, Typeable g) =>
    LookupFunction (forall x. ResidueField x => f x -> g x)

type FunctionRegistry a = Map ByteString (LookupFunction a)

lookupFunction ::
    forall f g (a :: Type). (Typeable f, Typeable g) =>
    FunctionRegistry a -> FunctionId (f a -> g a) ->
    (forall x. ResidueField x => f x -> g x)
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

instance NFData (LookupFunction a) where
    rnf = rwhnf

-- | Circuit context in the form of a system of polynomial constraints.
data CircuitContext a o = CircuitContext
    {
        acSystem         :: Map ByteString (Constraint a),
        -- ^ The system of polynomial constraints
        acLookupFunction :: FunctionRegistry a,
        -- ^ The set of lookup functions
        acLookup         :: MonoidalMap (LookupType a) (Set [NewVar]),
        -- ^ The lookup constraints for the selected variables
        acWitness        :: Map ByteString (CircuitWitness a),
        -- ^ The witness generation functions
        acFold           :: Map ByteString (CircuitFold a),
        -- ^ The set of folding operations
        acOutput         :: o (Var a)
        -- ^ The output variables
    } deriving Generic

deriving via (GenericSemigroupMonoid (CircuitContext a o))
    instance (Ord a, o ~ U1) => Semigroup (CircuitContext a o)

deriving via (GenericSemigroupMonoid (CircuitContext a o))
    instance (Ord a, o ~ U1) => Monoid (CircuitContext a o)

instance (Show a, Show1 o) => Show (CircuitContext a o) where
    showsPrec = hliftShowsPrec liftShowsPrec liftShowList

-- TODO: make it more readable
instance Show a => HShow (CircuitContext a) where
    hliftShowsPrec f _ _ r =
        showString "CircuitContext "
        . showString "{ acSystem = " . shows (acSystem r)
        . showString "\n, acLookup = " . shows (acLookup r)
        . showString "\n, acOutput = " . f showsPrec showList 0 (acOutput r)
        . showString " }"

instance (NFData a, NFData1 o) => NFData (CircuitContext a o) where
    rnf = hliftRnf liftRnf

instance NFData a => HNFData (CircuitContext a) where
    hliftRnf r (CircuitContext s lf l w f o) =
        rnf (s, lf, l, w, f) `seq` r rnf o

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

-- | Variables are SHA256 digests (32 bytes)
type VarField = Zp (2 ^ (32 * 8))

getAllVars :: CircuitContext a o -> [NewVar]
getAllVars ac =
    fmap EqVar (M.keys $ acWitness ac)
    <> M.foldMapWithKey ((. keys) . \fi -> fmap (FoldLVar fi)) (acFold ac)
    where
        keys :: CircuitFold a -> [ByteString]
        keys CircuitFold {..} =
            toList $ imapRep (\r _ -> toByteString r) foldSeed

--------------------------------- Evaluation -----------------------------------

allWitnesses ::
    Arithmetic a => CircuitContext a o -> (ByteString -> a) -> (NewVar -> a)
allWitnesses CircuitContext {..} inputs = error "TODO"
     {-
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
    -}

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
    sfoldl fun (behead -> (sc, foldSeed)) foldSeedP streamHash
           foldStream (behead -> (cc, Par1 foldCount)) =
        let foldStep (p :*: f :*: g) = fun (fool f) (fmap pure p) (fool g)
            fldID = runHash $ merkleHash
                ( acOutput . fst . foldStep $ tabulate (EqVar . toByteString)
                , foldSeed, acOutput streamHash, foldCount )
            (resultC :*: resultP) = tabulate (\case
                    Left v -> LinVar one (FoldLVar fldID (toByteString v)) zero
                    Right v -> LinVar one (FoldPVar fldID (toByteString v)) zero
                )
            fc = emptyContext { acFold = M.singleton fldID CircuitFold {..} }
         in ((sc <> cc <> fc) { acOutput = resultC }, at <$> resultP)

----------------------------- MonadCircuit instance ----------------------------

instance
    (Arithmetic a, Binary a, o ~ U1) =>
    MonadCircuit (Var a) a (CircuitWitness a) (State (CircuitContext a o)) where

    unconstrained wf = case runWitnessF wf (\sV -> LinUVar one sV zero) of
        ConstUVar c -> pure (ConstVar c)
        LinUVar k x b -> pure (LinVar k x b)
        _ -> do
            let v = witToVar @a wf
            -- TODO: forbid reassignment of variables
            zoom #acWitness $ modify (M.insert v wf)
            pure $ toVar (EqVar v)

    constraint p =
        let evalConstVar = \case
                LinVar k sysV b -> fromConstant k * var sysV + fromConstant b
                ConstVar cV -> fromConstant cV
            evalMaybe = \case
                ConstVar cV -> Just cV
                _ -> Nothing
         in case p evalMaybe of
                Just c -> if c == zero
                          then pure ()
                          else error "The constraint is non-zero"
                Nothing -> zoom #acSystem . modify $
                    M.insert (witToVar (p at)) (p evalConstVar)

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
                constraint (($ LinVar one v zero) - ($ src))
                pure v

    registerFunction f = do
        let functionId =
                runHash @(Just (Order a)) $ sum (f $ tabulate merkleHash)
        zoom #acLookupFunction $ modify (M.insert functionId $ LookupFunction f)
        pure (FunctionId functionId)

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
