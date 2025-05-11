module ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal (
        ArithmeticCircuit(..),
        Var (..),
        NewVar (..),
        Arithmetic,
        -- circuit constructors
        naturalCircuit,
        idCircuit,
        -- evaluation functions
        witnessGenerator,
        eval,
        eval1,
        exec,
        exec1,
        indexW,
    ) where

import           Data.Binary                                        (Binary)
import           Data.Function                                      (id, (.))
import           Data.Functor                                       (Functor, (<$>))
import           Data.Functor.Rep
import           Data.Maybe                                         (fromJust)
import           GHC.Generics                                       (Par1 (..), U1 (..))

import           ZkFold.Algebra.Class
import           ZkFold.Data.ByteString                             (fromByteString, toByteString)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext (acOutput), allWitnesses,
                                                                     emptyContext)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var

-- | Arithmetic circuit in the form of a system of polynomial constraints.
newtype ArithmeticCircuit a i o =
    ArithmeticCircuit { acContext :: CircuitContext a o }

----------------------------- Circuit constructors -----------------------------

-- | Given a natural transformation from input @i@ to output @o@,
-- returns a corresponding arithmetic circuit.
naturalCircuit ::
    (Arithmetic a, Representable i, Binary (Rep i)) =>
    (forall x. i x -> o x) -> ArithmeticCircuit a i o
naturalCircuit f = ArithmeticCircuit emptyContext { acOutput = f acInput }
    where acInput = tabulate (toVar . EqVar . toByteString)

-- | Identity circuit which returns its input @i@ and doesn't use the payload.
idCircuit ::
    (Arithmetic a, Representable i, Binary (Rep i)) => ArithmeticCircuit a i i
idCircuit = naturalCircuit id

----------------------------- Evaluation functions -----------------------------

indexW ::
    (Arithmetic a, Representable i, Binary (Rep i)) =>
    ArithmeticCircuit a i o -> i a -> Var a -> a
indexW circuit inputs =
    let witGen = witnessGenerator circuit inputs
     in \case LinVar k newV b -> k * witGen newV + b
              ConstVar cV -> cV

witnessGenerator ::
    (Arithmetic a, Representable i, Binary (Rep i)) =>
    ArithmeticCircuit a i o -> i a -> NewVar -> a
witnessGenerator ArithmeticCircuit {..} =
    allWitnesses acContext . (\i -> index i . fromJust . fromByteString)

-- | Evaluates the arithmetic circuit with one output using the supplied input map.
eval1 ::
    (Arithmetic a, Representable i, Binary (Rep i)) =>
    ArithmeticCircuit a i Par1 -> i a -> a
eval1 ctx i = unPar1 (eval ctx i)

-- | Evaluates the arithmetic circuit using the supplied input map.
eval ::
    (Arithmetic a, Representable i, Binary (Rep i), Functor o) =>
    ArithmeticCircuit a i o -> i a -> o a
eval ctx i = indexW ctx i <$> acOutput (acContext ctx)

-- | Evaluates the arithmetic circuit with no inputs and one output.
exec1 :: Arithmetic a => ArithmeticCircuit a U1 Par1 -> a
exec1 ac = eval1 ac U1

-- | Evaluates the arithmetic circuit with no inputs.
exec :: (Arithmetic a, Functor o) => ArithmeticCircuit a U1 o -> o a
exec ac = eval ac U1

-- | Applies the values of the first couple of inputs to the arithmetic circuit.
-- apply ::
--  (Eq a, Field a, Ord (Rep j), Representable i, Functor o) =>
--  i a -> ArithmeticCircuit a (i :*: j) o -> ArithmeticCircuit a j o
-- apply xs ac = ac
--   { acSystem = fmap (evalPolynomial evalMonomial varF) (acSystem ac)
--   , acLookup = S.fromList . catMaybes . toList . filterSet <$> acLookup ac
--   , acWitness = (>>= witF) <$> acWitness ac
--   , acFold = bimap outF (>>= witF) <$> acFold ac
--   , acOutput = outF <$> acOutput ac
--   }
--   where
--     outF (LinVar k (InVar (Left v)) b)  = ConstVar (k * index xs v + b)
--     outF (LinVar k (InVar (Right v)) b) = LinVar k (InVar v) b
--     outF (LinVar k (NewVar v) b)        = LinVar k (NewVar v) b
--     outF (ConstVar a)                   = ConstVar a

--     varF (InVar (Left v))  = fromConstant (index xs v)
--     varF (InVar (Right v)) = var (InVar v)
--     varF (NewVar v)        = var (NewVar v)

--     witF (InVar (Left v))  = WitnessF $ const $ fromConstant (index xs v)
--     witF (InVar (Right v)) = pure (InVar v)
--     witF (NewVar v)        = pure (NewVar v)

--     filterSet = S.map (Just . mapMaybe (\case
--                     NewVar v        -> Just (NewVar v)
--                     InVar (Right v) -> Just (InVar v)
--                     _               -> Nothing))

-- TODO: Add proper symbolic application functions

-- applySymOne :: ArithmeticCircuit a -> State (ArithmeticCircuit a) ()
-- applySymOne x = modify (\(f :: ArithmeticCircuit a) ->
--     let ins = acInput f
--     in f
--     {
--         acInput = tail ins,
--         acWitness = acWitness f . (singleton (head ins) (eval x empty)  `union`)
--     }

-- applySym :: [ArithmeticCircuit a] -> State (ArithmeticCircuit a) ()
-- applySym = foldr ((>>) . applySymOne) (return ())

-- applySymArgs :: ArithmeticCircuit a -> [ArithmeticCircuit a] -> ArithmeticCircuit a
-- applySymArgs x xs = execState (applySym xs) x
