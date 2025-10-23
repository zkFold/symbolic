{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.ArithmeticCircuit (
  -- * Type and getters
  ArithmeticCircuit (..),

  -- * Constructors from context
  solder,
  guessOutput,

  -- * Arrow-like constructors
  idCircuit,
  naturalCircuit,

  -- * Old Symbolic functions
  witnessF,
  fromCircuitF,
  hmap,
  packWith,
  unpackWith,

  -- * Circuit transformers
  optimize,
  desugarRanges,

  -- * Evaluation functions
  witnessGenerator,
  eval,
  eval1,
  exec,
  exec1,

  -- * Information about the system
  acSizeN,
  acSizeM,
  acSizeL,
  acSizeT,
  acPrint,

  -- * Testing functions
  checkCircuit,
  checkClosedCircuit,
) where

import Control.DeepSeq (NFData)
import Control.Monad (return)
import Control.Monad.State (State)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Foldable (Foldable, toList)
import Data.Function (id, ($), (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Functor.Classes (Show1)
import Data.Functor.Rep (Rep, Representable, index, tabulate)
import Data.Kind (Type)
import qualified Data.Map as M
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Ord (Ord)
import Data.Traversable (Traversable)
import GHC.Generics (Generic, Par1 (..), U1 (..))
import Numeric.Natural (Natural)
import Optics (over)
import System.IO (IO, putStr)
import Test.QuickCheck (Arbitrary, Property)
import qualified Test.QuickCheck as Q
import Text.Pretty.Simple (pPrint)
import Text.Show (Show)

import ZkFold.Algebra.Class
import ZkFold.Algebra.Polynomial.Multivariate (evalMonomial, evalPolynomial)
import qualified ZkFold.ArithmeticCircuit.Arbitrary as Arbitrary
import ZkFold.ArithmeticCircuit.Context (CircuitContext (..))
import qualified ZkFold.ArithmeticCircuit.Context as CC
import qualified ZkFold.ArithmeticCircuit.Context as Context
import qualified ZkFold.ArithmeticCircuit.Desugaring as Desugaring
import qualified ZkFold.ArithmeticCircuit.Optimization as Optimization
import ZkFold.ArithmeticCircuit.Var (CircuitWitness, NewVar, Var, evalVar, toVar)
import ZkFold.Data.Binary (fromByteString, toByteString)
import ZkFold.Data.Product (fromPair)
import ZkFold.Prelude (length)
import ZkFold.Symbolic.Class (Arithmetic)

-- | Arithmetic circuit in the form of a system of polynomial constraints.
newtype ArithmeticCircuit a (i :: Type -> Type) o
  = ArithmeticCircuit {acContext :: CircuitContext a o}
  deriving Generic
  deriving newtype
    ( FromJSON
    , Show
    , ToJSON
    )

instance (NFData a, NFData (o (Var a))) => NFData (ArithmeticCircuit a i o)

instance
  ( Arbitrary a
  , Arithmetic a
  , Binary a
  , Foldable i
  , Representable i
  , Binary (Rep i)
  , Representable o
  , Traversable o
  )
  => Arbitrary (ArithmeticCircuit a i o)
  where
  arbitrary = ArithmeticCircuit <$> Arbitrary.arbitraryContext allInputs 10
   where
    allInputs = toList (tabulate @i toByteString)

hmap
  :: (forall i. f i -> g i)
  -> ArithmeticCircuit a j f
  -> ArithmeticCircuit a j g
hmap f = ArithmeticCircuit . CC.hmap f . acContext

unpackWith
  :: Functor g
  => (forall i. f i -> g (h i))
  -> ArithmeticCircuit a j f
  -> g (ArithmeticCircuit a j h)
unpackWith f = fmap ArithmeticCircuit . CC.unpackWith f . acContext

packWith
  :: (Ord a, Foldable f, Functor f)
  => (forall i. f (g i) -> h i)
  -> f (ArithmeticCircuit a j g)
  -> ArithmeticCircuit a j h
packWith f = ArithmeticCircuit . CC.packWith f . fmap acContext

witnessF :: Functor o => ArithmeticCircuit a i o -> o (CircuitWitness a)
witnessF = CC.witnessF . acContext

fromCircuitF
  :: ArithmeticCircuit a i j
  -> (j (Var a) -> State (CircuitContext a U1) (o (Var a)))
  -> ArithmeticCircuit a i o
fromCircuitF (acContext -> ctx) m =
  ArithmeticCircuit (CC.fromCircuitF ctx m)

-------------------------- Constructors from context ---------------------------

solder
  :: (Representable i, Binary (Rep i))
  => (i NewVar -> CircuitContext a o) -> ArithmeticCircuit a i o
solder = ArithmeticCircuit . ($ tabulate toByteString)

guessOutput
  :: (Arithmetic a, Binary a, Representable j)
  => (Binary (Rep j), Representable o, Foldable o)
  => (forall x. j x -> (i x, o x))
  -> (i NewVar -> CircuitContext a o)
  -> ArithmeticCircuit a j U1
guessOutput f i = solder (Context.guessOutput i . fromPair . f)

--------------------------- Arrow-like constructors ----------------------------

-- | Given a natural transformation from input @i@ to output @o@,
-- returns a corresponding arithmetic circuit.
naturalCircuit
  :: (Arithmetic a, Representable i, Binary (Rep i))
  => (forall x. i x -> o x) -> ArithmeticCircuit a i o
naturalCircuit f = solder (Context.crown Context.emptyContext . f . fmap toVar)

-- | Identity circuit which returns its input @i@ and doesn't use the payload.
idCircuit
  :: (Arithmetic a, Representable i, Binary (Rep i)) => ArithmeticCircuit a i i
idCircuit = naturalCircuit id

---------------------------- Circuit transformers ------------------------------

desugarRanges
  :: (Arithmetic a, Binary a)
  => Maybe Natural -> ArithmeticCircuit a i o -> ArithmeticCircuit a i o
desugarRanges = over #acContext . Desugaring.desugarRanges . fromMaybe 1

optimize
  :: forall a i o
   . (Arithmetic a, Binary a, Binary (Rep i), Functor o)
  => ArithmeticCircuit a i o -> ArithmeticCircuit a i o
optimize = over #acContext $ Optimization.optimize (Optimization.isInputVar @i)

----------------------------- Evaluation functions -----------------------------

witnessGenerator
  :: (Arithmetic a, Representable i, Binary (Rep i))
  => ArithmeticCircuit a i o -> i a -> NewVar -> a
witnessGenerator ArithmeticCircuit {..} =
  Context.allWitnesses acContext . (. fromJust . fromByteString) . index

-- | Evaluates the arithmetic circuit using the supplied input.
eval
  :: (Arithmetic a, Representable i, Binary (Rep i), Functor o)
  => ArithmeticCircuit a i o -> i a -> o a
eval ac i = evalVar (witnessGenerator ac i) <$> acOutput (acContext ac)

-- | Evaluates the arithmetic circuit with one output using the supplied input.
eval1
  :: (Arithmetic a, Representable i, Binary (Rep i))
  => ArithmeticCircuit a i Par1 -> i a -> a
eval1 ac i = unPar1 (eval ac i)

-- | Evaluates the arithmetic circuit with no inputs.
exec :: (Arithmetic a, Functor o) => ArithmeticCircuit a U1 o -> o a
exec ac = eval ac U1

-- | Evaluates the arithmetic circuit with no inputs and one output.
exec1 :: Arithmetic a => ArithmeticCircuit a U1 Par1 -> a
exec1 ac = eval1 ac U1

-------------------------------- Information -----------------------------------

-- | Calculates the number of polynomial constraints in the system.
acSizeN :: ArithmeticCircuit a i o -> Natural
acSizeN = length . acSystem . acContext

-- | Calculates the number of variables in the system.
acSizeM :: ArithmeticCircuit a i o -> Natural
acSizeM = length . acWitness . acContext

-- | Calculates the number of all lookup constraints in the system.
acSizeL :: ArithmeticCircuit a i o -> Natural
acSizeL = sum . fmap length . acLookup . acContext

-- | Calculates the number of lookup tables in the system.
acSizeT :: ArithmeticCircuit a i o -> Natural
acSizeT = length . acLookup . acContext

-- | Prints the constraint system, the witness, and the output.
--
-- TODO: Move this elsewhere (?)
acPrint
  :: (Arithmetic a, Show a, Show1 o, Functor o)
  => ArithmeticCircuit a U1 o -> IO ()
acPrint ac@(ArithmeticCircuit ctx) = do
  let w = witnessGenerator ac U1
  putStr "System size: "
  pPrint $ acSizeN ac
  putStr "Variable size: "
  pPrint $ acSizeM ac
  putStr "Matrices: "
  pPrint $ M.elems (acSystem ctx)
  putStr "Witness: "
  pPrint $ M.fromList [(var, w var) | var <- Context.getAllVars ctx]
  putStr "Output: "
  pPrint $ acOutput ctx
  putStr "Value: "
  pPrint $ exec ac

---------------------------------- Testing -------------------------------------

-- TODO: move this closer to the test suite (?)

-- TODO: `checkClosedCircuit` should check all constraint types
checkClosedCircuit
  :: forall a o
   . Arithmetic a
  => Show a
  => ArithmeticCircuit a U1 o
  -> Property
checkClosedCircuit c@(ArithmeticCircuit ctx) =
  Q.withMaxSuccess 1 $ Q.conjoin [testPoly p | p <- M.elems (acSystem ctx)]
 where
  w = witnessGenerator c U1
  testPoly p = evalPolynomial evalMonomial w p Q.=== zero

-- TODO: `checkCircuit` should check all constraint types
checkCircuit
  :: Arbitrary x
  => Arithmetic a
  => Show a
  => Representable i
  => Binary (Rep i)
  => ArithmeticCircuit a i o
  -> (x -> i a)
  -> Property
checkCircuit c@(ArithmeticCircuit ctx) f =
  Q.conjoin [Q.property (testPoly p) | p <- M.elems (acSystem ctx)]
 where
  testPoly p = do
    ins <- f <$> Q.arbitrary
    let w = witnessGenerator c ins
    return $ evalPolynomial evalMonomial w p Q.=== zero
