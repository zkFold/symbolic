{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit (
        ArithmeticCircuit (..),
        -- high-level functions
        solder,
        optimize,
        desugarRanges,
        idCircuit,
        naturalCircuit,
        -- evaluation functions
        witnessGenerator,
        eval,
        eval1,
        exec,
        exec1,
        -- information about the system
        acSizeN,
        acSizeM,
        acSizeL,
        acSizeT,
        acPrint,
        -- Testing functions
        checkCircuit,
        checkClosedCircuit,
    ) where

import           Control.DeepSeq                                         (NFData)
import           Control.Monad                                           (return)
import           Data.Aeson                                              (FromJSON, ToJSON)
import           Data.Binary                                             (Binary)
import           Data.Foldable                                           (Foldable, toList)
import           Data.Function                                           (id, ($), (.))
import           Data.Functor                                            (Functor, fmap, (<$>))
import           Data.Functor.Classes                                    (Show1)
import           Data.Functor.Rep                                        (Rep, Representable, index, tabulate)
import           Data.Kind                                               (Type)
import qualified Data.Map                                                as M
import           Data.Maybe                                              (fromJust)
import           Data.Traversable                                        (Traversable)
import           GHC.Generics                                            (Generic, Par1 (..), U1 (..))
import           Numeric.Natural                                         (Natural)
import           Optics                                                  (over)
import           System.IO                                               (IO, putStr)
import           Test.QuickCheck                                         (Arbitrary, Property, arbitrary, conjoin,
                                                                          property, withMaxSuccess, (===))
import           Text.Pretty.Simple                                      (pPrint)
import           Text.Show                                               (Show)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Polynomial.Multivariate                  (evalMonomial, evalPolynomial)
import           ZkFold.Data.ByteString                                  (fromByteString, toByteString)
import           ZkFold.Prelude                                          (length)
import           ZkFold.Symbolic.Class                                   (Arithmetic)
import qualified ZkFold.Symbolic.Compiler.ArithmeticCircuit.Arbitrary    as Arbitrary
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context      (CircuitContext (..), allWitnesses,
                                                                          emptyContext, getAllVars)
import qualified ZkFold.Symbolic.Compiler.ArithmeticCircuit.Desugaring   as Desugaring
import qualified ZkFold.Symbolic.Compiler.ArithmeticCircuit.Optimization as Optimization
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var          (NewVar (..), evalVar, toVar)

-- | Arithmetic circuit in the form of a system of polynomial constraints.
newtype ArithmeticCircuit a (i :: Type -> Type) o =
    ArithmeticCircuit { acContext :: CircuitContext a o }
    deriving Generic
    deriving newtype (FromJSON, ToJSON, NFData, Show)

instance ( Arbitrary a, Arithmetic a, Binary a
         , Foldable i, Representable i, Binary (Rep i)
         , Representable o, Traversable o
         ) => Arbitrary (ArithmeticCircuit a i o) where
    arbitrary = ArithmeticCircuit <$> Arbitrary.arbitraryContext allInputs 10
        where allInputs = toList $ tabulate @i (EqVar . toByteString)

---------------------------- High-level functions ------------------------------

solder ::
    (Representable i, Binary (Rep i)) =>
    (i NewVar -> CircuitContext a o) -> ArithmeticCircuit a i o
solder = ArithmeticCircuit . ($ tabulate (EqVar . toByteString))

desugarRanges ::
    (Arithmetic a, Binary a) =>
    ArithmeticCircuit a i o -> ArithmeticCircuit a i o
desugarRanges = over #acContext Desugaring.desugarRanges

optimize ::
    forall a i o. (Arithmetic a, Binary a, Binary (Rep i), Functor o) =>
    ArithmeticCircuit a i o -> ArithmeticCircuit a i o
optimize = over #acContext $ Optimization.optimize (Optimization.isInputVar @i)

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

witnessGenerator ::
    (Arithmetic a, Representable i, Binary (Rep i)) =>
    ArithmeticCircuit a i o -> i a -> NewVar -> a
witnessGenerator ArithmeticCircuit {..} =
    allWitnesses acContext . (. fromJust . fromByteString) . index

-- | Evaluates the arithmetic circuit using the supplied input map.
eval ::
    (Arithmetic a, Representable i, Binary (Rep i), Functor o) =>
    ArithmeticCircuit a i o -> i a -> o a
eval ac i = evalVar (witnessGenerator ac i) <$> acOutput (acContext ac)

-- | Evaluates the arithmetic circuit with one output using the supplied input map.
eval1 ::
    (Arithmetic a, Representable i, Binary (Rep i)) =>
    ArithmeticCircuit a i Par1 -> i a -> a
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
acPrint ::
  (Arithmetic a, Show a, Show1 o, Functor o) =>
  ArithmeticCircuit a U1 o -> IO ()
acPrint ac@(ArithmeticCircuit ctx) = do
    let w = witnessGenerator ac U1
    putStr "System size: "
    pPrint $ acSizeN ac
    putStr "Variable size: "
    pPrint $ acSizeM ac
    putStr "Matrices: "
    pPrint $ M.elems (acSystem ctx)
    putStr "Witness: "
    pPrint $ M.fromList [ (var, w var) | var <- getAllVars ctx ]
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
    withMaxSuccess 1 $ conjoin [ testPoly p | p <- M.elems (acSystem ctx) ]
    where
        w = witnessGenerator c U1
        testPoly p = evalPolynomial evalMonomial w p === zero

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
    conjoin [ property (testPoly p) | p <- M.elems (acSystem ctx) ]
    where
        testPoly p = do
            ins <- f <$> arbitrary
            let w = witnessGenerator c ins
            return $ evalPolynomial evalMonomial w p === zero
