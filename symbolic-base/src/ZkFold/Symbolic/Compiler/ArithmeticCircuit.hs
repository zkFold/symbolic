{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Compiler.ArithmeticCircuit (
        ArithmeticCircuit,
        Constraint,
        Var,
        witnessGenerator,
        -- high-level functions
        optimize,
        desugarRanges,
        emptyCircuit,
        idCircuit,
        naturalCircuit,
        inputPayload,
        guessOutput,
        -- low-level functions
        eval,
        eval1,
        exec,
        exec1,
        -- information about the system
        acSizeN,
        acSizeM,
        acSizeL,
        acSizeT,
        acSystem,
        acValue,
        acPrint,
        -- Variable mapping functions
        hlmap,
        mapVarArithmeticCircuit,
        -- Arithmetization type fields
        acWitness,
        acInput,
        acOutput,
        -- Testing functions
        checkCircuit,
        checkClosedCircuit,
    ) where

import           Control.DeepSeq                                         (NFData)
import           Control.Monad                                           (foldM)
import           Control.Monad.State                                     (execState)
import           Data.Binary                                             (Binary)
import           Data.Either                                             (partitionEithers)
import           Data.Foldable                                           (for_)
import           Data.Functor.Rep                                        (Representable (..), mzipRep)
import           Data.Map                                                hiding (drop, foldl, foldr, map, null, splitAt,
                                                                          take)
import qualified Data.Map.Monoidal                                       as M
import qualified Data.Set                                                as S
import           Data.Void                                               (absurd)
import           GHC.Generics                                            (U1 (..), (:*:))
import           Numeric.Natural                                         (Natural)
import           Prelude                                                 hiding (Num (..), drop, length, product,
                                                                          splitAt, sum, take, (!!), (^))
import           Test.QuickCheck                                         (Arbitrary, Property, arbitrary, conjoin,
                                                                          property, withMaxSuccess, (===))
import           Text.Pretty.Simple                                      (pPrint)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Polynomial.Multivariate                  (evalMonomial, evalPolynomial)
import           ZkFold.Data.HFunctor                                    (hmap)
import           ZkFold.Data.Product                                     (fstP, sndP)
import           ZkFold.Prelude                                          (assert, length)
import           ZkFold.Symbolic.Class                                   (fromCircuit2F)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Instance     ()
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Lookup
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Map
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Optimization
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var          (toVar)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Witness      (WitnessF)
import           ZkFold.Symbolic.Data.Combinators                        (expansion)
import           ZkFold.Symbolic.MonadCircuit                            (MonadCircuit (..))

--------------------------------- High-level functions --------------------------------

desugarRange :: (Arithmetic a, MonadCircuit i a w m) => i -> (a, a) -> m ()
desugarRange i (a, b)
  | a /= zero = error "non-zero lower bound not supported yet"
  | b == negate one = return ()
  | otherwise = do
    let bs = binaryExpansion (toConstant b)
    is <- expansion (length bs) i
    case dropWhile ((== one) . fst) (zip bs is) of
      [] -> return ()
      ((_, k0):ds) -> do
        z <- newAssigned (one - ($ k0))
        ge <- foldM (\j (c, k) -> newAssigned $ forceGE j c k) z ds
        constraint (($ ge) - one)
  where forceGE j c k
          | c == zero = ($ j) * (one - ($ k))
          | otherwise = one + ($ k) * (($ j) - one)

-- | Desugars range constraints into polynomial constraints
desugarRanges ::
  (Arithmetic a, Binary a, Binary (Rep i), Ord (Rep i), Representable i) =>
  ArithmeticCircuit a i o -> ArithmeticCircuit a i o
desugarRanges c =
  let (rm, tm) = partitionEithers
                    [ maybe (Right (k, v)) (Left . (, v)) (asRange k)
                    | (k, v) <- M.assocs (acLookup c)
                    ]
      r' = flip execState c {acOutput = U1} $ traverse (uncurry desugarRange)
              -- TODO: @v@ should belong to either of segments
              [ (toVar v, assert (length k == 1) (length k) (head k))
              | (S.toList -> k, s) <- rm, [v] <- S.toList s
              ]
  in r' { acLookup = M.fromList tm, acOutput = acOutput c }

-- | Payload of an input to arithmetic circuit.
-- To be used as an argument to 'compileWith'.
inputPayload ::
  (Representable i) => (forall x. i x -> o x) -> o (WitnessF a (SysVar i))
inputPayload f = f $ tabulate (pure . InVar)

guessOutput ::
  (Arithmetic a, Binary a, Binary (Rep i), Binary (Rep o)) =>
  (Ord (Rep i), Ord (Rep o), NFData (Rep i), NFData (Rep o)) =>
  (Representable i, Representable o, Foldable o) =>
  ArithmeticCircuit a i o -> ArithmeticCircuit a (i :*: o) U1
guessOutput c = fromCircuit2F (hlmap fstP c) (hmap sndP idCircuit) $ \o o' -> do
  for_ (mzipRep o o') $ \(i, j) -> constraint (\x -> x i - x j)
  return U1

----------------------------------- Information -----------------------------------

-- | Calculates the number of polynomial constraints in the system.
acSizeN :: ArithmeticCircuit a i o -> Natural
acSizeN = length . acSystem

-- | Calculates the number of variables in the system.
acSizeM :: ArithmeticCircuit a i o -> Natural
acSizeM = length . acWitness

-- | Calculates the number of all lookup constraints in the system.
acSizeL :: ArithmeticCircuit a i o -> Natural
acSizeL = sum . fmap length . acLookup

-- | Calculates the number of lookup tables in the system.
acSizeT :: ArithmeticCircuit a i o -> Natural
acSizeT = length . acLookup

acValue ::
  (Arithmetic a, Binary a, Functor o) => ArithmeticCircuit a U1 o -> o a
acValue = exec

-- | Prints the constraint system, the witness, and the output.
--
-- TODO: Move this elsewhere (?)
-- TODO: Check that all arguments have been applied.
acPrint ::
  (Arithmetic a, Binary a, Show a) =>
  (Show (o (Var a U1)), Show (o a), Functor o) =>
  ArithmeticCircuit a U1 o -> IO ()
acPrint ac = do
    let m = elems (acSystem ac)
        w = witnessGenerator ac U1
        v = acValue ac
        o = acOutput ac
    putStr "System size: "
    pPrint $ acSizeN ac
    putStr "Variable size: "
    pPrint $ acSizeM ac
    putStr "Matrices: "
    pPrint m
    putStr "Witness: "
    pPrint w
    putStr "Output: "
    pPrint o
    putStr "Value: "
    pPrint v

---------------------------------- Testing -------------------------------------

-- TODO: move this closer to the test suite (?)

-- TODO: `checkClosedCircuit` should check all constraint types
checkClosedCircuit
    :: forall a o
     . Arithmetic a
    => Binary a
    => Show a
    => ArithmeticCircuit a U1 o
    -> Property
checkClosedCircuit c = withMaxSuccess 1 $ conjoin [ testPoly p | p <- elems (acSystem c) ]
    where
        w = witnessGenerator c U1
        testPoly p = evalPolynomial evalMonomial varF p === zero
        varF (InVar v)  = absurd v
        varF (NewVar v) = w ! v

-- TODO: `checkCircuit` should check all constraint types
checkCircuit
    :: Arbitrary x
    => Arithmetic a
    => Binary a
    => Show a
    => Representable i
    => ArithmeticCircuit a i o
    -> (x -> i a)
    -> Property
checkCircuit c f = conjoin [ property (testPoly p) | p <- elems (acSystem c) ]
    where
        testPoly p = do
            ins <- f <$> arbitrary
            let w = witnessGenerator c ins
                varF (InVar v)  = index ins v
                varF (NewVar v) = w ! v
            return $ evalPolynomial evalMonomial varF p === zero
