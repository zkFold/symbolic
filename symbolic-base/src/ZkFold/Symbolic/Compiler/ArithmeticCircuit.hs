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
        acSizeR,
        acSystem,
        acValue,
        acPrint,
        -- Variable mapping functions
        hlmap,
        hpmap,
        mapVarArithmeticCircuit,
        -- Arithmetization type fields
        acWitness,
        acInput,
        acOutput,
        -- Testing functions
        checkCircuit,
        checkClosedCircuit,
        isConstantInput
    ) where

import           Control.DeepSeq                                         (NFData)
import           Control.Monad                                           (foldM)
import           Control.Monad.State                                     (execState)
import           Data.Binary                                             (Binary)
import           Data.Bool                                               (bool)
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

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Polynomials.Multivariate            (evalMonomial, evalPolynomial)
import           ZkFold.Base.Data.HFunctor                               (hmap)
import           ZkFold.Base.Data.Product                                (fstP, sndP)
import           ZkFold.Prelude                                          (length)
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
  | b == negate one = return ()
  | otherwise = do
    let bs = binaryExpansion (toConstant b)
        as = binaryExpansion (toConstant a)
    isb <- expansion (length bs) i
    case dropWhile ((== one) . fst) (zip bs isb) of
      [] -> return ()
      ((_, k0):ds) -> do
        z <- newAssigned (one - ($ k0))
        ge <- foldM (\j (c, k) -> newAssigned $ forceGE j c k) z ds
        constraint (($ ge) - one)
    isa <- expansion (length as) i
    case dropWhile ((== zero) . fst) (zip bs isa) of
      [] -> return ()
      ((_, k0):ds) -> do
        z <- newAssigned (one - ($ k0))
        ge <- foldM (\j (c, k) -> newAssigned $ forceLE j c k) z ds
        constraint (($ ge) - one)
  where forceGE j c k
          | c == zero = ($ j) * (one - ($ k))
          | otherwise = one + ($ k) * (($ j) - one)

        forceLE j c k
          | c == one = ($ j) * (one - ($ k))
          | otherwise = one + ($ k) * (($ j) - one)

-- | Desugars range constraints into polynomial constraints
desugarRanges ::
  (Arithmetic a, Binary a, Binary (Rep p), Binary (Rep i), Ord (Rep i)) =>
  ArithmeticCircuit a p i o -> ArithmeticCircuit a p i o
desugarRanges c =
  let r' = flip execState c {acOutput = U1} . traverse (uncurry desugarRange) $
          [(head $ map toVar v, k) | (k', s) <- M.toList rm, v <- S.toList s, k <- S.toList k']
      rm = M.mapKeys (\k -> bool (error "There should only be a range-lookups here") (fromRange k) (isRange k)) rm'
      (rm', tm) = M.partitionWithKey (\k _ -> isRange k) (acLookup c)
  in r' { acLookup = tm, acOutput = acOutput c }

-- | Payload of an input to arithmetic circuit.
-- To be used as an argument to 'compileWith'.
inputPayload ::
  (Representable p, Representable i) =>
  (forall x. p x -> i x -> o x) -> o (WitnessF a (WitVar p i))
inputPayload f =
  f (tabulate $ pure . WExVar) (tabulate $ pure . WSysVar . InVar)

guessOutput ::
  (Arithmetic a, Binary a, Binary (Rep p), Binary (Rep i), Binary (Rep o)) =>
  (Ord (Rep i), Ord (Rep o), NFData (Rep i), NFData (Rep o)) =>
  (Representable i, Representable o, Foldable o) =>
  ArithmeticCircuit a p i o -> ArithmeticCircuit a p (i :*: o) U1
guessOutput c = fromCircuit2F (hlmap fstP c) (hmap sndP idCircuit) $ \o o' -> do
  for_ (mzipRep o o') $ \(i, j) -> constraint (\x -> x i - x j)
  return U1

----------------------------------- Information -----------------------------------

-- | Calculates the number of constraints in the system.
acSizeN :: ArithmeticCircuit a p i o -> Natural
acSizeN = length . acSystem

-- | Calculates the number of variables in the system.
acSizeM :: ArithmeticCircuit a p i o -> Natural
acSizeM = length . acWitness

-- | Calculates the number of range lookups in the system.
acSizeR :: ArithmeticCircuit a p i o -> Natural
acSizeR = sum . map length . M.elems . acLookup

acValue ::
  (Arithmetic a, Binary a, Functor o) => ArithmeticCircuit a U1 U1 o -> o a
acValue = exec

-- | Prints the constraint system, the witness, and the output.
--
-- TODO: Move this elsewhere (?)
-- TODO: Check that all arguments have been applied.
acPrint ::
  (Arithmetic a, Binary a, Show a) =>
  (Show (o (Var a U1)), Show (o a), Functor o) =>
  ArithmeticCircuit a U1 U1 o -> IO ()
acPrint ac = do
    let m = elems (acSystem ac)
        w = witnessGenerator ac U1 U1
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

isConstantInput ::
  ( Arithmetic a, Binary a, Show a, Representable p, Representable i
  , Show (p a), Show (i a), Arbitrary (p a), Arbitrary (i a)
  ) => ArithmeticCircuit a p i o -> Property
isConstantInput c = property $ \x y p -> witnessGenerator c p x === witnessGenerator c p y

checkClosedCircuit
    :: forall a o
     . Arithmetic a
    => Binary a
    => Show a
    => ArithmeticCircuit a U1 U1 o
    -> Property
checkClosedCircuit c = withMaxSuccess 1 $ conjoin [ testPoly p | p <- elems (acSystem c) ]
    where
        w = witnessGenerator c U1 U1
        testPoly p = evalPolynomial evalMonomial varF p === zero
        varF (InVar v)  = absurd v
        varF (NewVar v) = w ! v

checkCircuit
    :: Arbitrary (p a)
    => Arbitrary (i a)
    => Arithmetic a
    => Binary a
    => Show a
    => Representable p
    => Representable i
    => ArithmeticCircuit a p i o
    -> Property
checkCircuit c = conjoin [ property (testPoly p) | p <- elems (acSystem c) ]
    where
        testPoly p = do
            ins <- arbitrary
            pls <- arbitrary
            let w = witnessGenerator c pls ins
                varF (InVar v)  = index ins v
                varF (NewVar v) = w ! v
            return $ evalPolynomial evalMonomial varF p === zero
