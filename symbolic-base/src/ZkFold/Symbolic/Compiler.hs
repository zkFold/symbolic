{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators  #-}

module ZkFold.Symbolic.Compiler where

import           Control.Monad                                      (return)
import           Data.Aeson                                         (ToJSON, ToJSON1, ToJSONKey)
import           Data.Binary                                        (Binary)
import           Data.Function                                      (id, ($))
import           Data.Functor.Rep                                   (Rep)
import           Data.List                                          ((++))
import           Data.Type.Equality
import           GHC.Generics                                       (Par1 (Par1))
import           System.IO                                          (FilePath, IO, putStrLn)
import           Text.Show                                          (show)

import           ZkFold.Algebra.Class
import           ZkFold.Prelude                                     (writeFileJSON)
import           ZkFold.Symbolic.Class                              (Arithmetic, fromCircuit2F)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext, fool)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var     (NewVar)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.MonadCircuit                       (MonadCircuit (..))

-- | A constraint defining what it means
-- for function of type @f@ to be compilable.
type CompilesWith c s f =
    ( SymbolicFunction f, Context f ~ c, Support f ~ s c
    , SymbolicInput s )

-- | A constraint defining what it means
-- for data of type @y@ to be properly restorable.
type RestoresFrom c y = (SymbolicData y)

-- | @compileWith opts inputT f@ compiles a function @f@ into an optimized
-- arithmetic circuit packed inside a suitable 'SymbolicData'.
compileWith :: forall a y i j s f c0 c1.
    ( CompilesWith c0 s f, c0 ~ CircuitContext a
    , RestoresFrom c1 y, c1 ~ ArithmeticCircuit a i
    , Arithmetic a
    , Binary a, Binary (Rep i)) =>
    ((j NewVar -> c0 (Image f)) -> c1 (Layout y)) ->
    (forall x. j x -> Layout s x) -> f -> y c1
compileWith opts support f = fromContext $ optimize $ opts \x ->
    let input = fromContext $ fool $ support x
        b = isValid input
     in fromCircuit2F (arithmetize f input) (toContext b) \r (Par1 i) -> do
            constraint (one - ($ i))
            return r

-- | @compile f@ compiles a function @f@ into an optimized arithmetic circuit
-- packed inside a suitable 'SymbolicData'.
compile :: forall a y s f .
    ( CompilesWith (CircuitContext a) s f, Layout y ~ Image f, Arithmetic a, Binary a
    , RestoresFrom (ArithmeticCircuit a (Layout s)) y) => f -> y (ArithmeticCircuit a (Layout s))
compile = compileWith solder id

-- | Compiles a function `f` into an arithmetic circuit. Writes the result to a file.
compileIO ::
    forall a s f . (ToJSON a, ToJSONKey a, Arithmetic a, Binary a, LayoutFunctor (Image f), ToJSON1 (Image f)) =>
    (CompilesWith (CircuitContext a) s f) => FilePath -> f -> IO ()
compileIO scriptFile f = do
    let ac = toContext @(Sym (Image f)) $ compile f
    putStrLn "\nCompiling the script...\n"
    putStrLn $ "Number of constraints: " ++ show (acSizeN ac)
    putStrLn $ "Number of variables: " ++ show (acSizeM ac)
    writeFileJSON scriptFile ac
    putStrLn $ "Script saved: " ++ scriptFile
