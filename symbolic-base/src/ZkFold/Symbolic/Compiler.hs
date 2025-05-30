{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators  #-}

module ZkFold.Symbolic.Compiler where

import           Control.Applicative                                (pure)
import           Control.Monad                                      (return)
import           Data.Aeson                                         (ToJSON, ToJSON1, ToJSONKey)
import           Data.Bifunctor                                     (bimap)
import           Data.Binary                                        (Binary)
import           Data.Function                                      (const, ($), (.))
import           Data.Functor                                       (fmap)
import           Data.Functor.Rep                                   (Rep)
import           Data.List                                          ((++))
import           Data.Tuple                                         (swap)
import           Data.Type.Equality
import           GHC.Generics                                       (Par1 (Par1), U1 (..), (:*:))
import           System.IO                                          (FilePath, IO, putStrLn)
import           Text.Show                                          (show)

import           ZkFold.Algebra.Class
import           ZkFold.Data.Product                                (toPair)
import           ZkFold.Prelude                                     (writeFileJSON)
import           ZkFold.Symbolic.Class                              (fromCircuit2F)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (CircuitContext, fool)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Var     (NewVar)
import           ZkFold.Symbolic.Data.Bool                          (Bool (Bool))
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Input
import           ZkFold.Symbolic.MonadCircuit                       (MonadCircuit (..))

-- | A constraint defining what it means
-- for function of type @f@ to be compilable.
type CompilesWith c s f =
    ( SymbolicData f, Context f ~ c, Support f ~ s
    , SymbolicInput s, Context s ~ c )

-- | A constraint defining what it means
-- for data of type @y@ to be properly restorable.
type RestoresFrom c y = (SymbolicOutput y, Context y ~ c, Payload y ~ U1)

-- | @compileWith opts inputT f@ compiles a function @f@ into an optimized
-- arithmetic circuit packed inside a suitable 'SymbolicData'.
compileWith :: forall a y i j s f c0 c1.
    ( CompilesWith c0 s f, c0 ~ CircuitContext a
    , RestoresFrom c1 y, c1 ~ ArithmeticCircuit a i
    , Binary a, Binary (Rep i)) =>
    ((j NewVar -> c0 (Layout f)) -> c1 (Layout y)) ->
    (forall x. j x -> (Payload s x, Layout s x)) -> f -> y
compileWith opts support f = restore . const . (,U1) . optimize $ opts \x ->
    let input = restore . const . bimap fool (fmap pure) $ swap (support x)
        Bool b = isValid input
     in fromCircuit2F (arithmetize f input) b \r (Par1 i) -> do
            constraint (one - ($ i))
            return r

-- | @compile f@ compiles a function @f@ into an optimized arithmetic circuit
-- packed inside a suitable 'SymbolicData'.
compile :: forall a y s f .
    ( CompilesWith (CircuitContext a) s f, Layout y ~ Layout f, Binary a
    , RestoresFrom (ArithmeticCircuit a (Payload s :*: Layout s)) y) => f -> y
compile = compileWith solder toPair

-- | Compiles a function `f` into an arithmetic circuit. Writes the result to a file.
compileIO ::
    forall a s f . (ToJSON a, ToJSONKey a, Binary a, ToJSON1 (Layout f)) =>
    (CompilesWith (CircuitContext a) s f) => FilePath -> f -> IO ()
compileIO scriptFile f = do
    let ac = compile f
    putStrLn "\nCompiling the script...\n"
    putStrLn $ "Number of constraints: " ++ show (acSizeN ac)
    putStrLn $ "Number of variables: " ++ show (acSizeM ac)
    writeFileJSON scriptFile ac
    putStrLn $ "Script saved: " ++ scriptFile
