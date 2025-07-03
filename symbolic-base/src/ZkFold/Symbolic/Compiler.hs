{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Compiler where

import Control.Applicative (pure)
import Control.Monad (return)
import Data.Aeson (ToJSON, ToJSON1, ToJSONKey)
import Data.Bifunctor (bimap)
import Data.Binary (Binary)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Functor.Rep (Rep)
import Data.List ((++))
import Data.Tuple (swap)
import Data.Type.Equality (type (~))
import GHC.Generics (Par1 (Par1), U1 (..), (:*:))
import System.IO (FilePath, IO, putStrLn)
import Text.Show (show)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit
import ZkFold.ArithmeticCircuit.Context (CircuitContext, fool)
import ZkFold.ArithmeticCircuit.Var (NewVar)
import ZkFold.Data.Product (toPair)
import ZkFold.Prelude (writeFileJSON)
import ZkFold.Symbolic.Class (fromCircuit2F)
import ZkFold.Symbolic.Data.Bool (Bool (Bool))
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..))

type Ctx = (Type -> Type) -> Type

class SymbolicFunction f where
  type Context f :: Ctx
  type Domain f :: Ctx -> Type
  type Range f :: Type

  -- | Saturates a symbolic function.
  apply :: f -> Domain f (Context f) -> Range f

instance (SymbolicFunction y, Context y ~ c) => SymbolicFunction (x c -> y) where
  type Context (x c -> y) = c
  type Domain (x c -> y) = x G.:*: Domain y
  type Range (x c -> y) = Range y
  apply f (x G.:*: y) = apply (f x) y

instance SymbolicFunction (x (c :: Ctx)) where
  type Context (x c) = c
  type Domain (x c) = Proxy
  type Range (x c) = x c
  apply x _ = x

-- | A constraint defining what it means
-- for function of type @f@ to be compilable.
type CompilesWith c s f =
  ( SymbolicFunction f
  , SymbolicData (Range f)
  , Context (Range f) ~ c
  , Domain f ~ s
  , SymbolicInput s
  , Context s ~ c
  )

-- | A constraint defining what it means
-- for data of type @y@ to be properly restorable.
type RestoresFrom c y = (SymbolicData y, Context y ~ c, Payload y ~ U1)

-- | @compileWith opts inputT f@ compiles a function @f@ into an optimized
-- arithmetic circuit packed inside a suitable 'SymbolicData'.
compileWith
  :: forall a y i j s f c0 c1
   . ( CompilesWith c0 s f
     , c0 ~ CircuitContext a
     , RestoresFrom c1 y
     , c1 ~ ArithmeticCircuit a i
     , Binary a
     , Binary (Rep i)
     )
  => ((j NewVar -> c0 (Layout (Range f))) -> c1 (Layout y))
  -> (forall x. j x -> (Payload s x, Layout s x))
  -> f
  -> y
compileWith opts support f = restore . (,U1) . optimize $ opts \x ->
  let input = restore . bimap fool (fmap pure) $ swap (support x)
      Bool b = isValid input
      output = apply f input
   in fromCircuit2F (arithmetize output) b \r (Par1 i) -> do
        constraint (one - ($ i))
        return r

-- | @compile f@ compiles a function @f@ into an optimized arithmetic circuit
-- packed inside a suitable 'SymbolicData'.
compile
  :: forall a y s f
   . ( CompilesWith (CircuitContext a) s f
     , Layout y ~ Layout (Range f)
     , Binary a
     , RestoresFrom (ArithmeticCircuit a (Payload s :*: Layout s)) y
     )
  => f
  -> y
compile = compileWith solder toPair

-- | Compiles a function `f` into an arithmetic circuit. Writes the result to a file.
compileIO
  :: forall a s f
   . (ToJSON a, ToJSONKey a, Binary a, ToJSON1 (Layout (Range f)))
  => CompilesWith (CircuitContext a) s f
  => FilePath
  -> f
  -> IO ()
compileIO scriptFile f = do
  let ac = compile f
  putStrLn "\nCompiling the script...\n"
  putStrLn $ "Number of constraints: " ++ show (acSizeN ac)
  putStrLn $ "Number of variables: " ++ show (acSizeM ac)
  writeFileJSON scriptFile ac
  putStrLn $ "Script saved: " ++ scriptFile
