{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Compiler where

import Control.Applicative (pure)
import Control.Monad (return)
import Data.Aeson (ToJSON, ToJSON1, ToJSONKey)
import Data.Bifunctor (bimap)
import Data.Binary (Binary)
import Data.Function (($), (.))
import Data.Functor (fmap, Functor)
import Data.Functor.Rep (Rep, Representable)
import Data.List ((++))
import Data.Tuple (swap)
import Data.Type.Equality (type (~))
import GHC.Generics (Par1 (Par1), U1 (..), (:*:) (..))
import System.IO (FilePath, IO, putStrLn)
import Text.Show (show)

import ZkFold.Algebra.Class
import ZkFold.ArithmeticCircuit
import ZkFold.ArithmeticCircuit.Context (CircuitContext, fool)
import ZkFold.ArithmeticCircuit.Var (NewVar)
import ZkFold.Data.Product (toPair)
import ZkFold.Prelude (writeFileJSON)
import ZkFold.Symbolic.Class (fromCircuit2F, Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool (Bool))
import ZkFold.Symbolic.Data.Class
import ZkFold.Symbolic.Data.Input
import ZkFold.Symbolic.MonadCircuit (MonadCircuit (..))
import Data.Kind (Type)
import Data.Proxy (Proxy)
import ZkFold.Symbolic.Data.Vec (runVec)

type Ctx = (Type -> Type) -> Type

class ( Symbolic (Context f)
      , SymbolicInput (Domain f)
      , SymbolicData (Range f)
      ) => SymbolicFunction f where
  type Context f :: Ctx
  type Domain f :: Ctx -> Type
  type Range f :: Ctx -> Type

  -- | Saturates a symbolic function.
  apply :: f -> Domain f (Context f) -> Range f (Context f)

instance ( SymbolicInput x
         , SymbolicFunction y
         , Context y ~ c
         ) => SymbolicFunction (x c -> y) where
  type Context (x c -> y) = c
  type Domain (x c -> y) = x :*: Domain y
  type Range (x c -> y) = Range y
  apply f (x :*: y) = apply (f x) y

instance (SymbolicData x, Symbolic c) => SymbolicFunction (x c) where
  type Context (x c) = c
  type Domain (x c) = Proxy
  type Range (x c) = x
  apply x _ = x

-- | @compileWith opts inputT f@ compiles a function @f@ into an optimized
-- arithmetic circuit packed inside a suitable 'SymbolicData'.
compileWith
  :: forall a y i j s f c0 c1 n
   . ( SymbolicFunction f
     , Context f ~ c0
     , Domain f ~ s
     , SymbolicData y
     , c0 ~ CircuitContext a
     , c1 ~ ArithmeticCircuit a i
     , Binary a
     , Binary (Rep i)
     , n ~ Order a
     , Functor (Payload s n)
     , Payload y n ~ U1
     )
  => ((j NewVar -> c0 (Layout (Range f) n)) -> c1 (Layout y n))
  -> (forall x. j x -> (Payload s n x, Layout s n x))
  -> f
  -> y c1
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
  :: forall a y s f n
   . ( SymbolicFunction f
     , Context f ~ CircuitContext a
     , Domain f ~ s
     , Representable (Layout s n)
     , Representable (Payload s n)
     , Binary (Rep (Layout s n))
     , Binary (Rep (Payload s n))
     , Binary a
     , n ~ Order a
     , SymbolicData y
     , Layout y n ~ Layout (Range f) n
     , Payload y n ~ U1
     )
  => f
  -> y (ArithmeticCircuit a (Payload s n :*: Layout s n))
compile = compileWith solder toPair

-- | Compiles a function `f` into an arithmetic circuit. Writes the result to a file.
compileIO
  :: forall a f
   . ( ToJSON a, ToJSONKey a, Binary a, ToJSON1 (Layout (Range f) (Order a))
     , Binary (Rep (Layout (Domain f) (Order a)))
     , Binary (Rep (Payload (Domain f) (Order a)))
     , Representable (Layout (Domain f) (Order a))
     , Representable (Payload (Domain f) (Order a))
     , SymbolicFunction f, Context f ~ CircuitContext a
   )
  => FilePath
  -> f
  -> IO ()
compileIO scriptFile f = do
  let ac = runVec (compile f)
  putStrLn "\nCompiling the script...\n"
  putStrLn $ "Number of constraints: " ++ show (acSizeN ac)
  putStrLn $ "Number of variables: " ++ show (acSizeM ac)
  writeFileJSON scriptFile ac
  putStrLn $ "Script saved: " ++ scriptFile
