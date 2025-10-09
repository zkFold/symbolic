{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Symbolic.Ledger.Types.Orphans 
(
) where
import ZkFold.Symbolic.Class (Symbolic, Ctx)
  
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Data.Vector (Vector)
import GHC.Generics ((:.:) (..))
import Data.Kind (Type)
import Prelude (($), (<$>))
import qualified ZkFold.Symbolic.Algorithm.Hash.Poseidon as Poseidon
import GHC.IsList (IsList(..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)


-- Alternative way to define Hashable instance for Vector taking context.
-- newtype VectorTakingCtx (n) (a :: Ctx -> Type) c = VectorTakingCtx ( (Vector n :.: a) c)
--   deriving stock (Generic, Generic1)
--   deriving anyclass SymbolicData

-- instance (Symbolic c, SymbolicData a) => Hashable (HashSimple c) (VectorTakingCtx n a c) where
--   hasher = Poseidon.hash

-- deriving via (VectorTakingCtx n a c) instance (Symbolic c, SymbolicData a) => Hashable (HashSimple c) ((Vector n :.: a) c

instance forall context n (a :: Ctx -> Type). (Symbolic context, Hashable (HashSimple context) (a context)) => Hashable (HashSimple context) ((Vector n :.: a) context) where
  hasher (Comp1 x) = Poseidon.poseidonHashDefault $ Base.hasher <$> toList x

instance Symbolic context => Hashable (HashSimple context) (FieldElement context) where
  hasher = Poseidon.hash