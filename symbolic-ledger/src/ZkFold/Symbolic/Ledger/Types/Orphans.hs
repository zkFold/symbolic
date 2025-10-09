{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module ZkFold.Symbolic.Ledger.Types.Orphans 
(

) where
import ZkFold.Symbolic.Class (Symbolic, Ctx)
  
import ZkFold.Symbolic.Data.Hash (Hashable)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Ledger.Types.Hash
import ZkFold.Data.Vector (Vector)
import GHC.Generics ((:.:) (..), Generic1, Generic)
import Data.Kind (Type)
import Prelude (($), (<$>))
import qualified ZkFold.Symbolic.Algorithm.Hash.Poseidon as Poseidon
import GHC.IsList (IsList(..))
import ZkFold.Symbolic.Data.Class (SymbolicData)


-- newtype VectorTakingCtx (n) (a :: Ctx -> Type) c = VectorTakingCtx ( (Vector n :.: a) c)
--   deriving stock (Generic, Generic1)
--   deriving anyclass SymbolicData

-- instance (Symbolic c, SymbolicData a) => Hashable (HashSimple c) (VectorTakingCtx n a c) where
--   hasher = Poseidon.hash

-- deriving via (VectorTakingCtx n a c) instance (Symbolic c, SymbolicData a) => Hashable (HashSimple c) ((Vector n :.: a) c)

instance forall context n (a :: Ctx -> Type). (Symbolic context, Hashable (HashSimple context) (a context)) => Hashable (HashSimple context) ((Vector n :.: a) context) where
  hasher (Comp1 x) = Poseidon.poseidonHashDefault $ Base.hasher <$> toList x
  
{-
input -> toList x = [a, b, c]
h [h a, h b, h c] --> h [n - 1, n - 1, n - 1] -> 3n - 3 + 2
-----------------
h [a, b, c] --> 3n - 1
-}
