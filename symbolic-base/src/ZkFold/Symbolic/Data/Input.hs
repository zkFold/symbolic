{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Input (
    SymbolicInput (..)
) where

import           Data.Type.Equality               (type (~))
import           Data.Typeable                    (Proxy (..))
import qualified GHC.Generics                     as G
import           GHC.TypeLits                     (KnownNat)
import           Prelude                          (($), (.))

import           ZkFold.Algebra.Class
import           ZkFold.Data.Vector               (Vector)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.MonadCircuit

-- | A class for Symbolic input.
class SymbolicOutput d => SymbolicInput d where
    isValid :: d -> Bool (Context d)
    default isValid ::
      (G.Generic d, GSymbolicInput (G.Rep d), GContext (G.Rep d) ~ Context d)
      => d -> Bool (Context d)
    isValid = gisValid @(G.Rep d) . G.from

instance Symbolic c => SymbolicInput (Bool c) where
  isValid (Bool b) = Bool $ fromCircuitF b $
      \(G.Par1 v) -> do
        u <- newAssigned (\x -> x v * (one - x v))
        isZero $ G.Par1 u

instance (Symbolic c, LayoutFunctor f) => SymbolicInput (c f) where
  isValid _ = true

instance Symbolic c => SymbolicInput (Proxy c) where
  isValid _ = true

instance (
    Context x ~ Context y
    , SymbolicInput x
    , SymbolicInput y
    ) => SymbolicInput (x, y)

instance (
    Context x ~ Context y
    , Context y ~ Context z
    , SymbolicInput x
    , SymbolicInput y
    , SymbolicInput z
    ) => SymbolicInput (x, y, z)

instance (KnownNat n, SymbolicInput x) => SymbolicInput (Vector n x) where
  isValid = all isValid

class GSymbolicData u => GSymbolicInput u where
    gisValid :: u x -> Bool (GContext u)

instance
    ( GContext u ~ GContext v
    , GSupport u ~ GSupport v
    , GSymbolicInput u
    , GSymbolicInput v
    ) => GSymbolicInput (u G.:*: v) where
    gisValid (u G.:*: v) = gisValid u && gisValid v

instance GSymbolicInput u => GSymbolicInput (G.M1 i c u) where
    gisValid (G.M1 u) = gisValid u

instance SymbolicInput x => GSymbolicInput (G.Rec0 x) where
    gisValid (G.K1 x) = isValid x
