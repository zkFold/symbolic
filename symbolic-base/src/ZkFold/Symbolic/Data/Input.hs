{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Input (
    SymbolicInput (..)
) where

import           GHC.Generics                     ((:*:) (..), (:.:) (..))
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
class SymbolicData x => SymbolicInput x where
    isValid :: Symbolic c => x c -> Bool c
    default isValid :: (G.Generic1 x, GSymbolicInput (G.Rep1 x))
      => Symbolic c => x c -> Bool c
    isValid = gisValid @(G.Rep1 x) . G.from1

instance SymbolicInput Bool where
  isValid b = fromContext $ fromCircuitF (toContext b) $
      \(G.Par1 v) -> do
        u <- newAssigned (\x -> x v * (one - x v))
        isZero $ G.Par1 u

instance (
      SymbolicInput x
    , SymbolicInput y
    ) => SymbolicInput (x :*: y)

instance (KnownNat n, SymbolicInput x) => SymbolicInput (Vector n :.: x) where
  isValid = all isValid . unComp1

class GSymbolicData x => GSymbolicInput x where
    gisValid :: Symbolic c => x c -> Bool c

instance
    ( GSymbolicInput u
    , GSymbolicInput v
    ) => GSymbolicInput (u :*: v) where
    gisValid (u :*: v) = gisValid u && gisValid v

instance GSymbolicInput x => GSymbolicInput (G.M1 i c x) where
    gisValid (G.M1 x) = gisValid x

instance SymbolicInput x => GSymbolicInput (G.Rec1 x) where
    gisValid (G.Rec1 x) = isValid x
