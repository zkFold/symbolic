{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


module ZkFold.Symbolic.UPLC.Class (IsData (..), Sym, ExValue (..), ExList (..), BSLength, StrLength, IntLength) where

import           Data.Maybe                         (Maybe (..))
import           Data.Proxy                         (Proxy (..))
import           Data.Typeable                      (Typeable)
import           Prelude                            (type (~))

import           ZkFold.Symbolic.Data.Bool          (Bool)
import           ZkFold.Symbolic.Data.Class         (SymbolicData (..), SymbolicOutput)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Conditional   (Conditional)
import           ZkFold.Symbolic.Data.Int
import qualified ZkFold.Symbolic.Data.List          as L
import           ZkFold.Symbolic.Data.VarByteString
import           ZkFold.Symbolic.Fold               (SymbolicFold)
import qualified ZkFold.Symbolic.UPLC.Data          as Symbolic
import           ZkFold.UPLC.BuiltinType


-- | Class of Symbolic datatypes used inside Converter.
-- Each instance enforces a one-to-one correspondence between some 'BuiltinType'
-- and its interpretation as a Symbolic datatype in arbitrary context 'c'.
class
    ( Typeable v
    , SymbolicOutput v, SymbolicFold c
    , Context v ~ c, Support v ~ Proxy c
    -- TODO: Remove after Conditional becomes part of SymbolicData
    , Conditional (Bool c) v
    ) => IsData (t :: BuiltinType) v c | t c -> v, v -> t, v -> c where
  asPair :: v -> Maybe (ExValue c, ExValue c)
  asList :: v -> Maybe (ExList c)

-- | Existential wrapper around list of 'IsData' Symbolic types.
data ExList c = forall t v. IsData t v c => ExList (L.List c v)

-- | Existential wrapper around 'IsData' Symbolic types.
data ExValue c = forall t v. IsData t v c => ExValue v

-- | We can evaluate UPLC terms in arbitrary 'Symbolic' context as long as
-- it is also 'Typeable'.
type Sym c = (SymbolicFold c, Typeable c)

type IntLength = 64
instance (Sym c, KnownRegisters c IntLength Auto) => IsData BTInteger (Int IntLength Auto c) c where
  asPair _ = Nothing
  asList _ = Nothing

type BSLength = 4000
instance Sym c => IsData BTByteString (VarByteString BSLength c) c where
  asPair _ = Nothing
  asList _ = Nothing

type StrLength = 40000
instance Sym c => IsData BTString (VarByteString StrLength c) c where
  asPair _ = Nothing
  asList _ = Nothing

instance Sym c => IsData BTBool (Bool c) c where
  asPair _ = Nothing
  asList _ = Nothing

instance Sym c => IsData BTUnit (Proxy c) c where
  asPair _ = Nothing
  asList _ = Nothing

instance Sym c => IsData BTData (Symbolic.Data c) c where
  asPair _ = Nothing
  asList _ = Nothing

instance (Sym c, IsData t v c) => IsData (BTList t) (L.List c v) c where
  asPair _ = Nothing
  asList l = Just (ExList l)

instance (Sym c, IsData t v c, IsData t' v' c) => IsData (BTPair t t') (v, v') c where
  asPair (p, q) = Just (ExValue p, ExValue q)
  asList _ = Nothing

-- Uncomment these lines as more types are available in Converter:
-- instance Sym c => IsData BTBLSG1 ??? c where asPair _ = Nothing
-- instance Sym c => IsData BTBLSG2 ??? c where asPair _ = Nothing
-- instance Sym c => IsData BTBLSMLResult ??? c where asPair _ = Nothing
