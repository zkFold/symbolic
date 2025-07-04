{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}

module ZkFold.Symbolic.UPLC.Data where

import Data.Function (flip, (.))
import Data.Type.Equality (type (~))
import qualified GHC.Generics as G
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.ByteString (ByteString)
import ZkFold.Symbolic.Data.Class (SymbolicData, Context)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.List (List, slice, map)
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.Sum (Sum, match, inject)
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.Morph (MorphFrom, (@), MorphTo (Morph))
import ZkFold.Symbolic.Data.Switch (Switch)
import ZkFold.Symbolic.Fold (SymbolicFold)

data DataPtr c = MkDataPtr
    { ptrOffset :: FieldElement c
    , ptrLength :: FieldElement c
    }
    deriving (G.Generic)

instance Symbolic c => SymbolicData (DataPtr c)

instance Symbolic c => SymbolicInput (DataPtr c)

data DataCell a i r b c
    = DConstrCell
        { cTag :: UInt 64 (Fixed 16) c
        , cFields :: List c a
        }
    | DMapCell (List c (a, a))
    | DListCell (List c a)
    | DIntCell (Int i r c)
    | DBSCell (ByteString b c)
    deriving (G.Generic)

mapCell ::
    forall c x y i r b. (SymbolicFold c, SymbolicData x, Context x ~ c, SymbolicData y, Context y ~ c) =>
    MorphFrom c x y -> DataCell x i r b c -> DataCell y i r b c
mapCell f DConstrCell {..} = DConstrCell { cFields = map f cFields, .. }
mapCell f (DMapCell es) = DMapCell (map (Morph
    \(k :: Switch s x, v :: Switch s x) -> (f @ k :: Switch s y, f @ v :: Switch s y)) es)
mapCell f (DListCell xs) = DListCell (map f xs)
mapCell _ (DIntCell int) = DIntCell int
mapCell _ (DBSCell bs) = DBSCell bs

-- | Plutus Core's Data as a Symbolic datatype.
newtype Data i r b c = MkData { runData :: List c (Sum (DataCell (DataPtr c) i r b c) c) }

deriving newtype instance (Symbolic c, KnownNat b, KnownRegisters c i r) => SymbolicData (Data i r b c)

deriving newtype instance
    (Symbolic c, KnownRegisterSize r, KnownNat i, KnownNat b, KnownRegisters c i r)
    => SymbolicInput (Data i r b c)

indexData :: forall i r b c. (SymbolicFold c, KnownNat b, KnownRegisters c i r) => Data i r b c -> DataPtr c -> Data i r b c
indexData MkData {..} MkDataPtr {..} = MkData { runData = slice (Morph (flip match
    (inject . mapCell @_ @(DataPtr _) @(DataPtr _) @i @r (Morph _))))
    ptrOffset ptrLength runData }

