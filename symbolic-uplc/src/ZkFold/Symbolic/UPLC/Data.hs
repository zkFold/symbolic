{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module ZkFold.Symbolic.UPLC.Data (DataCell (..), Data, KnownData, unfoldData, foldData, serialiseData) where

import Data.Function (($), (.))
import Data.Type.Equality (type (~))
import GHC.Generics qualified as G
import ZkFold.Algebra.Class
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (BaseField, Symbolic)
import ZkFold.Symbolic.Data.Class (SymbolicData (HasRep))
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.Int (Int)
import ZkFold.Symbolic.Data.List
import ZkFold.Symbolic.Data.Sum (Sum, inject, match)
import ZkFold.Symbolic.Data.UInt (UInt)
import ZkFold.Symbolic.Data.VarByteString (VarByteString)
import ZkFold.Symbolic.Fold (SymbolicFold)
import Prelude (error)

import ZkFold.Symbolic.UPLC.Constants
import ZkFold.UPLC.Data qualified as Data

data DataPtr c = MkDataPtr
  { ptrOffset :: FieldElement c
  , ptrLength :: FieldElement c
  }
  deriving (G.Generic, G.Generic1, SymbolicData, SymbolicInput)

nullptr :: Symbolic c => DataPtr c
nullptr = MkDataPtr zero zero

instance Symbolic c => Eq (DataPtr c)

type ConstrTag = UInt 64 (Fixed 16)

data DataCell a c
  = DConstrCell {cTag :: ConstrTag c, cFields :: List a c}
  | DMapCell (List (a G.:*: a) c)
  | DListCell (List a c)
  | DIntCell (Int IntLength IntRegSize c)
  | DBSCell (VarByteString BSLength c)
  deriving G.Generic1

instance (SymbolicFold c, KnownData c) => FromConstant Data.Data (DataCell Data c) where
  fromConstant = \case
    Data.DConstr t f -> DConstrCell (fromConstant t) (fromConstant f)
    Data.DMap es ->
      DMapCell $
        fromConstant
          [ (fromConstant k :: Data c) G.:*: (fromConstant v :: Data c)
          | (k, v) <- es
          ]
    Data.DList xs -> DListCell (fromConstant xs)
    Data.DI int -> DIntCell (fromConstant int)
    Data.DB bs -> DBSCell (fromConstant bs)

mapCell
  :: forall c g x y
   . ( SymbolicFold c
     , SymbolicData g
     , HasRep g c
     , SymbolicData x
     , HasRep x c
     , SymbolicData y
     , HasRep y c
     )
  => g c -> (forall d. (SymbolicFold d, BaseField d ~ BaseField c) => g d -> x d -> y d) -> DataCell x c -> DataCell y c
mapCell g f DConstrCell {..} = DConstrCell {cFields = mapWithCtx g f cFields, ..}
mapCell g f (DMapCell es) =
  DMapCell (mapWithCtx g (\h (k G.:*: v) -> f h k G.:*: f h v) es)
mapCell g f (DListCell xs) = DListCell (mapWithCtx g f xs)
mapCell _ _ (DIntCell int) = DIntCell int
mapCell _ _ (DBSCell bs) = DBSCell bs

concatMapCell
  :: forall c x y
   . (SymbolicFold c, SymbolicData x, HasRep x c, SymbolicData y)
  => (forall d. BaseField d ~ BaseField c => HasRep' y d)
  => (forall d. (SymbolicFold d, BaseField d ~ BaseField c) => x d -> List y d)
  -> DataCell x c
  -> List y c
concatMapCell f DConstrCell {..} = concatMap f cFields
concatMapCell f (DMapCell es) = concatMap (\(k G.:*: v) -> f k ++ f v) es
concatMapCell f (DListCell xs) = concatMap f xs
concatMapCell _ (DIntCell _) = emptyList
concatMapCell _ (DBSCell _) = emptyList

-- | Plutus Core's Data as a Symbolic datatype.
newtype Data c = MkData {runData :: List (Sum (DataCell DataPtr)) c}

type KnownData c = KnownRegisters c IntLength IntRegSize

deriving newtype instance SymbolicData Data

deriving newtype instance SymbolicInput Data

deriving newtype instance (Symbolic c, KnownData c) => Eq (Data c)

indexData :: forall c. (SymbolicFold c, KnownData c) => Data c -> DataPtr c -> Data c
indexData MkData {..} MkDataPtr {..} = MkData {runData = slice ptrOffset ptrLength runData}

nextPtr :: Symbolic c => DataPtr c -> Data c -> DataPtr c
nextPtr MkDataPtr {..} MkData {..} = MkDataPtr (ptrOffset + ptrLength) (size runData)

unfoldData
  :: (SymbolicFold c, KnownData c, SymbolicData y, HasRep y c)
  => Data c -> (DataCell Data c -> y c) -> y c
unfoldData (uncons . runData -> (h, t)) k =
  match h $ k . mapCell (MkData t) indexData

foldData :: forall c. (SymbolicFold c, KnownData c) => DataCell Data c -> Data c
foldData cell = MkData (inject offset .: concatMapCell runData cell)
 where
  offset = case cell of
    DConstrCell {..} -> DConstrCell {cFields = toPtrs cFields, ..}
    DMapCell es ->
      DMapCell $
        tail $
          scanl
            ( \(_ G.:*: p) (k G.:*: v) ->
                let q = nextPtr p k in q G.:*: nextPtr q v
            )
            (nullptr G.:*: nullptr)
            es
    DListCell xs -> DListCell (toPtrs xs)
    DIntCell int -> DIntCell int
    DBSCell bs -> DBSCell bs

  toPtrs :: List Data c -> List DataPtr c
  toPtrs = tail . scanl nextPtr nullptr

instance (SymbolicFold c, KnownData c) => FromConstant Data.Data (Data c) where
  fromConstant = foldData . fromConstant

serialiseData :: SymbolicFold c => Data c -> VarByteString BSLength c
serialiseData = error "TODO: serialiseData"
