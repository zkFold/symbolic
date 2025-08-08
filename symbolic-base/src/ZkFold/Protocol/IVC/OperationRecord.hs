{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.OperationRecord where

import Data.Either (Either (..))
import GHC.Generics (Generic)
import Prelude (Integer, ($), type (~))

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Algebra.Number (Natural)
import ZkFold.Symbolic.Data.Class (SymbolicData (..), withoutConstraints)
import ZkFold.Symbolic.Data.List (List, emptyList, head, (.:))
import ZkFold.Symbolic.Data.Sum (Sum, inject, match)

newtype OperationRecord c s ctx = OperationRecord (List ctx (Sum (Either (c, c, c) (c, s, c)) ctx))
  deriving Generic

instance
  (SymbolicData c, SymbolicData s, Context c ~ ctx, Context s ~ ctx)
  => SymbolicData (OperationRecord c s ctx)

newRec
  :: (SymbolicData c, SymbolicData s, Context c ~ ctx, Context s ~ ctx)
  => c
  -> OperationRecord c s ctx
newRec c = OperationRecord $ inject (Left (c, c, c)) .: emptyList

addOp
  :: (SymbolicData c, SymbolicData s, Context c ~ ctx, Context s ~ ctx)
  => (AdditiveSemigroup c, Scale s c)
  => Either c s
  -> OperationRecord c s ctx
  -> OperationRecord c s ctx
addOp op' (OperationRecord ops) =
  let c =
        match
          (head ops)
          ( \case
              Left (_, _, x) -> x
              Right (_, _, x) -> x
          )
   in OperationRecord $
        case op' of
          Left c' -> inject $ Left (c, c', withoutConstraints $ c + c')
          Right s -> inject $ Right (c, s, withoutConstraints $ scale s c)
          .: ops

instance
  ( SymbolicData c, Context c ~ ctx, SymbolicData s, Context s ~ ctx
  , AdditiveSemigroup c, Scale s c
  ) => AdditiveSemigroup (OperationRecord c s ctx)
  where
  OperationRecord ops + record =
    let c =
          match
            (head ops)
            ( \case
                Left (_, _, x) -> x
                Right (_, _, x) -> x
            )
     in addOp (Left c) record

instance
  ( AdditiveMonoid c
  , Scale s c
  , SymbolicData c
  , Context c ~ ctx
  , SymbolicData s
  , Context s ~ ctx
  , FromConstant Natural s
  )
  => AdditiveMonoid (OperationRecord c s ctx)
  where
  zero = newRec zero

instance
  ( AdditiveMonoid c
  , Scale s c
  , SymbolicData c
  , Context c ~ ctx
  , SymbolicData s
  , Context s ~ ctx
  , FromConstant Natural s
  , FromConstant Integer s
  )
  => AdditiveGroup (OperationRecord c s ctx)
  where
  negate = scale (-1 :: Integer)

instance
  ( AdditiveSemigroup c, Scale s c, SymbolicData c, Context c ~ ctx
  , SymbolicData s, Context s ~ ctx, s ~ ScalarFieldOf c
  ) => Scale s (OperationRecord c s ctx)
  where
  scale s = addOp (Right s)

instance
  ( AdditiveSemigroup c
  , Scale s c
  , SymbolicData c
  , Context c ~ ctx
  , SymbolicData s
  , Context s ~ ctx
  , FromConstant Natural s
  )
  => Scale Natural (OperationRecord c s ctx)
  where
  scale n = addOp (Right $ fromConstant n)

instance
  ( AdditiveSemigroup c
  , Scale s c
  , SymbolicData c
  , Context c ~ ctx
  , SymbolicData s
  , Context s ~ ctx
  , FromConstant Integer s
  )
  => Scale Integer (OperationRecord c s ctx)
  where
  scale n = addOp (Right $ fromConstant n)

instance
  ( CyclicGroup c
  , Scale s c
  , SymbolicData c
  , Context c ~ ctx
  , SymbolicData s
  , Context s ~ ctx
  , FromConstant Natural s
  , FromConstant Integer s
  , Scale (ScalarFieldOf c) (OperationRecord c s ctx)
  )
  => CyclicGroup (OperationRecord c s ctx)
  where
  type ScalarFieldOf (OperationRecord c s ctx) = ScalarFieldOf c
  pointGen = newRec pointGen
