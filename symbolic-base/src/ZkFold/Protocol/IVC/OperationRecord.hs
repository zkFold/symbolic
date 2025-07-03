{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.OperationRecord where

import Data.Either (Either (..))
import Data.List (foldr, map, zipWith)
import GHC.Generics (Generic)
import ZkFold.Algebra.Class
import ZkFold.Algebra.Number (Natural)
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit (..), PedersonSetup (groupElements))
import ZkFold.Symbolic.Data.Class (SymbolicData (..), withoutConstraints)
import ZkFold.Symbolic.Data.List (List, emptyList, head, (.:))
import ZkFold.Symbolic.Data.Sum (OneOf, embedOneOf, matchOneOf, zeroed)
import Prelude (Integer, ($), type (~))

newtype OperationRecord c s ctx = OperationRecord (List ctx (OneOf [(c, c, c), (c, s, c)] ctx))
  deriving Generic

instance
  (SymbolicData c, SymbolicData s, Context c ~ ctx, Context s ~ ctx)
  => SymbolicData (OperationRecord c s ctx)

newRec
  :: (SymbolicData c, SymbolicData s, Context c ~ ctx, Context s ~ ctx)
  => c
  -> OperationRecord c s ctx
newRec c = OperationRecord $ embedOneOf (Left (c, c, c)) .: emptyList

addOp
  :: (SymbolicData c, SymbolicData s, Context c ~ ctx, Context s ~ ctx, HomomorphicCommit [s] c, Scale s c)
  => Either c s
  -> OperationRecord c s ctx
  -> OperationRecord c s ctx
addOp op' (OperationRecord ops) =
  let c =
        matchOneOf
          (head ops)
          ( \case
              Left (_, _, x) -> x
              Right (Left (_, _, x)) -> x
              Right (Right _) -> zeroed
          )
   in OperationRecord $
        case op' of
          Left c' -> embedOneOf $ Left (c, c', withoutConstraints $ c + c')
          Right s -> embedOneOf $ Right $ Left (c, s, withoutConstraints $ scale s c)
          .: ops

instance
  (HomomorphicCommit [s] c, Scale s c, SymbolicData c, Context c ~ ctx, SymbolicData s, Context s ~ ctx)
  => AdditiveSemigroup (OperationRecord c s ctx)
  where
  OperationRecord ops + record =
    let c =
          matchOneOf
            (head ops)
            ( \case
                Left (_, _, x) -> x
                Right (Left (_, _, x)) -> x
                Right (Right _) -> zeroed
            )
     in addOp (Left c) record

instance
  ( HomomorphicCommit [s] c
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
  ( HomomorphicCommit [s] c
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
  (HomomorphicCommit [s] c, Scale s c, SymbolicData c, Context c ~ ctx, SymbolicData s, Context s ~ ctx)
  => Scale s (OperationRecord c s ctx)
  where
  scale s = addOp (Right s)

instance
  ( HomomorphicCommit [s] c
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
  ( HomomorphicCommit [s] c
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
  ( PedersonSetup [] (OperationRecord c s ctx)
  , HomomorphicCommit [s] c
  , Scale s c
  , SymbolicData c
  , Context c ~ ctx
  , SymbolicData s
  , Context s ~ ctx
  , FromConstant Natural s
  , FromConstant Integer s
  )
  => HomomorphicCommit [s] (OperationRecord c s ctx)
  where
  hcommit ops = foldr (+) zero $ zipWith scale ops $ map withoutConstraints $ groupElements @[] @(OperationRecord c s ctx)
