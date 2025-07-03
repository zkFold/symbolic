{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.OperationRecord where

import Data.Either (Either (..))
import ZkFold.Algebra.Class
import ZkFold.Protocol.IVC.Commit (HomomorphicCommit (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..), withoutConstraints)
import ZkFold.Symbolic.Data.List (List, head, (.:))
import ZkFold.Symbolic.Data.Sum (OneOf, embedOneOf, matchOneOf, zeroed)
import Prelude (($), type (~))

newtype OperationRecord c s ctx = OperationRecord (List ctx (OneOf [(c, c, c), (c, s, c)] ctx))

addOp
  :: (SymbolicData c, SymbolicData s, Context c ~ ctx, Context s ~ ctx, HomomorphicCommit [s] c, Scale s c)
  => OneOf [c, s] ctx
  -> OperationRecord c s ctx
  -> OperationRecord c s ctx
addOp op' (OperationRecord ops) =
  let c =
        matchOneOf
          (head ops)
          ( \case
              Left (_, _, c1) -> c1
              Right (Left (_, _, c1)) -> c1
              Right (Right _) -> zeroed
          )
   in OperationRecord $
        matchOneOf
          op'
          ( \case
              Left c' -> embedOneOf $ Left (c, c', withoutConstraints $ c + c')
              Right (Left s) -> embedOneOf $ Right $ Left (c, s, withoutConstraints $ scale s c)
              Right (Right _) -> zeroed
          )
          .: ops
