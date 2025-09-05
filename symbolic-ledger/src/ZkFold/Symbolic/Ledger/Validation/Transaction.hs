{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ZkFold.Symbolic.Ledger.Validation.Transaction (
  validateTransaction,
) where

import Data.Function ((&))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Eq
import ZkFold.Data.Vector (Vector)
import ZkFold.Prelude (foldl')
import ZkFold.Symbolic.Data.Bool (Bool, BoolType (..))
import ZkFold.Symbolic.Ledger.Types
import qualified Prelude as P

validateTransaction
  :: forall context bo
   . SignatureTransaction context
  => AccountInfo context
  -> AccountInfo context
  -> Vector bo (Address context, Address context, AssetValue context)
  -> Transaction context
  -> (Bool context, AccountInfo context, AccountInfo context)
validateTransaction ai aiWithoutBridgedOut bridgedOutAssets tx =
  ifThenElse
    tx.isBridgeOut
    ( foldl'
        (\found (from, to, asset) -> found || (from, to, asset) == (tx.from, tx.to, tx.asset))
        (false :: Bool context)
        bridgedOutAssets
    , subtractAsset ai (tx.from, tx.asset)
    , aiWithoutBridgedOut
    )
    ( true
    , subtractAsset ai (tx.from, tx.asset)
        & addAsset ai (tx.to, tx.asset)
    , subtractAsset aiWithoutBridgedOut (tx.from, tx.asset)
        & addAsset aiWithoutBridgedOut (tx.to, tx.asset)
    )

subtractAsset = P.undefined

addAsset = P.undefined
