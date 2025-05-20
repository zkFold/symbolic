{-# LANGUAGE UndecidableInstances #-}
module ZkFold.Symbolic.Ledger.Types.Output where

import           GHC.Generics                         (Generic)
import           Prelude                              hiding (Bool, Eq, length, splitAt, (*), (+))

import           ZkFold.Symbolic.Class                (Symbolic)
import           ZkFold.Symbolic.Data.Bool            (Bool)
import           ZkFold.Symbolic.Data.Class           (SymbolicData)
import           ZkFold.Symbolic.Data.Conditional     (Conditional)
import           ZkFold.Symbolic.Data.Eq              (Eq)
import           ZkFold.Symbolic.Ledger.Types.Address (Address)
import           ZkFold.Symbolic.Ledger.Types.Datum   (Datum)
import           ZkFold.Symbolic.Ledger.Types.Value   (AssetValues, KnownRegistersAssetQuantity)

-- | Transaction output.
data Output context = Output
        { txoAddress :: Address context
        -- ^ 'Address' at which the value is locked.
        , txoValue   :: AssetValues context
        -- ^ 'AssetValue' locked by the output.
        , txoDatum   :: Datum context
        -- ^ 'Datum' associated with the output.
        }
  deriving stock Generic

instance (KnownRegistersAssetQuantity context, Symbolic context) => SymbolicData (Output context)
instance (KnownRegistersAssetQuantity context, Symbolic context) => Conditional (Bool context) (Output context)
instance (KnownRegistersAssetQuantity context, Symbolic context) => Eq (Output context)
