module ZkFold.Symbolic.Algorithm.UTxO.Membership where

import Data.Foldable (foldl')
import Data.Type.Equality
import GHC.Generics ((:*:) (..))
import GHC.TypeLits (KnownNat, Nat)

import ZkFold.Algebra.Class hiding (Euclidean (..))
import ZkFold.Control.Conditional (ifThenElse)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Algorithm.Mithril (StakeDistribution, mithril)
import ZkFold.Symbolic.Class (BaseField, Symbolic)
import ZkFold.Symbolic.Data.Combinators (GetRegisterSize, NumberOfRegisters, RegisterSize (Auto))
import ZkFold.Symbolic.Data.EllipticCurve.Point (Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

data Direction = LeftDir | RightDir

data MerklePath (d :: Nat) ctx = MerklePath
  { mpSiblings :: Vector d (FieldElement ctx)
  , mpDirections :: Vector d (FieldElement ctx)
  }

newtype PaddedBytes (len :: Nat) ctx = PaddedBytes {unPaddedBytes :: Vector len (FieldElement ctx)}

-- | Public parameters.
data UTxOParams m d outLen point p q ctx = UTxOParams
  { upStakeDist :: StakeDistribution m point ctx
  , upThreshold :: FieldElement ctx
  , upSignatureRS :: (FFA p 'Auto ctx, FFA q 'Auto ctx)
  , upUtxoRoot :: FieldElement ctx
  }

-- | Witness (private inputs).
data UTxOWitness d outLen ctx = UTxOWitness
  { uwLeafKeyHash :: FieldElement ctx
  , uwTxOutPadded :: PaddedBytes outLen ctx
  , uwMerklePath :: MerklePath d ctx
  }

-- | Circuit output.
data UtxOOutput outLen ctx = UtxOOutput
  { uoOk :: FieldElement ctx
  , uoTxOutPaddedOut :: PaddedBytes outLen ctx
  }
