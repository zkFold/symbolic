module ZkFold.Symbolic.Data.JWT.Twitch () where

import           Control.DeepSeq                    (NFData, force)
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import qualified Data.Aeson                         as JSON
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Constraint                    (Dict (..), withDict, (:-) (..))
import           Data.Constraint.Nat                (Max, divNat, minusNat, plusNat, timesNat)
import           Data.Constraint.Unsafe             (unsafeAxiom, unsafeSNat)
import           Data.Maybe                         (fromMaybe)
import           Data.Scientific                    (toBoundedInteger)
import qualified Data.Text                          as T
import           Generic.Random                     (genericArbitrary, uniform)
import           GHC.Generics                       (Generic, Par1 (..))
import           GHC.TypeLits                       (withKnownNat)
import           Prelude                            (fmap, pure, type (~), ($), (.), (<$>))
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor          (hmap)
import qualified ZkFold.Base.Data.Vector            as V
import           ZkFold.Base.Data.Vector            ((!!))
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString    (ByteString (..), concat, toWords)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.UInt
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), wipeUnassigned, (@+))
import           ZkFold.Symbolic.MonadCircuit       (newAssigned)
