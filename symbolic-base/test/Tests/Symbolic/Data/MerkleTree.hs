{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Tests.Symbolic.Data.MerkleTree (specMerkleTree) where

import qualified Data.Eq as P
import Data.Function (($))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat)
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary (arbitrary), (==>))
import Text.Show (Show, show)

import Tests.Common (it, typeAt)
import Tests.Symbolic.Data.Common (specConstantRoundtrip)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Data.Eq ((==))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Maybe (Maybe (fromJust))
import ZkFold.Symbolic.Data.MerkleTree
import ZkFold.Symbolic.Interpreter (Interpreter)

specMerkleTree'
  :: forall d a
   . (KnownNat d, KnownMerkleTree d)
  => (Arbitrary a, Arithmetic a, Show a, Typeable a)
  => Spec
specMerkleTree' = describe (show $ typeAt @(MerkleTree d (Interpreter a))) do
  specConstantRoundtrip @a @(MerkleTree d) "MerkleTree" "basefield" arbitrary
  it "replace . index == id" \(t :: MerkleTree d (Interpreter a)) position ->
    let value = t !! position
     in replace MerkleEntry {..} t P.== t
  it "replace . search' == id" \(t :: MerkleTree d (Interpreter a)) position ->
    let pred x = x == fromConstant (toConstant (t !! position))
     in replace (fromJust $ search pred t) t P.== t
  it
    "index . replace == id"
    \(t :: MerkleTree d (Interpreter a)) position value ->
      replace MerkleEntry {..} t !! position P.== value
  it
    "replace p x . replace p y == replace p x"
    \(t :: MerkleTree d (Interpreter a)) p x y ->
      replace (MerkleEntry p x) (replace (MerkleEntry p y) t)
        P.== replace (MerkleEntry p x) t
  it
    "replace p x . replace q y == replace q y . replace p x"
    \(t :: MerkleTree d (Interpreter a)) p q x y ->
      let e = MerkleEntry p x
          f = MerkleEntry q y
       in p P./= q ==> replace e (replace f t) P.== replace f (replace e t)
  it "contains . index == true" \(t :: MerkleTree d (Interpreter a)) position ->
    let value = t !! position in t `contains` MerkleEntry {..} P.== true
  it "contains . search' == true" \(t :: MerkleTree d (Interpreter a)) position ->
    let pred x = x == fromConstant (toConstant (t !! position))
     in t `contains` fromJust (search pred t) P.== true

specMerkleTree :: Spec
specMerkleTree = specMerkleTree' @4 @(Zp BLS12_381_Scalar)
