{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Tests.Symbolic.Data.MerkleTree (specMerkleTree) where

import Data.Function (($))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat)
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary, arbitrary)
import Text.Show (Show, show)

import Tests.Common (it, typeAt)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Data.Eq ((/=), (==))
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Data.Maybe (Maybe (fromJust))
import ZkFold.Symbolic.Data.MerkleTree
import ZkFold.Symbolic.Testing (embedding, (===), (==>))

specMerkleTree'
  :: forall d a
   . (KnownNat d, KnownMerkleTree d)
  => (Arbitrary a, Arithmetic a, Show a, Typeable a)
  => Spec
specMerkleTree' = describe (show $ typeAt @(MerkleTree d a)) do
  it "const roundtrip" $ embedding @(MerkleTree d) @a arbitrary arbitrary
  it "replace . index == id" \(t :: MerkleTree d a) position ->
    let value = t !! position
     in replace MerkleEntry {..} t === t
  it "replace . search' == id" \(t :: MerkleTree d a) position ->
    let pred x = x == fromConstant (toConstant (t !! position))
     in replace (fromJust $ search pred t) t === t
  it
    "index . replace == id"
    \(t :: MerkleTree d a) position value ->
      replace MerkleEntry {..} t !! position === value
  it
    "replace p x . replace p y == replace p x"
    \(t :: MerkleTree d a) p x y ->
      replace (MerkleEntry p x) (replace (MerkleEntry p y) t)
        === replace (MerkleEntry p x) t
  it
    "replace p x . replace q y == replace q y . replace p x"
    \(t :: MerkleTree d a) p q x y ->
      let e = MerkleEntry p x
          f = MerkleEntry q y
       in p /= q ==> replace e (replace f t) === replace f (replace e t)
  it "contains . index == true" \(t :: MerkleTree d a) position ->
    let value = t !! position in t `contains` MerkleEntry {..}
  it "contains . search' == true" \(t :: MerkleTree d a) position ->
    let pred x = x == fromConstant (toConstant (t !! position))
     in t `contains` fromJust (search pred t)

specMerkleTree :: Spec
specMerkleTree = specMerkleTree' @4 @(Zp BLS12_381_Scalar)
