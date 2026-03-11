{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Tests.Symbolic.Data.MerkleTree (specMerkleTree) where

import qualified Data.Eq as P
import Data.Function (const, ($))
import Data.Functor (fmap)
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
import GHC.Generics ((:.:) (..))
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
        siblings = Comp1 $ fmap (const zero) (unComp1 position)
     in replace (addSiblings t MerkleEntry {..}) t P.== t
  it "replace . search' == id" \(t :: MerkleTree d (Interpreter a)) position ->
    let pred x = x == fromConstant (toConstant (t !! position))
     in replace (fromJust $ search pred t) t P.== t
  it
    "index . replace == id"
    \(t :: MerkleTree d (Interpreter a)) position value ->
      let siblings = Comp1 $ fmap (const zero) (unComp1 position)
       in replace MerkleEntry {..} t !! position P.== value
  it
    "replace p x . replace p y == replace p x"
    \(t :: MerkleTree d (Interpreter a)) p x y ->
      let mk v = addSiblings t (MerkleEntry p v (Comp1 $ fmap (const zero) (unComp1 p)))
       in replace (mk x) (replace (mk y) t) P.== replace (mk x) t
  it
    "replace p x . replace q y == replace q y . replace p x"
    \(t :: MerkleTree d (Interpreter a)) p q x y ->
      let e = addSiblings t (MerkleEntry p x (Comp1 $ fmap (const zero) (unComp1 p)))
          f = addSiblings t (MerkleEntry q y (Comp1 $ fmap (const zero) (unComp1 q)))
       in p P./= q ==> replace e (replace f t) P.== replace f (replace e t)
  it "contains . index == true" \(t :: MerkleTree d (Interpreter a)) position ->
    let value = t !! position
        siblings = Comp1 $ fmap (const zero) (unComp1 position)
     in t `contains` MerkleEntry {..} P.== true
  it "contains . search' == true" \(t :: MerkleTree d (Interpreter a)) position ->
    let pred x = x == fromConstant (toConstant (t !! position))
     in t `contains` fromJust (search pred t) P.== true

specMerkleTree :: Spec
specMerkleTree = specMerkleTree' @4 @(Zp BLS12_381_Scalar)
