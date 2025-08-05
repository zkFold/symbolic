{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Data.MerkleTree (
  specMerkleTree,
) where

import Data.Bool (Bool (..))
import Data.Eq (Eq (..))
import Data.Maybe (Maybe (..))
import GHC.TypeNats (type (-), type (^))
import Test.Hspec (Spec, describe)
import Test.QuickCheck (Arbitrary)
import Prelude (Show, ($))
import qualified Prelude as Haskell

import Tests.Common (it)
import ZkFold.Algebra.Class (Ring, toConstant)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import ZkFold.Algebra.Field (Zp)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Data.MerkleTree
import ZkFold.Data.Vector (Vector (..), (!!))
import ZkFold.Symbolic.Data.Combinators (Iso (..))

testId
  :: forall d h
   . ( Ring h
     , Eq h
     )
  => MerkleTree d h
  -> Bool
testId mt = mt == (from (from mt :: Vector (MerkleTreeSize d) h) :: MerkleTree d h)

testFind
  :: forall d h
   . Eq h
  => MerkleTree d h
  -> Zp (MerkleTreeSize d)
  -> Bool
testFind mt@(MerkleTree _ leaves) k =
  let h = leaves !! toConstant k
   in find (== h) mt == Just h

testLookup
  :: forall d h
   . ( KnownNat (MerkleTreeSize d)
     , Eq h
     )
  => MerkleTree d h
  -> Zp (MerkleTreeSize d)
  -> Haskell.Bool
testLookup mt@(MerkleTree _ leaves) k =
  let h = leaves !! toConstant k
   in case elemIndex h mt of
        Just idx -> lookup mt idx == h
        Nothing -> False

testReplaceAt
  :: forall d h
   . ( Ring h
     , Eq h
     )
  => Zp (MerkleTreeSize d)
  -> h
  -> MerkleTree d h
  -> Bool
testReplaceAt k h mt = lookup (replaceAt k h mt) k == h

specMerkleTree'
  :: forall a d
   . ( Arbitrary a
     , Ring a
     , Eq a
     , Show a
     , KnownNat (2 ^ (d - 1))
     )
  => Spec
specMerkleTree' = do
  describe "MerkleTree specification" $
    describe "Merkle tree" $ do
      it "is isomorphic to Vector" $ \mt ->
        testId @d @a mt
      it "finds an element in the Merkle tree" $ \mt k ->
        testFind @d @a mt k
      it "Looks up an element in the Merkle tree" $ \mt k ->
        testLookup @d @a mt k
      it "replaces an element in the Merkle tree" $ \mt k h ->
        testReplaceAt @d @a k h mt

specMerkleTree :: Spec
specMerkleTree = do
  specMerkleTree' @(Zp BLS12_381_Scalar) @3
