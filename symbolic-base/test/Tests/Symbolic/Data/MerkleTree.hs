{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module Tests.Symbolic.Data.MerkleTree
  ( specMerkleTree
  ) where


import           Control.Monad                               (replicateM)
import           Data.Binary                                 (Binary)
import           Data.Semialign                              (Zip)
import           Data.Type.Equality                          (type (~))
import           GHC.Generics                                (Par1 (Par1), U1 (..))
import           Prelude                                     (Int, fmap, fromIntegral, return, ($), (.), (^))
import qualified Prelude                                     as Haskell
import           Test.Hspec                                  (Spec, describe)
import           Test.QuickCheck                             (Arbitrary, Gen, Property, (.&.), (===))
import           Tests.Symbolic.ArithmeticCircuit            (it)

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..), PrimeField, one, (-!))
import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.Basic.Number            (KnownNat, Natural, type (-), type (<=), type (^), value)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import qualified ZkFold.Base.Data.Vector                     as V
import           ZkFold.Base.Data.Vector                     (Vector (..), (!!))
import           ZkFold.Prelude                              (chooseNatural)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (ArithmeticCircuit, exec1)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators            (Iso (..), KnownRegisterSize, NumberOfRegisters,
                                                              RegisterSize (..))
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.MerkleTree
import           ZkFold.Symbolic.Data.UInt                   (UInt)
import           ZkFold.Symbolic.Fold
import           ZkFold.Symbolic.Interpreter                 (Interpreter (..))


type AC a = ArithmeticCircuit a U1 U1
type I a = Interpreter a

evalBool :: forall a . (Arithmetic a, Binary a) => Bool (AC a) -> a
evalBool (Bool ac) = exec1 ac

evalBoolVec :: forall a . Bool (Interpreter a) -> a
evalBoolVec (Bool (Interpreter (Par1 v))) = v

testId :: forall c x d n.
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  , Zip (Layout x)
  , n ~ 2 ^ (d-1),1 <= d
  , KnownNat n, Eq x, BooleanOf x ~ Bool c
  ) => Vector n x -> Bool c
testId v = v == (from (from v :: MerkleTree d x) :: Vector n x)


testLookup :: forall x c d n.
  ( SymbolicOutput x
  , Context x ~ c
  , SymbolicFold c
  , KnownNat d
  , 1 <= d
  , Zip (Layout x)
  , n ~ 2 ^ (d-1)
  , Eq x, BooleanOf x ~ Bool c
  ) => Vector n x -> Natural -> Bool c
testLookup v n = (v !! n) == lookup (from v :: MerkleTree d x) (MerkleTreePath . fmap Bool . indToPath @c @d . embed . Par1 $ fromConstant n)

tossPow :: Natural -> Gen Natural
tossPow m = chooseNatural (0, (2 :: Natural) ^ m -! 1)

genVec :: forall n. KnownNat n => Natural -> Gen (Vector n Natural)
genVec m = do
  a <- replicateM (fromIntegral $ value @n) (tossPow m)
  return $ V.unsafeToVector a

specMerkleTree' :: forall n m p rs d rm.
  ( PrimeField (Zp p)
  , KnownNat n, 2 ^ (d-1) ~ n, KnownNat d, 1 <= d
  , KnownNat m
  , KnownRegisterSize rs
  , rm ~ NumberOfRegisters (Zp p) m rs
  , KnownNat rm
  ) => Spec
specMerkleTree' = do
    describe ("MerkleTree specification") $ do
      it "testId" $ do
        v <- genVec @n (value @m)
        let vI = fmap fromConstant v :: Vector n (UInt m rs (I (Zp p)))
            vA = fmap fromConstant v :: Vector n (UInt m rs (AC (Zp p)))
            bI = evalBoolVec $ testId vI
            bA = evalBool @(Zp p) $ testId vA
            trueEq = one
        return $ bI === bA .&. bI === (one :: Zp p) .&. bA === (one :: Zp p) .&. bI === trueEq
      -- it "testPath" $ do
      --   k <- tossPow (d-1)
      --   return $ testPath @x @c @d @n k
      -- it "testLookup" $ \(v :: Vector n x) -> testLookup v 1

specMerkleTree :: Spec
specMerkleTree = do
    specMerkleTree' @8 @32 @(BLS12_381_Scalar) @Auto @4



--------------------------------------------------------------------------------

-- testPath :: forall x c d n.
--   ( SymbolicOutput x
--   , Context x ~ c
--   , SymbolicFold c
--   , KnownNat d
--   , Zip (Layout x)
--   , n ~ 2 ^ (d-1),1 <= d, KnownNat n
--   , Show x, Eq x
--   ) => Natural -> Property
-- testPath n = (fromConstant n :: UInt n Auto c) === ( strictConv $ ind $ indToPath @c @d . embed . Par1 $ fromConstant n)



-- testFind :: forall c x d n.
--   ( SymbolicInput x
--   , Context x ~ c, KnownNat d
--   , SymbolicFold c
--   , n ~ 2 ^ d
--   , Eq x, Conditional (Bool c) x, Eq (c Par1), Show (M.Maybe c x)
--   ) => (x -> Bool c) -> Vector n x -> Property
-- testFind pred v = M.find pred (toV v) === find (Morph pred :: MorphFrom c x (Bool c)) (from v :: MerkleTree d x)


-- Add tests to verify (among other things) the circuit efficiency of those operations and their compositions.
-- For example, a composition of findPath and lookup should only do d hashes in-circuit (instead of naive 2*d)
-- since we verify the same path from the leaf to the root.
