{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}

module Tests.Symbolic.Data.MerkleTree
  ( specMerkleTree
  ) where

import           Data.Binary                                 (Binary)
import           GHC.Generics                                (Par1 (Par1), U1 (..), type (:*:) ((:*:)))
import           Prelude                                     (return, ($), (.))
import qualified Prelude                                     as Haskell
import           Test.Hspec                                  (Spec, describe)
import           Test.Hspec.QuickCheck                       (prop)
import           Test.QuickCheck                             (Arbitrary, Gen)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Field                        (Zp)
import           ZkFold.Algebra.Number                       (KnownNat, Natural, value)
import           ZkFold.Algebra.EllipticCurve.BLS12_381      (BLS12_381_Scalar)
import qualified ZkFold.Data.Vector                          as V
import           ZkFold.Data.Vector                          (Vector (..))
import           ZkFold.Prelude                              (chooseNatural)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler                    (compile)
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit  (eval1)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Combinators            (Iso (..))
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement           (FieldElement (FieldElement))
import qualified ZkFold.Symbolic.Data.List                   as L
import           ZkFold.Symbolic.Data.List                   (emptyList, lSize)
import           ZkFold.Symbolic.Data.MerkleTree
import           ZkFold.Symbolic.Data.Morph                  (MorphTo (..))
import           ZkFold.Symbolic.Fold
import           ZkFold.Symbolic.Interpreter                 (Interpreter (..))

-- evalBool :: forall a . (Arithmetic a, Binary a) => Bool (AC a) -> a
-- evalBool (Bool ac) = exec1 ac

evalBoolVec :: forall a . Bool (Interpreter a) -> a
evalBoolVec (Bool (Interpreter (Par1 v))) = v

-- testId :: forall c x d n.
--   ( SymbolicOutput x
--   , Context x ~ c
--   , SymbolicFold c
--   , KnownNat d
--   , n ~ 2 ^ (d-1),1 <= d
--   , Eq x, BooleanOf x ~ Bool c
--   , KnownNat n
  -- ) => Vector n x -> Bool c
-- testId v = v == (from (from v :: MerkleTree d x) :: Vector n x)

testIso2 :: forall c. (SymbolicFold c) => FieldElement c -> FieldElement c -> Bool c
testIso2 x y = let v = V.unsafeToVector [x,y]
   in v == (from (from v :: MerkleTree 2 (FieldElement c)) :: Vector 2 (FieldElement c))

testIso3 :: forall c. (SymbolicFold c) => FieldElement c -> FieldElement c -> FieldElement c -> FieldElement c -> Bool c
testIso3 x y z w = let v = V.unsafeToVector [x,y,z,w]
   in v == (from (from v :: MerkleTree 3 (FieldElement c)) :: Vector 4 (FieldElement c))

testPath :: forall d c. (Symbolic c, KnownNat d) => FieldElement c -> Bool c
testPath fe@(FieldElement e) = fe == (FieldElement . ind $ indToPath @c @d e)

testLayerFolding :: forall c. (SymbolicFold c) => FieldElement c -> FieldElement c -> FieldElement c -> FieldElement c -> Bool c
testLayerFolding x y z w =
  let l = x L..: y L..: z L..: w L..: emptyList
      m = L.foldr (Morph \(a :: FieldElement s, b :: L.List s (FieldElement s)) -> a L..: b)
            (emptyList :: L.List c (FieldElement c)) l
   in fromConstant (4 :: Natural) == FieldElement (lSize l)
      &&
      fromConstant (4 :: Natural) == FieldElement (lSize m)

-- testLookup :: forall c.
--   ( SymbolicFold c
--   , KnownNat (NumberOfRegisters (BaseField c) (NumberOfBits (BaseField c) ) 'Auto)
--   ) => FieldElement c -> FieldElement c -> FieldElement c -> FieldElement c -> FieldElement c -> Bool c
-- testLookup fe x y z w =
--   let v = V.unsafeToVector [x,y,z,w]
--       l = x L..: y L..: z L..: w L..: emptyList
--       mt = from v :: MerkleTree 3 (FieldElement c)
--       n = from fe
--       mp = MerkleTreePath (Bool <$> indToPath @c (fromFieldElement fe)) :: MerkleTreePath 3 c
--    in (l L.!! n) == lookup mt mp

-- testFind :: forall c. (SymbolicFold c) => BaseField c -> BaseField c -> FieldElement c -> FieldElement c -> FieldElement c -> FieldElement c -> Bool c
-- testFind aw out x y z w =
--   let v = V.unsafeToVector [x,y,z,w]
--       mt = from v :: MerkleTree 3 (FieldElement c)
--       p ::  BaseField c -> MorphFrom c (FieldElement c) (Bool c)
--       p t = Morph \(a :: FieldElement s) -> fromConstant t == a
--       fw = find (p aw) mt
--       fOut = find (p out) mt
--    in (fOut == nothing) && (fromConstant aw == fromJust fw)

tossPow :: Natural -> Gen Natural
tossPow m = chooseNatural (0, (2 :: Natural) ^ m -! 1)

-- genVec :: forall n. KnownNat n => Natural -> Gen (Vector n Natural)
-- genVec m = do
--   a <- replicateM (fromIntegral $ value @n) (tossPow m)
  -- return $ V.unsafeToVector a

specMerkleTree' :: forall a d.
  ( PrimeField a
  , KnownNat d
  , Arbitrary a, Arithmetic a, Binary a, Haskell.Show a
  ) => Spec
specMerkleTree' = do
    describe "MerkleTree specification" $ do
      prop "testId2" $ \x y ->
        let ac = eval1 (compile @a testIso2) ((U1 :*: U1 :*: U1) :*: Par1 x :*: Par1 y :*: U1)
            i  = evalBoolVec $ testIso2 (fromConstant x) (fromConstant y)
         in ac Haskell.== i && (i Haskell.== one)
      prop "testId3" $ \x y z w ->
        let ac = eval1 (compile @a testIso3) ((U1 :*: U1 :*: U1 :*: U1 :*: U1) :*: Par1 x :*: Par1 y :*: Par1 z :*: Par1 w :*: U1)
            i  = evalBoolVec $ testIso3 (fromConstant x) (fromConstant y) (fromConstant z) (fromConstant w)
         in ac Haskell.== i && (i Haskell.== one)
      prop "testPath" $ do
        r <- tossPow (value @d)
        let ac = eval1 (compile @a (testPath @d)) ((U1 :*: U1) :*: Par1 (fromConstant r) :*: U1)
            i  = evalBoolVec $ testPath @d (fromConstant r)
        return $ ac Haskell.== i && (i Haskell.== one)
      prop "testLayerFolding" $ \x y z w ->
        let ac = eval1 (compile @a testLayerFolding) ((U1 :*: U1 :*: U1 :*: U1 :*: U1) :*: Par1 x :*: Par1 y :*: Par1 z :*: Par1 w :*: U1)
            i  = evalBoolVec $ testLayerFolding (fromConstant x) (fromConstant y) (fromConstant z) (fromConstant w)
         in ac Haskell.== i && (i Haskell.== one)
      -- prop "testLookup" $ \x y z w -> do
      --   r <- tossPow d
      --   let ac = eval1 (compile @a testLookup) ((U1 :*: U1 :*: U1 :*: U1 :*: U1 :*: U1) :*: Par1 (fromConstant r) :*: Par1 x :*: Par1 y :*: Par1 z :*: Par1 w :*: U1)
      --       i  = evalBoolVec $ testLookup (fromConstant r) (fromConstant x) (fromConstant y) (fromConstant z) (fromConstant w)
      --   return $ (ac, i) === (i, one)
      -- -- TODO: benchmark this test and optimize find
      -- prop "testFind" $ \x y z -> do
      --   w   <- fromConstant <$> tossPow d :: Gen a
      --   out <- fromConstant <$> tossPow d :: Gen a
      --   let ac = eval1 (compile @a (testFind w out)) ((U1 :*: U1 :*: U1 :*: U1 :*: U1) :*: Par1 x :*: Par1 y :*: Par1 z :*: Par1 w :*: U1)
      --       (xf, yf, zf, wf, outf) = (fromConstant x ::  FieldElement (I a), fromConstant y, fromConstant z, fromConstant w, fromConstant out)
      --       i  = evalBoolVec $ testFind w out xf yf zf wf
      --       c  = evalBoolVec $ (outf /= xf) && (outf /= yf) && (outf /= zf) && (outf /= wf)
      --   return $ xor (ac Haskell.== i) (i Haskell.== one) `xor` (c Haskell.== one)

specMerkleTree :: Spec
specMerkleTree = do
    specMerkleTree' @(Zp BLS12_381_Scalar) @4
