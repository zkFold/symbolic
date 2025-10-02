{-# OPTIONS_GHC -Wno-orphans #-}
module ZkFold.FFI.Rust.Halo2 where
import ZkFold.FFI.Rust.Types hiding (length)
import ZkFold.Protocol.Halo2
import ZkFold.FFI.Rust.Conversion
import ZkFold.Data.Binary
import Data.Vector.Storable (Storable)
import GHC.Base (undefined)
import GHC.IO (unsafePerformIO)
import Foreign.Ptr
import Prelude
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign (withForeignPtr)
import Foreign.C.Types
import Control.Monad
import ZkFold.Algebra.Number (Natural)
import Data.Binary (Word64)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup(ScalarFieldOf))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point)
import Foreign.C.String
-- import Data.Vector.Fusion.Bundle.Monadic (scanl1M')

instance forall a . Storable a => Storable [a] where
  sizeOf a = sizeOf (undefined :: Int) + length a * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    len <- peek (castPtr ptr)
    peekArray len (castPtr ptr `plusPtr` sizeOf (undefined :: Int))
  poke ptr a = do
    poke (castPtr ptr) (length a)
    pokeArray (castPtr ptr `plusPtr` sizeOf (undefined :: Int)) a

h2rGeneric :: Storable a => a -> RustData
h2rGeneric a = unsafePerformIO $ do
    fptr <- callocForeignPtrBytes @CChar (sizeOf a)
    withForeignPtr fptr $ \ptr -> do
        poke (castPtr ptr) a
        pure $ RData fptr

r2hGeneric :: Storable a => RustData -> a
r2hGeneric r = unsafePerformIO $ do
    withForeignPtr (rawData r) $ \ptr -> do
        peek (castPtr ptr)

pokeWithOff :: Storable a => Ptr b -> Int -> a -> IO Int
pokeWithOff ptr off x = do
    poke (castPtr ptr `plusPtr` off) x
    pure $ off + sizeOf x

peekWithOff :: Storable a => Ptr c -> IO (Int, a) -> b -> IO (Int, a)
peekWithOff ptr input _ = do
    (off, _) <- input
    res <- peek (castPtr ptr `plusPtr` off)
    pure (off + sizeOf res, res)

newtype RustPlonkupWitnessData a = RPlonkupWitnessData { rawPlonkupWitnessData :: RustData }

instance (Storable a) => Storable (PlonkupWitnessData a) where
  sizeOf PlonkupWitnessData {..} = sum $ sizeOf <$> [w1, w2, w3]
  alignment _ = alignment (undefined :: a)
  peek ptr' = do
    [w1, w2, w3] <- (snd <$>) . tail <$> sequence (scanl (peekWithOff @[a] (castPtr ptr')) (pure (0, undefined)) (replicate 3 undefined))
    pure $ PlonkupWitnessData {..}

  poke ptr (PlonkupWitnessData {..}) = do
    foldM_ (pokeWithOff ptr) 0 [w1, w2, w3]


instance (Storable a) => RustHaskell (RustPlonkupWitnessData a) (PlonkupWitnessData a) where
    h2r = RPlonkupWitnessData . h2rGeneric
    r2h = r2hGeneric . rawPlonkupWitnessData

newtype RustPlonkupSelectors a = RPlonkupSelectors { rawPlonkupSelectors :: RustData }

instance (Storable a) => Storable (PlonkupSelectors a) where
  sizeOf PlonkupSelectors {..} = sum $ sizeOf <$> [qMul, qLeft, qRight, qOutput, qConst, qLookup]
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    [qMul, qLeft, qRight, qOutput, qConst, qLookup] <- 
        tail . fmap snd <$> 
            sequence (scanl (peekWithOff ptr) (pure (0, undefined)) (replicate 6 undefined))
    pure $ PlonkupSelectors {..}

  poke ptr PlonkupSelectors {..} = do
    foldM_ (pokeWithOff ptr) 0 [qMul, qLeft, qRight, qOutput, qConst, qLookup]


instance (Storable a) => RustHaskell (RustPlonkupSelectors a) (PlonkupSelectors a) where
  h2r = RPlonkupSelectors . h2rGeneric
  r2h = r2hGeneric . rawPlonkupSelectors

newtype RustPlonkupTableData a = RPlonkupTableData { rawPlonkupTableData :: RustData }

instance (Storable a) => Storable (PlonkupTableData a) where
  sizeOf PlonkupTableData {..} = sum $ sizeOf <$> [table1, table2, table3]
  alignment _ = alignment (undefined :: a)
  peek ptr' = do
    [table1, table2, table3] <- (snd <$>) . tail <$> sequence (scanl (peekWithOff @[a] (castPtr ptr')) (pure (0, undefined)) (replicate 3 undefined))
    pure $ PlonkupTableData {..}

  poke ptr (PlonkupTableData {..}) = do
    foldM_ (pokeWithOff ptr) 0 [table1, table2, table3]

instance (Storable a) => RustHaskell (RustPlonkupTableData a) (PlonkupTableData a) where
  h2r = RPlonkupTableData . h2rGeneric
  r2h = r2hGeneric . rawPlonkupTableData

newtype RustPlonkupCircuit a = RPlonkupCircuit { rawPlonkupCircuit :: RustData }

naturalToUInt64 :: Natural -> Word64
naturalToUInt64 = fromIntegral

mapTup4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
mapTup4 f (a, b, c, d) = (f a, f b, f c, f d)

instance Storable a => Storable (a, a, a, a) where
  sizeOf (a, b, c, d) = sum $ sizeOf <$> [a, b, c, d] 

  alignment (a, _, _, _) = alignment a

  peek ptr = do
    [a, b, c, d] <- (snd <$> ) . tail <$> sequence (scanl (peekWithOff @a (castPtr ptr)) (pure (0, undefined)) (replicate 4 undefined))
    pure (a, b, c, d)

  poke ptr (a, b, c, d) = do
    foldM_ (pokeWithOff ptr) 0 [a, b, c, d]

instance Storable a => Storable (PlonkupCircuit a) where
  sizeOf PlonkupCircuit {..} = sum [sizeOf $ naturalToUInt64 circuitSize, sizeOf witness, sizeOf selectors, sizeOf table, sizeOf $ mapTup4 naturalToUInt64 <$> copyConstraints]

  alignment _ = alignment (undefined :: a)

  poke ptr PlonkupCircuit {..} = do
    offSize <- pokeWithOff ptr 0 (naturalToUInt64 circuitSize)
    offWitness <- pokeWithOff ptr offSize witness
    offSelectors <- pokeWithOff ptr offWitness selectors 
    offTable <- pokeWithOff ptr offSelectors table
    void $ pokeWithOff ptr offTable (mapTup4 naturalToUInt64 <$> copyConstraints)
    
  peek _ = error "Not implemented: peek for PlonkupCircuit"

instance (Storable a) => RustHaskell (RustPlonkupCircuit a) (PlonkupCircuit a) where
  h2r = RPlonkupCircuit . h2rGeneric
  r2h = error "Not implemented: r2h for PlonkupCircuit"



foreign import ccall unsafe "r_halo2_prove"
  r_halo2_prove :: CString -> IO ()


