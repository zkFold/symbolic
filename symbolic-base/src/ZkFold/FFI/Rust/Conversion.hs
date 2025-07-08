{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.Conversion where

import Control.Monad
import Data.Binary
import Data.Bits
import qualified Data.ByteString as BS
import Foreign (
  Ptr,
  Storable (alignment, peek, poke, sizeOf),
  castPtr,
  copyArray,
  finalizerFree,
  newForeignPtr,
  plusPtr,
  pokeArray,
  withForeignPtr,
 )
import Foreign.C.Types
import GHC.Base
import GHC.IO (unsafePerformIO)
import GHC.Num.Natural (naturalFromAddr, naturalToAddr)
import GHC.Ptr (Ptr (..))
import ZkFold.Algebra.Class hiding (sum)
import ZkFold.Algebra.EllipticCurve.BLS12_381 hiding (Fq, Fq12, Fr)
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number
import ZkFold.FFI.Rust.RustFunctions
import ZkFold.FFI.Rust.Types
import Prelude hiding (Eq, Num (..), sum, (*), (/), (^))
import qualified Prelude as P

-- import Control.Monad ((<$))
class RustHaskell r h | r -> h, h -> r where
  h2r :: h -> r
  r2h :: r -> h

instance {-# OVERLAPPABLE #-} (RustHaskell r h, Storable h) => Storable r where
  sizeOf :: r -> Int
  sizeOf _ = sizeOf (undefined :: h)

  alignment :: r -> Int
  alignment _ = alignment (undefined :: h)

  peek :: Ptr r -> IO r
  peek = error "Do not call peek on Rust type"

  --  peekElemOff = error "Do not call peekElemOff on Rust type"
  --  peekByteOff = error "Do not call peekByteOff on Rust type"

  poke :: Ptr r -> r -> IO ()
  poke = error "Do not call poke on Rust type"

--  pokeElemOff = error "Do not call pokeElemOff on Rust type"
--  pokeByteOff = error "Do not call pokeByteOff on Rust type"

pointG1Size :: Int
pointG1Size = sizeOf (undefined :: Rust_BLS12_381_G1_Point)

pointG2Size :: Int
pointG2Size = sizeOf (undefined :: Rust_BLS12_381_G2_Point)

pointGTSize :: Int
pointGTSize = sizeOf (undefined :: BLS12_381_GT)

scalarSize :: Int
scalarSize = sizeOf (undefined :: Fr)

o2nScalar :: Fr -> Fr
o2nScalar old = unsafePerformIO $ do
  withForeignPtr (rawData $ rawScalar old) $ \ptr -> do
    ptrNew <- r_h2r_scalar ptr scalarSize
    RScalar . RData <$> (newForeignPtr rustFinalizer ptrNew)

n2oScalar :: Fr -> Fr
n2oScalar new = unsafePerformIO $ do
  out <- callocForeignPtrBytes @CChar scalarSize
  withForeignPtr (rawData $ rawScalar new) $ \ptr -> do
    withForeignPtr out $ \optr -> do
      r_r2h_scalar ptr optr
  return $ RScalar $ RData out

o2nG1 :: Rust_BLS12_381_G1_Point -> Rust_BLS12_381_G1_Point
o2nG1 old = unsafePerformIO $ do
  withForeignPtr (rawData $ rawPoint old) $ \ptr -> do
    ptrNew <- r_h2r_g1 ptr pointG1Size
    RPoint . RData <$> newForeignPtr rustFinalizer ptrNew

n2oG1 :: Rust_BLS12_381_G1_Point -> Rust_BLS12_381_G1_Point
n2oG1 new = unsafePerformIO $ do
  out <- callocForeignPtrBytes @CChar pointG1Size
  withForeignPtr (rawData $ rawPoint new) $ \ptr -> do
    withForeignPtr out $ \optr -> do
      r_r2h_g1 ptr optr
  return $ RPoint $ RData out

pokeNatural :: Ptr Natural -> Natural -> IO ()
pokeNatural (Ptr addr) n = void (naturalToAddr n addr 0#)

peekNatural :: Int -> Ptr Natural -> IO Natural
peekNatural (I# size) (Ptr addr) = naturalFromAddr (int2Word# size) addr 0#

-- Fr

instance Storable EC.Fr where
  sizeOf :: EC.Fr -> Int
  sizeOf _ = 32

  alignment :: EC.Fr -> Int
  alignment _ = 8

  peek :: Ptr EC.Fr -> IO EC.Fr
  peek = peekZpLE (sizeOf @EC.Fr undefined)

  poke :: Ptr EC.Fr -> EC.Fr -> IO ()
  poke = pokeZpLE

instance RustHaskell Fr EC.Fr where
  r2h s =
    {-# SCC "r2h_fr" #-}
    unsafePerformIO $
      withForeignPtr fptr $ \ptr -> do
        peek (castPtr $ ptr)
   where
    fptr = rawData $ rawScalar $ n2oScalar s

  h2r p =
    {-# SCC "h2r_fr" #-}
    o2nScalar $ unsafePerformIO $ do
      fptr <- callocForeignPtrBytes (sizeOf (undefined :: EC.Fr))
      withForeignPtr fptr $ \ptr -> do
        poke (castPtr ptr) p
      return $ RScalar $ RData fptr

-- Fq

instance Storable EC.Fq where
  sizeOf :: EC.Fq -> Int
  sizeOf _ = 48

  alignment :: EC.Fq -> Int
  alignment _ = 8

  peek :: Ptr EC.Fq -> IO EC.Fq
  peek = peekZpLE (sizeOf @EC.Fq undefined)

  poke :: Ptr EC.Fq -> EC.Fq -> IO ()
  poke = pokeZpLE

-- G1

instance Storable BLS12_381_G1_Point where
  sizeOf :: BLS12_381_G1_Point -> Int
  sizeOf _ = 96

  alignment :: BLS12_381_G1_Point -> Int
  alignment _ = 8

  peek :: Ptr BLS12_381_G1_Point -> IO BLS12_381_G1_Point
  peek ptr = do
    a <- BS.packCStringLen (castPtr ptr, sizeOf @BLS12_381_G1_Point undefined)
    if BS.pack infByteStringRepr == a
      then return $ Weierstrass $ Point zero one True
      else do
        x <- peek @EC.Fq (castPtr ptr)
        y <- peek @EC.Fq (ptr `plusPtr` sizeOf @EC.Fq undefined)
        return $ Weierstrass $ Point x y False

  poke :: Ptr BLS12_381_G1_Point -> BLS12_381_G1_Point -> IO ()
  poke ptr (Weierstrass (Point _ _ True)) = pokeArray (castPtr ptr) infByteStringRepr
  poke ptr (Weierstrass (Point x y False)) = do
    poke (castPtr ptr) x
    poke (castPtr ptr `plusPtr` sizeOf @EC.Fq undefined) y

instance Storable BLS12_381_G1_JacobianPoint where
  sizeOf :: BLS12_381_G1_JacobianPoint -> Int
  sizeOf _ = sizeOf @BLS12_381_G1_Point undefined

  alignment :: BLS12_381_G1_JacobianPoint -> Int
  alignment _ = alignment @BLS12_381_G1_Point undefined

  peek :: Ptr BLS12_381_G1_JacobianPoint -> IO BLS12_381_G1_JacobianPoint
  peek ptr = project @BLS12_381_G1_Point <$> peek (castPtr ptr)

  poke :: Ptr BLS12_381_G1_JacobianPoint -> BLS12_381_G1_JacobianPoint -> IO ()
  poke ptr pt = poke (castPtr ptr) (project @_ @BLS12_381_G1_Point pt)

instance RustHaskell Rust_BLS12_381_G1_Point BLS12_381_G1_Point where
  r2h s =
    {-# SCC "r2h_bls_g1" #-}
    unsafePerformIO $
      withForeignPtr fptr $ \ptr -> do
        peek (castPtr $ ptr)
   where
    fptr = rawData $ rawPoint $ n2oG1 s

  h2r p =
    {-# SCC "h2r_bls_g1" #-}
    o2nG1 $ unsafePerformIO $ do
      fptr <- callocForeignPtrBytes (sizeOf (undefined :: BLS12_381_G1_Point))
      withForeignPtr fptr $ \ptr -> do
        poke (castPtr ptr) p
      return $ RPoint $ RData fptr

instance RustHaskell Rust_BLS12_381_G1_JacobianPoint BLS12_381_G1_JacobianPoint where
  r2h (G1_Jacobian rp) = project @BLS12_381_G1_Point $ r2h rp

  h2r p = G1_Jacobian $ h2r (project @_ @BLS12_381_G1_Point p)

-- G2

infByteStringRepr :: [Word8]
infByteStringRepr = replicate 47 0 <> (bit 6 : replicate 48 0)

instance Storable BLS12_381_G2_Point where
  sizeOf :: BLS12_381_G2_Point -> Int
  sizeOf _ = 192

  alignment :: BLS12_381_G2_Point -> Int
  alignment _ = 8

  peek :: Ptr BLS12_381_G2_Point -> IO BLS12_381_G2_Point
  peek ptr =
    decode . BS.fromStrict
      <$> BS.packCStringLen (castPtr ptr, sizeOf @BLS12_381_G2_Point undefined)

  poke :: Ptr BLS12_381_G2_Point -> BLS12_381_G2_Point -> IO ()
  poke ptr p =
    BS.useAsCStringLen
      (BS.toStrict $ encode p)
      (\(fptr, len) -> copyArray (castPtr $ ptr) fptr len)

instance Storable BLS12_381_G2_JacobianPoint where
  sizeOf :: BLS12_381_G2_JacobianPoint -> Int
  sizeOf _ = sizeOf @BLS12_381_G2_Point undefined

  alignment :: BLS12_381_G2_JacobianPoint -> Int
  alignment _ = alignment @BLS12_381_G2_Point undefined

  peek :: Ptr BLS12_381_G2_JacobianPoint -> IO BLS12_381_G2_JacobianPoint
  peek ptr = project @BLS12_381_G2_Point <$> peek (castPtr ptr)

  poke :: Ptr BLS12_381_G2_JacobianPoint -> BLS12_381_G2_JacobianPoint -> IO ()
  poke ptr pt = poke (castPtr ptr) (project @_ @BLS12_381_G2_Point pt)

o2nG2 :: Rust_BLS12_381_G2_Point -> Rust_BLS12_381_G2_Point
o2nG2 old = unsafePerformIO $ do
  withForeignPtr (rawData $ rawPoint old) $ \ptr -> do
    ptrNew <- r_h2r_g2 ptr pointG2Size
    RPoint . RData <$> newForeignPtr rustFinalizer ptrNew

n2oG2 :: Rust_BLS12_381_G2_Point -> Rust_BLS12_381_G2_Point
n2oG2 new = unsafePerformIO $ do
  out <- callocForeignPtrBytes @CChar pointG2Size
  withForeignPtr (rawData $ rawPoint new) $ \ptr -> do
    withForeignPtr out $ \optr -> do
      r_r2h_g2 ptr optr
  return $ RPoint $ RData out

instance RustHaskell Rust_BLS12_381_G2_Point BLS12_381_G2_Point where
  r2h p = unsafePerformIO $
    withForeignPtr (rawData $ rawPoint $ n2oG2 p) $ \ptr -> do
      peek (castPtr $ ptr)

  h2r p = o2nG2 $ unsafePerformIO $ do
    fptr <- callocForeignPtrBytes (sizeOf (undefined :: BLS12_381_G2_Point))
    withForeignPtr fptr $ \ptr -> do
      poke (castPtr ptr) (p)
    return $ RPoint $ RData fptr

instance RustHaskell Rust_BLS12_381_G2_JacobianPoint BLS12_381_G2_JacobianPoint where
  r2h (G2_Jacobian rp) = project @BLS12_381_G2_Point $ r2h rp

  h2r p = G2_Jacobian $ h2r (project @_ @BLS12_381_G2_Point p)

instance Storable EC.BLS12_381_GT where
  sizeOf _ = 12 P.* (sizeOf (undefined :: EC.Fq))

  alignment _ = alignment (undefined :: EC.Fq)

  peek ptr =
    BLS12_381_GT . decode . BS.fromStrict
      <$> BS.packCStringLen (castPtr ptr, sizeOf @EC.BLS12_381_GT undefined)

  poke ptr (BLS12_381_GT p) =
    BS.useAsCStringLen
      (BS.toStrict $ encode p)
      (\(fptr, len) -> copyArray (castPtr ptr) fptr len)

instance RustHaskell Fq12 BLS12_381_GT where
  r2h p = unsafePerformIO $
    withForeignPtr (rawData $ rawScalar $ n2oGT p) $ \ptr -> do
      peek (castPtr $ ptr)

  h2r p = o2nGT $ unsafePerformIO $ do
    fptr <- callocForeignPtrBytes (sizeOf (undefined :: EC.BLS12_381_GT))
    withForeignPtr fptr $ \ptr -> do
      poke (castPtr ptr) p
    return $ RScalar $ RData fptr

o2nGT :: Fq12 -> Fq12
o2nGT old = unsafePerformIO $ do
  withForeignPtr (rawData $ rawScalar old) $ \ptr -> do
    ptrNew <- r_h2r_gt ptr pointGTSize
    RScalar . RData <$> newForeignPtr rustFinalizer ptrNew

n2oGT :: Fq12 -> Fq12
n2oGT new = unsafePerformIO $ do
  out <- callocForeignPtrBytes @CChar pointGTSize
  withForeignPtr (rawData $ rawScalar new) $ \ptr -> do
    withForeignPtr out $ \optr -> do
      r_r2h_gt ptr optr
  return $ RScalar $ RData out

-- Zp

peekZpLE :: KnownNat a => Int -> Ptr (Zp a) -> IO (Zp a)
peekZpLE size ptr = toZp . toInteger <$> peekNatural size (castPtr ptr)

pokeZpLE :: Ptr (Zp a) -> Zp a -> IO ()
pokeZpLE ptr p = pokeNatural (castPtr ptr) (fromZp p)
