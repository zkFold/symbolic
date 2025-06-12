{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}

module ZkFold.FFI.Rust.Conversion where

import           Control.Monad
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString                        as BS
import           Foreign
import           GHC.Base
import           GHC.IO                                 (unsafePerformIO)
import           GHC.Num.Integer                        (integerToInt#)
import           GHC.Num.Natural                        (naturalFromAddr, naturalToAddr)
import           GHC.Ptr                                (Ptr (..))
import           Prelude                                hiding (Eq, Num (..), sum, (/), (^))

import           ZkFold.Algebra.Class                   hiding (sum)
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import           ZkFold.Algebra.EllipticCurve.BLS12_381 hiding (Fq, Fr)
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.Field
import           ZkFold.Algebra.Number
import           ZkFold.FFI.Rust.Types

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
  r2h (RScalar (RData fptr)) = {-# SCC r2h_fr #-} unsafePerformIO $
    withForeignPtr fptr $ \ptr -> do
    peek (castPtr $ ptr)

  h2r p = {-# SCC h2r_fr #-} unsafePerformIO $ do
    fptr <- callocForeignPtrBytes (sizeOf (undefined :: EC.Fr))
    withForeignPtr fptr $ \ptr -> do
      poke (castPtr ptr) p
    return $ RScalar $ RData fptr

-- Fq

instance Storable EC.Fq where

  sizeOf :: EC.Fq -> Int
  sizeOf    _ = 48

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
    then return $ Weierstrass $ Point zero zero True
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
  r2h (RPoint (RData fptr)) = {-# SCC r2h_bls_g1 #-} unsafePerformIO $
    withForeignPtr fptr $ \ptr -> do
    peek (castPtr $ ptr)

  h2r p = {-# SCC h2r_bls_g1 #-} unsafePerformIO $ do
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
  peek ptr = decode . BS.fromStrict <$>
    BS.packCStringLen (castPtr ptr, sizeOf @BLS12_381_G2_Point undefined)

  poke :: Ptr BLS12_381_G2_Point -> BLS12_381_G2_Point -> IO ()
  poke ptr p =
    BS.useAsCStringLen (BS.toStrict $ encode p) (
      \(fptr, len) -> copyArray (castPtr $ ptr) fptr len
        )

instance Storable BLS12_381_G2_JacobianPoint where
  sizeOf :: BLS12_381_G2_JacobianPoint -> Int
  sizeOf _ = sizeOf @BLS12_381_G2_Point undefined

  alignment :: BLS12_381_G2_JacobianPoint -> Int
  alignment _ = alignment @BLS12_381_G2_Point undefined

  peek :: Ptr BLS12_381_G2_JacobianPoint -> IO BLS12_381_G2_JacobianPoint
  peek ptr = project @BLS12_381_G2_Point <$> peek (castPtr ptr) 

  poke :: Ptr BLS12_381_G2_JacobianPoint -> BLS12_381_G2_JacobianPoint -> IO ()
  poke ptr pt = poke (castPtr ptr) (project @_ @BLS12_381_G2_Point pt)

instance RustHaskell Rust_BLS12_381_G2_Point BLS12_381_G2_Point where
  r2h (RPoint (RData fptr)) = unsafePerformIO $
    withForeignPtr fptr $ \ptr -> do
    peek (castPtr $ ptr)

  h2r p = unsafePerformIO $ do
    fptr <- callocForeignPtrBytes (sizeOf (undefined :: BLS12_381_G2_Point))
    withForeignPtr fptr $ \ptr -> do
      poke (castPtr ptr) p
    return $ RPoint $ RData fptr

instance RustHaskell Rust_BLS12_381_G2_JacobianPoint BLS12_381_G2_JacobianPoint where
  r2h (G2_Jacobian rp) = project @BLS12_381_G2_Point $ r2h rp

  h2r p = G2_Jacobian $ h2r (project @_ @BLS12_381_G2_Point p)


-- Zp

peekZpLE :: KnownNat a => Int -> Ptr (Zp a) -> IO (Zp a)
peekZpLE size ptr = do
    let !(Ptr addr) = ptr
    toZp . toInteger <$> naturalFromAddr (int2Word# (integerToInt# $ toInteger size)) addr 0#

pokeZpLE :: Ptr (Zp a) -> Zp a -> IO ()
pokeZpLE ptr p = do
    let !(Ptr addr) = ptr
    !_ <- naturalToAddr (fromZp p) addr 0#
    return ()
