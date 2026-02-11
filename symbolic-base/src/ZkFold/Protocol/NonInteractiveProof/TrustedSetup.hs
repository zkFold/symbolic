{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module ZkFold.Protocol.NonInteractiveProof.TrustedSetup (
  readTrustedSetup,
  saveTrustedSetup,
  powersOfTau2e18p6,
  powersOfTauSubset,
  TrustedSetup (..),
) where

import Control.Monad (forM_, mapM)
import qualified Data.ByteString as BS
import Data.Maybe (Maybe (..))
import Data.Type.Equality (type (~))
import GHC.Generics (Generic)
import GHC.IO.Handle (BufferMode (..), SeekMode (..), hSeek, hSetBuffering)
import GHC.IO.StdHandles (openBinaryFile)
import System.IO (Handle, IOMode (..), hClose)
import Prelude (Bool (..), FilePath, IO, otherwise, ($), (.), (>>=))
import qualified Prelude as P

import Paths_symbolic_base
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Binary
import ZkFold.Data.Eq
import ZkFold.Data.Vector (Vector)
import qualified ZkFold.Data.Vector as V

data TrustedSetup (n :: Natural)
  = TrustedSetup
  { g1s :: Vector n BLS12_381_G1_JacobianPoint
  , g2_0 :: BLS12_381_G2_JacobianPoint
  , g2_1 :: BLS12_381_G2_JacobianPoint
  }
  deriving (Generic, P.Show)

-- | Read the the first 2^18 + 6 G1 points and both G2 points from the Midnight trusted setup.
-- See https://github.com/midnightntwrk/midnight-trusted-setup/tree/main
powersOfTau2e18p6 :: IO (TrustedSetup (2 ^ 18 + 6))
powersOfTau2e18p6 = powersOfTauSubset

-- | Read no more than 2^18 + 6 G1 points and both G2 points from the Midnight trusted setup.
powersOfTauSubset :: forall (n :: Natural). KnownNat n => IO (TrustedSetup n)
powersOfTauSubset = do
  let n = value @n
  fp <- if n P.<= 2 P.^ 18 P.+ 6
    then getDataFileName "data/midnight_powers_of_tau_2e18"
    else getDataFileName "data/powers_of_tau_2e20"
  Just ts <- readTrustedSetup fp True
  P.pure ts

-- | Parse the trusted setup file and extract @n@ G1 points and both G2 points.
readTrustedSetup
  :: forall (n :: Natural)
   . KnownNat n
  => FilePath
  -- ^ Path to the file
  -> Bool
  -- ^ whether G1 points are compressed. Does not affect G2 points as they are never compressed
  -> IO (Maybe (TrustedSetup n))
readTrustedSetup fp isCompressed = do
  handle <- openBinaryFile fp ReadMode
  hSetBuffering handle NoBuffering

  !g1s <- V.replicateM @n $ getG handle g1Size

  hSeek handle SeekFromEnd $ P.fromIntegral (-2 P.* g2Size)

  !g20 <- getG handle g2Size
  !g21 <- getG handle g2Size

  hClose handle

  -- sanity check: make sure all the points lie on the curve
  let ts = do
        g1s' <- mapM (>>= checkPt) g1s
        g20' <- g20 >>= checkPt
        g21' <- g21 >>= checkPt
        P.pure $ TrustedSetup g1s' g20' g21'

  P.pure ts
 where
  g1Size, g2Size :: P.Int
  g1Size = if isCompressed then 48 else 96
  g2Size = 192

  checkPt :: (EllipticCurve pt, BooleanOf (BaseFieldOf pt) ~ Bool) => pt -> Maybe pt
  checkPt pt = if isOnCurve pt then Just pt else Nothing

  getG :: forall g. Binary g => Handle -> P.Int -> IO (Maybe g)
  getG handle size = do
    buf <- BS.hGet handle size
    let pt = fromByteString buf
    P.pure pt

-- | Save the trusted setup to a file.
saveTrustedSetup
  :: forall (n :: Natural)
   . FilePath
  -- ^ Path to the file
  -> Bool
  -- ^ whether to compress G1 points. Does not affect G2 points as they are never compressed
  -> TrustedSetup n
  -- ^ Trusted Setup to save
  -> IO ()
saveTrustedSetup fp isCompressed TrustedSetup {..} = do
  handle <- openBinaryFile fp WriteMode

  forM_ g1s $ BS.hPut handle . toBS
  BS.hPut handle . toByteString $ g2_0
  BS.hPut handle . toByteString $ g2_1

  hClose handle
 where
  toBS :: forall pt. (Compressible pt, Binary pt, Binary (Compressed pt)) => pt -> BS.ByteString
  toBS pt
    | isCompressed = toByteString $ compress pt
    | otherwise = toByteString pt
