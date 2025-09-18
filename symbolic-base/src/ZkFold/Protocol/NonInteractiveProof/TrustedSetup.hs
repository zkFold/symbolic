{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.NonInteractiveProof.TrustedSetup (
  readTrustedSetup,
  saveTrustedSetup,
  powersOfTau2e18p6,
  powersOfTauSubset,
  TrustedSetup (..),
) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Maybe (Maybe (..))
import GHC.Generics (Generic)
import GHC.IO.Handle (BufferMode (..), SeekMode (..), hSeek, hSetBuffering)
import GHC.IO.StdHandles (openBinaryFile)
import System.IO (Handle, IOMode (..), hClose)
import Prelude (Bool (..), FilePath, IO, otherwise, ($), (.), (<$>), (<*>))
import qualified Prelude as P

import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Binary
import ZkFold.Data.Vector (Vector)
import qualified ZkFold.Data.Vector as V

data TrustedSetup (n :: Natural)
  = TrustedSetup
  { g1s :: Vector n BLS12_381_G1_JacobianPoint
  , g2_0 :: BLS12_381_G2_JacobianPoint
  , g2_1 :: BLS12_381_G2_JacobianPoint
  }
  deriving (Generic, P.Show)

powersOfTau2e18p6 :: IO (TrustedSetup (2 ^ 18 + 6))
powersOfTau2e18p6 = powersOfTauSubset

powersOfTauSubset :: forall (n :: Natural). KnownNat n => IO (TrustedSetup n)
powersOfTauSubset = do
  Just ts <- readTrustedSetup "data/midnight_powers_of_tau_2e18" True
  P.pure ts

readTrustedSetup
  :: forall (n :: Natural)
   . KnownNat n
  => FilePath
  -> Bool
  -> IO (Maybe (TrustedSetup n))
readTrustedSetup fp isCompressed = do
  handle <- openBinaryFile fp ReadMode
  hSetBuffering handle NoBuffering

  !g1s <- V.replicateM @n $ getG handle g1Size

  hSeek handle SeekFromEnd $ P.fromIntegral (-2 P.* g2Size)

  !g20 <- getG handle g2Size
  !g21 <- getG handle g2Size

  hClose handle

  P.pure $ TrustedSetup <$> P.sequence g1s <*> g20 <*> g21
 where
  g1Size, g2Size :: P.Int
  g1Size = if isCompressed then 48 else 96
  g2Size = 192

  getG :: forall g. Binary g => Handle -> P.Int -> IO (Maybe g)
  getG handle size = do
    buf <- BS.hGet handle size
    let pt = fromByteString buf
    P.pure pt

saveTrustedSetup
  :: forall (n :: Natural)
   . FilePath
  -> Bool
  -> TrustedSetup n
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
