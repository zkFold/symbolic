module ZkFold.Protocol.NonInteractiveProof.TrustedSetup (
  readPoints,
) where

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import GHC.IO.Handle (BufferMode (..), SeekMode (..), hSeek, hSetBuffering)
import GHC.IO.StdHandles (openBinaryFile)
import System.IO (IOMode (..))
import Prelude (FilePath, IO, ($), (.))
import qualified Prelude as P

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Data.Binary

readPoints :: FilePath -> IO (Vector BLS12_381_G1_JacobianPoint, Vector BLS12_381_G2_JacobianPoint)
readPoints fp = do
  handle <- openBinaryFile fp ReadMode
  hSetBuffering handle NoBuffering

  pts <- replicateM 5 $ do
    buf <- BS.hGet handle 96
    let pt = fromByteString buf :: P.Maybe BLS12_381_G1_JacobianPoint
    hSeek handle RelativeSeek 96
    P.pure pt

  P.print pts
  P.undefined
