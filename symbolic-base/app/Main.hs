module Main where

-- import Prelude
import ZkFold.FFI.Rust.RustFunctions

import           Foreign.C.String

import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import           Control.Monad
import qualified Data.Bool
import           Foreign
import           Foreign.C.Types
import           GHC.Base
import           GHC.IO                                 (unsafePerformIO)
import           GHC.Natural                            (naturalToInteger)
import           Prelude                                hiding (fromIntegral, negate, (+), (-), (^))
import qualified Prelude                                as P

import           ZkFold.Algebra.Class                   hiding (sum)
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import           ZkFold.Algebra.EllipticCurve.BLS12_381 hiding (Fq, Fr)
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.Number
import           ZkFold.Algebra.Polynomial.Univariate
import           ZkFold.Control.Conditional
import           ZkFold.Data.ByteString
import qualified ZkFold.Data.Eq
import           ZkFold.FFI.Rust.Conversion
import           ZkFold.FFI.Rust.Poly
import           ZkFold.FFI.Rust.RustFunctions
import           ZkFold.FFI.Rust.Types
import           ZkFold.Symbolic.MonadCircuit

import ZkFold.FFI.Rust.RustBLS

hTwo :: EC.Fr
hTwo = fromConstant (2 :: Natural)

rTwo :: Fr
rTwo = h2r hTwo

-- rNewTwo :: Fr
-- rNewTwo = unsafePerformIO $ do
--     withForeignPtr (rawData $ rawScalar rTwo) $ \ptr -> do
--         ptrNew <- hsPtrToRsPtr ptr scalarSize
--         RScalar . RData <$> (newForeignPtr finalizerFree ptrNew )

-- rOldTwo :: Fr
-- rOldTwo = unsafePerformIO $ do
--     out <- callocForeignPtrBytes @CChar scalarSize
--     withForeignPtr (rawData $ rawScalar rNewTwo) $ \ptr -> do
--         withForeignPtr out $ \optr -> do
--             rsPtrToHsPtr ptr optr
--     return $ RScalar $ RData $  out



main :: IO ()
main = do
    let res = rmult (fromConstant (2 :: Natural)) (fromConstant (3 :: Natural))
    let res2 = ((fromConstant (2 :: Natural)) :: EC.Fr) +  (fromConstant (3 :: Natural))
    print $ "Result: " <> show res
    print $ "Result: " <> show res2
    -- print $ "Haskell two: " <> show hTwo
    -- print $ "Rust two: " <> show rTwo
    -- print $ "Rust new two: " <> show rNewTwo
    -- print $ "Rust old two: " <> show rOldTwo

