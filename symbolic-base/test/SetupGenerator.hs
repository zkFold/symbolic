{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import GHC.TypeNats (type (+), type (^))
import System.Directory (createDirectoryIfMissing)
import Prelude

import ZkFold.Algebra.Class (AdditiveGroup (..), FromConstant (..), MultiplicativeGroup (..), Scale (..))
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_JacobianPoint, BLS12_381_G2_JacobianPoint)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..), Point (..), ScalarFieldOf)
import ZkFold.Algebra.Number (KnownNat, value)
import ZkFold.Data.Vector (Vector)
import qualified ZkFold.Data.Vector as V
import ZkFold.Protocol.NonInteractiveProof.TrustedSetup (TrustedSetup (..), saveTrustedSetup)

-- We need 2^20 + 6 points.
type N = 2 ^ 20 + 6

main :: IO ()
main = do
  putStrLn "Generating trusted setup for 2^20 + 6 points..."
  let g1 = pointGen :: BLS12_381_G1_JacobianPoint
      g2 = pointGen :: BLS12_381_G2_JacobianPoint
      -- We use a simple "toxic waste" secret for testing purposes.
      -- IN PRODUCTION THIS MUST BE A REAL MPC CEREMONY.
      secret = 123456789 :: Integer
      secretScalar = fromConstant secret :: ScalarFieldOf BLS12_381_G1_JacobianPoint

  -- Generate powers of tau for G1: g1, g1^s, g1^(s^2), ...
  -- Note: scaling in additive notation is multiplication by scalar.
  -- g1 * s^0, g1 * s^1, ...

  let powers = take (fromIntegral (value @N)) $ iterate (* secretScalar) (fromConstant (1 :: Integer))
      g1s_list = map (\p -> scale p g1) powers

  let g1s_vec = V.unsafeToVector @N g1s_list

  let g2_0 = g2
      -- G2 has the same scalar field as G1 (BLS12-381 is a pairing curve)
      -- But strictly speaking ScalarFieldOf G2 might be distinct type alias?
      -- They should be the same Fr.
      g2_1 = scale (fromConstant secret :: ScalarFieldOf BLS12_381_G2_JacobianPoint) g2

  let ts =
        TrustedSetup
          { g1s = g1s_vec
          , g2_0 = g2_0
          , g2_1 = g2_1
          }

  let filePath = "data/powers_of_tau_2e20"
  putStrLn $ "Saving to " ++ filePath
  saveTrustedSetup filePath True ts
  putStrLn "Done."
