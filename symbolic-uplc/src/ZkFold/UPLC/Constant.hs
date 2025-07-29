{-# LANGUAGE DataKinds #-}

module ZkFold.UPLC.Constant where

import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Text (Text)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import Prelude (Integer)

import ZkFold.UPLC.BuiltinType (BuiltinType (..))
import ZkFold.UPLC.Data (Data)

-- | Constants available in Plutus Core.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
--
-- Note that all constants are annotated with a corresponding 'BuiltinType'
-- to avoid implementation errors.
data Constant (t :: BuiltinType) where
  CInteger :: Integer -> Constant BTInteger
  CByteString :: ByteString -> Constant BTByteString
  CString :: Text -> Constant BTString
  CBool :: Bool -> Constant BTBool
  CUnit :: () -> Constant BTUnit
  CData :: Data -> Constant BTData
  CList :: [Constant t] -> Constant (BTList t)
  CPair :: Constant t -> Constant u -> Constant (BTPair t u)
  CG1 :: BLS12_381_G1_Point -> Constant BTBLSG1
  CG2 :: BLS12_381_G2_Point -> Constant BTBLSG2
