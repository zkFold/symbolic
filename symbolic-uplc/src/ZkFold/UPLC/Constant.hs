{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module ZkFold.UPLC.Constant where

import Control.Applicative ((<*>))
import Control.Monad (fail)
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Functor ((<$>))
import Data.Text (Text)
import Flat qualified
import Flat.Decoder qualified as Flat
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_Point, BLS12_381_G2_Point)
import Prelude (Integer)

import ZkFold.UPLC.BuiltinType (BuiltinType (..), DemotedType (..))
import ZkFold.UPLC.Data (Data, getData)

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

-- | A decoder of a 'Constant', according to
-- [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Jul 2025).
getConstant :: DemotedType t -> Flat.Get (Constant t)
getConstant = \case
  DInteger -> CInteger <$> Flat.decode
  DByteString -> CByteString <$> Flat.decode
  DString -> CString <$> Flat.decode
  DBool -> CBool <$> Flat.decode
  DUnit -> CUnit <$> Flat.decode
  DData -> CData <$> getData
  DemList t -> CList <$> Flat.decodeListWith (getConstant t)
  DPair t u -> CPair <$> getConstant t <*> getConstant u
  DG1 -> fail "BLS deserialization not supported"
  DG2 -> fail "BLS deserialization not supported"
  DGR -> fail "BLS deserialization not supported"
