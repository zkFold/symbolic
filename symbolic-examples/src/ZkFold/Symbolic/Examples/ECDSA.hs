{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.ECDSA (exampleECDSA) where

import GHC.Generics ((:*:))
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Symbolic.Algorithm.ECDSA.ECDSA (ecdsaVerifyMessageHash)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.Data.UInt (KnownUInt)

exampleECDSA
  :: Symbolic ctx
  => KnownFFA FpModulus ctx
  => KnownFFA FqModulus ctx
  => KnownUInt 256 ctx
  => Pallas_Point ctx
  -> FFA FqModulus ctx
  -> (FFA FqModulus :*: FFA FqModulus) ctx
  -> Bool ctx
exampleECDSA = ecdsaVerifyMessageHash @256
