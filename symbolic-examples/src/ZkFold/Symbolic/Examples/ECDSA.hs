{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.ECDSA (exampleECDSA) where

import GHC.Generics ((:*:))
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Algorithm.ECDSA.ECDSA (ecdsaVerifyMessageHash)
import ZkFold.Symbolic.Class (BaseField, Symbolic)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Combinators (GetRegisterSize, NumberOfRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)

exampleECDSA
  :: Symbolic ctx
  => KnownNat (NumberOfRegisters (BaseField ctx) 256 'Auto)
  => KnownNat (GetRegisterSize (BaseField ctx) 256 'Auto)
  => KnownFFA FpModulus 'Auto ctx
  => KnownFFA FqModulus 'Auto ctx
  => Pallas_Point ctx
  -> FFA FqModulus 'Auto ctx
  -> (FFA FqModulus 'Auto :*: FFA FqModulus 'Auto) ctx
  -> Bool ctx
exampleECDSA = ecdsaVerifyMessageHash @256
