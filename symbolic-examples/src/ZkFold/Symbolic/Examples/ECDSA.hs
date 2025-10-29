{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Examples.ECDSA (exampleECDSA) where

import GHC.Generics ((:*:))
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Algorithm.ECDSA.ECDSA (ecdsaVerifyMessageHash)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Combinators (GetRegisterSize, NumberOfRegisters, RegisterSize (..))
import ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import ZkFold.Symbolic.Data.FFA (FFA, KnownFFA)
import ZkFold.Symbolic.V2 (Symbolic)

exampleECDSA
  :: Symbolic ctx
  => KnownNat (NumberOfRegisters ctx 256 'Auto)
  => KnownNat (GetRegisterSize ctx 256 'Auto)
  => KnownFFA FpModulus 'Auto ctx
  => KnownFFA FqModulus 'Auto ctx
  => Pallas_Point ctx
  -> FFA FqModulus 'Auto ctx
  -> (FFA FqModulus 'Auto :*: FFA FqModulus 'Auto) ctx
  -> CompatData Bool ctx
exampleECDSA = ecdsaVerifyMessageHash @256
