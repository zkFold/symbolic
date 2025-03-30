module Examples.ECDSA
  (exampleECDSA) where

import           ZkFold.Base.Algebra.Basic.Number         (KnownNat)
import           ZkFold.Base.Algebra.EllipticCurve.Pasta  (FpModulus, FqModulus)
import           ZkFold.Symbolic.Algorithms.ECDSA.ECDSA   (ecdsaVerify)
import           ZkFold.Symbolic.Class                    (Symbolic, BaseField)
import           ZkFold.Symbolic.Data.Bool                (Bool)
import           ZkFold.Symbolic.Data.Combinators         (RegisterSize (..), NumberOfRegisters, GetRegisterSize)
import           ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import           ZkFold.Symbolic.Data.FFA                 (FFA, KnownFFA)

exampleECDSA :: Symbolic ctx
  => KnownNat (NumberOfRegisters (BaseField ctx) 256 'Auto)
  => KnownNat (GetRegisterSize (BaseField ctx) 256 'Auto)
  => KnownFFA FpModulus 'Auto ctx
  => KnownFFA FqModulus 'Auto ctx
  => Pallas_Point ctx
  -> FFA FqModulus 'Auto ctx
  -> (FFA FqModulus 'Auto ctx, FFA FqModulus 'Auto ctx)
  -> Bool ctx
exampleECDSA = ecdsaVerify @256