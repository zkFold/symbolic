{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.Mithril
  (exampleMithril) where

import           ZkFold.Algebra.Number                   (KnownNat)
import           ZkFold.Algebra.EllipticCurve.Class      (CyclicGroup (ScalarFieldOf))
import           ZkFold.Algebra.EllipticCurve.Pasta      (FpModulus, FqModulus)
import           ZkFold.Symbolic.Algorithm.Mithril       (StakeDistribution, mithril)
import           ZkFold.Symbolic.Class                    (BaseField, Symbolic)
import           ZkFold.Symbolic.Data.Combinators         (GetRegisterSize, NumberOfRegisters, RegisterSize (..))
import           ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import           ZkFold.Symbolic.Data.FFA                 (KnownFFA)
import           ZkFold.Symbolic.Data.FieldElement        (FieldElement)

exampleMithril
  :: forall n m ctx .
     ( Symbolic ctx
     , KnownFFA FpModulus 'Auto ctx
     , KnownFFA FqModulus 'Auto ctx
     , KnownNat n
     , KnownNat (NumberOfRegisters (BaseField ctx) n 'Auto)
     , KnownNat (GetRegisterSize (BaseField ctx) n 'Auto)
     )
  => StakeDistribution m (Pallas_Point ctx) ctx
  -> ScalarFieldOf (Pallas_Point ctx)
  -> (ScalarFieldOf (Pallas_Point ctx), ScalarFieldOf (Pallas_Point ctx))
  -> FieldElement ctx
exampleMithril = mithril @m @n
