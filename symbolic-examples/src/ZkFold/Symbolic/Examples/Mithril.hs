{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Examples.Mithril (exampleMithril) where

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (ScalarFieldOf))
import ZkFold.Algebra.EllipticCurve.Pasta (FpModulus, FqModulus)
import ZkFold.Algebra.Number (KnownNat)
import ZkFold.Symbolic.Algorithm.Mithril (StakeDistribution, mithril)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.EllipticCurve.Pasta (Pallas_Point)
import ZkFold.Symbolic.Data.FFA (KnownFFA)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.UInt

exampleMithril
  :: forall n m ctx
   . ( Symbolic ctx
     , KnownFFA FpModulus 'Auto ctx
     , KnownFFA FqModulus 'Auto ctx
     , KnownNat n
     , KnownNat (NumberOfRegisters ctx n 'Auto)
     , KnownNat (GetRegisterSize ctx n 'Auto)
     )
  => StakeDistribution m (Pallas_Point ctx) ctx
  -> ScalarFieldOf (Pallas_Point ctx)
  -> (ScalarFieldOf (Pallas_Point ctx), ScalarFieldOf (Pallas_Point ctx))
  -> FieldElement ctx
exampleMithril = mithril @m @n
