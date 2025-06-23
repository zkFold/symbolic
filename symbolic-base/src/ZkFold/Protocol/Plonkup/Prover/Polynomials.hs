{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Prover.Polynomials where

import Prelude hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Protocol.Plonkup.Internal (PlonkupPolyExtended, PlonkupPolyExtendedLength)

data PlonkupCircuitPolynomials n g pv = PlonkupCircuitPolynomials
  { qlX :: !(PlonkupPolyExtended n g pv)
  , qrX :: !(PlonkupPolyExtended n g pv)
  , qoX :: !(PlonkupPolyExtended n g pv)
  , qmX :: !(PlonkupPolyExtended n g pv)
  , qcX :: !(PlonkupPolyExtended n g pv)
  , qkX :: !(PlonkupPolyExtended n g pv)
  , t1X :: !(PlonkupPolyExtended n g pv)
  , t2X :: !(PlonkupPolyExtended n g pv)
  , t3X :: !(PlonkupPolyExtended n g pv)
  , s1X :: !(PlonkupPolyExtended n g pv)
  , s2X :: !(PlonkupPolyExtended n g pv)
  , s3X :: !(PlonkupPolyExtended n g pv)
  }

instance
  ( Show (ScalarFieldOf g)
  , Show (pv (PlonkupPolyExtendedLength n))
  )
  => Show (PlonkupCircuitPolynomials n g pv)
  where
  show PlonkupCircuitPolynomials {..} =
    "Circuit Polynomials: "
      ++ show qlX
      ++ " "
      ++ show qrX
      ++ " "
      ++ show qoX
      ++ " "
      ++ show qmX
      ++ " "
      ++ show qcX
      ++ " "
      ++ show qkX
      ++ " "
      ++ show t1X
      ++ " "
      ++ show t2X
      ++ " "
      ++ show t3X
      ++ " "
      ++ show s1X
      ++ " "
      ++ show s2X
      ++ " "
      ++ show s3X
