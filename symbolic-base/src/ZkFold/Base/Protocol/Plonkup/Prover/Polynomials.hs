{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.Plonkup.Prover.Polynomials where

import           Control.DeepSeq                         (NFData)
import           GHC.Generics                            (Generic)
import           Prelude                                 hiding (Num (..), drop, length, sum, take, (!!), (/), (^))

import           ZkFold.Base.Algebra.EllipticCurve.Class (CyclicGroup (..))
import           ZkFold.Base.Protocol.Plonkup.Internal   (PlonkupPolyExtended)

data PlonkupCircuitPolynomials n g = PlonkupCircuitPolynomials {
        qlX :: PlonkupPolyExtended n g,
        qrX :: PlonkupPolyExtended n g,
        qoX :: PlonkupPolyExtended n g,
        qmX :: PlonkupPolyExtended n g,
        qcX :: PlonkupPolyExtended n g,
        qkX :: PlonkupPolyExtended n g,
        s1X :: PlonkupPolyExtended n g,
        s2X :: PlonkupPolyExtended n g,
        s3X :: PlonkupPolyExtended n g,
        tX  :: PlonkupPolyExtended n g
    }
    deriving Generic

deriving instance NFData (ScalarFieldOf g) => NFData (PlonkupCircuitPolynomials n g)

instance Show (ScalarFieldOf g) => Show (PlonkupCircuitPolynomials n g) where
    show PlonkupCircuitPolynomials {..} =
        "Circuit Polynomials: "
        ++ show qlX ++ " "
        ++ show qrX ++ " "
        ++ show qoX ++ " "
        ++ show qmX ++ " "
        ++ show qcX ++ " "
        ++ show qkX ++ " "
        ++ show s1X ++ " "
        ++ show s2X ++ " "
        ++ show s3X
