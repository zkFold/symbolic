module ZkFold.Protocol.Plonkup.Testing where

import           Prelude                            hiding (Num (..), drop, length, pi, sum, take, (!!), (/), (^))

import           ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (ScalarFieldOf))
import           ZkFold.Protocol.Plonkup.Internal   (PlonkupPolyExtended)

data PlonkupProverTestInfo n g1 pv = PlonkupProverTestInfo
    { omega         :: ScalarFieldOf g1
    , k1            :: ScalarFieldOf g1
    , k2            :: ScalarFieldOf g1
    , qlX           :: PlonkupPolyExtended n g1 pv
    , qrX           :: PlonkupPolyExtended n g1 pv
    , qoX           :: PlonkupPolyExtended n g1 pv
    , qmX           :: PlonkupPolyExtended n g1 pv
    , qcX           :: PlonkupPolyExtended n g1 pv
    , qkX           :: PlonkupPolyExtended n g1 pv
    , t1X           :: PlonkupPolyExtended n g1 pv
    , t2X           :: PlonkupPolyExtended n g1 pv
    , t3X           :: PlonkupPolyExtended n g1 pv
    , s1X           :: PlonkupPolyExtended n g1 pv
    , s2X           :: PlonkupPolyExtended n g1 pv
    , s3X           :: PlonkupPolyExtended n g1 pv
    , aX            :: PlonkupPolyExtended n g1 pv
    , bX            :: PlonkupPolyExtended n g1 pv
    , cX            :: PlonkupPolyExtended n g1 pv
    , piX           :: PlonkupPolyExtended n g1 pv
    , tX            :: PlonkupPolyExtended n g1 pv
    , z1X           :: PlonkupPolyExtended n g1 pv
    , z2X           :: PlonkupPolyExtended n g1 pv
    , fX            :: PlonkupPolyExtended n g1 pv
    , h1X           :: PlonkupPolyExtended n g1 pv
    , h2X           :: PlonkupPolyExtended n g1 pv
    , zhX           :: PlonkupPolyExtended n g1 pv
    , qX            :: PlonkupPolyExtended n g1 pv
    , qlowX         :: PlonkupPolyExtended n g1 pv
    , qmidX         :: PlonkupPolyExtended n g1 pv
    , qhighX        :: PlonkupPolyExtended n g1 pv
    , rX            :: PlonkupPolyExtended n g1 pv
    , alpha         :: ScalarFieldOf g1
    , beta          :: ScalarFieldOf g1
    , gamma         :: ScalarFieldOf g1
    , delta         :: ScalarFieldOf g1
    , epsilon       :: ScalarFieldOf g1
    , xi            :: ScalarFieldOf g1
    , zeta          :: ScalarFieldOf g1
    , omegas        :: pv n
    , omegas'       :: PlonkupPolyExtended n g1 pv
    , grandProduct1 :: pv n
    , grandProduct2 :: pv n
    , w1            :: pv n
    , w2            :: pv n
    , w3            :: pv n
    }
