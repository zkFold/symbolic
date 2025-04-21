{-# LANGUAGE TypeOperators #-}

module Examples.Ed25519 (
    exampleEd25519Scale
  ) where

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.EllipticCurve.Ed25519       (Ed25519_Base, Ed25519_Scalar)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Combinators           (RegisterSize (Auto))
import           ZkFold.Symbolic.Data.EllipticCurve.Ed25519
import           ZkFold.Symbolic.Data.FFA

exampleEd25519Scale
    :: ( Symbolic ctx
       , KnownFFA Ed25519_Base 'Auto ctx
       , KnownFFA Ed25519_Scalar 'Auto ctx
       )
    => ScalarFieldOf (Ed25519_Point ctx)
    -> Ed25519_Point ctx
    -> Ed25519_Point ctx
exampleEd25519Scale = scale
