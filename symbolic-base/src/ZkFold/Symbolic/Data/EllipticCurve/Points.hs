{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ZkFold.Symbolic.Data.EllipticCurve.Points where

import GHC.Generics
import Prelude (type (~))

import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Data.Eq
import ZkFold.Symbolic.Data.Bool
import ZkFold.Symbolic.Data.Class


-- Defining instances for 'SymbolicData':

newtype SymbolicPoint point = SymbolicPoint point
  deriving Generic

instance
  ( SymbolicData (BooleanOf field)
  , SymbolicData field
  , Context field ~ Context (BooleanOf field)
  )
  => SymbolicData (SymbolicPoint (Point field))

instance
  ( SymbolicData field
  , Context field ~ Context (BooleanOf field)
  )
  => SymbolicData (SymbolicPoint (JacobianPoint field))

instance SymbolicData field => SymbolicData (SymbolicPoint (AffinePoint field))

deriving newtype instance
  SymbolicEq field
  => SymbolicData (Weierstrass curve (SymbolicPoint (Point field)))

deriving newtype instance
  SymbolicEq field
  => SymbolicData (Weierstrass curve (SymbolicPoint (JacobianPoint field)))

deriving newtype instance
  SymbolicData field
  => SymbolicData (TwistedEdwards curve (SymbolicPoint (AffinePoint field)))


{-

-- Next we should define instances for 'SymbolicInput'.  After some experimentation, these
-- appear to be more challenging.

instance
  ( WeierstrassCurve curve field
  , SymbolicEq field
  )
  => SymbolicInput (Weierstrass curve (SymbolicPoint (Point field)))
  where
  isValid = isOnCurve

instance
  ( WeierstrassCurve curve field
  , SymbolicEq field
  )
  => SymbolicInput (Weierstrass curve (SymbolicPoint (JacobianPoint field)))
  where
  isValid = isOnCurve

-}
