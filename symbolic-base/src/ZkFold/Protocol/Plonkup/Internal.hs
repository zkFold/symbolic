{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Internal where

import Data.Constraint (withDict)
import Data.Constraint.Nat (plusNat, timesNat)
import Data.Functor.Classes (Show1)
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary (..))
import Prelude hiding (
  Num (..),
  drop,
  length,
  sum,
  take,
  (!!),
  (/),
  (^),
 )

import ZkFold.Algebra.Class (Bilinear (..), Scale (..))
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate (UnivariateFieldPolyVec (..))
import ZkFold.Data.Vector (Vector)
import ZkFold.FFI.Rust.Conversion
import ZkFold.Protocol.Plonkup.Utils (getParams, getSecretParams)
import ZkFold.Symbolic.Class (Arithmetic)
import ZkFold.Symbolic.Compiler.ArithmeticCircuit (ArithmeticCircuit (acContext))
import ZkFold.Symbolic.Compiler.ArithmeticCircuit.Context (acOutput)

{-
    NOTE: we need to parametrize the type of transcripts because we use BuiltinByteString on-chain and ByteString off-chain.
    Additionally, we don't want this library to depend on Cardano libraries.
-}

data Plonkup i o (n :: Natural) g1 g2 transcript pv = Plonkup
  { omega :: ScalarFieldOf g1
  , k1 :: ScalarFieldOf g1
  , k2 :: ScalarFieldOf g1
  , ac :: ArithmeticCircuit (ScalarFieldOf g1) i o
  , h1 :: g2
  , gs' :: Vector (n + 6) g1
  }

type PlonkupPermutationSize n = 3 * n

-- The maximum degree of the polynomials we need in the protocol is `4 * n + 5`.
type PlonkupPolyExtendedLength n = 4 * n + 6

with4n6 :: forall n {r}. KnownNat n => (KnownNat (4 * n + 6) => r) -> r
with4n6 f = withDict (timesNat @4 @n) (withDict (plusNat @(4 * n) @6) f)

type PlonkupPolyExtended n g pv = pv (PlonkupPolyExtendedLength n)

instance (Show (ScalarFieldOf g1), Show g1, Show g2, Show1 o) => Show (Plonkup i o n g1 g2 t pv) where
  show Plonkup {..} =
    "Plonkup: "
      ++ show omega
      ++ " "
      ++ show k1
      ++ " "
      ++ show k2
      ++ " "
      ++ show (acOutput $ acContext ac)
      ++ " "
      ++ show ac
      ++ " "
      ++ show h1
      ++ " "
      ++ show gs'

instance
  ( Arbitrary (ArithmeticCircuit (ScalarFieldOf g1) i o)
  , Arbitrary (ScalarFieldOf g1)
  , Arbitrary g1
  , Arbitrary g2
  , Arithmetic (ScalarFieldOf g1)
  , CyclicGroup g1
  , CyclicGroup g2
  , KnownNat (n + 6)
  , KnownNat n
  , Scale (ScalarFieldOf g1) g2
  )
  => Arbitrary (Plonkup i o n g1 g2 t pv)
  where
  arbitrary = do
    ac <- arbitrary
    x <- arbitrary
    let (omega, k1, k2) = getParams (value @n)
    let (gs, h1) = getSecretParams x
    return $ Plonkup omega k1 k2 ac h1 gs

lagrangeBasisGroupElements
  :: forall n g1 pv rustG1
   . ( Bilinear (V.Vector rustG1) (pv (PlonkupPolyExtendedLength n)) g1
     , KnownNat (PlonkupPolyExtendedLength n)
     , KnownNat n
     , RustHaskell rustG1 g1
     , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
     )
  => ScalarFieldOf g1 -> V.Vector g1 -> [g1]
lagrangeBasisGroupElements omega gs =
  map (\i -> (h2r <$> gs) `bilinear` polyVecLagrange @_ @pv @(PlonkupPolyExtendedLength n) (value @n) i omega) [1 ..]
