{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.Plonkup.Internal where

import           Data.Constraint                                     (withDict)
import           Data.Constraint.Nat                                 (plusNat, timesNat)
import           Data.Functor.Classes                                (Show1)
import           Data.Functor.Rep                                    (Rep)
import           Prelude                                             hiding (Num (..), drop, length, sum, take, (!!),
                                                                      (/), (^))
import           Test.QuickCheck                                     (Arbitrary (..))

import           ZkFold.Algebra.Class                                (Scale, Bilinear (..))
import           ZkFold.Algebra.EllipticCurve.Class                  (CyclicGroup (..))
import           ZkFold.Algebra.Number
import           ZkFold.Data.Vector                                  (Vector)
import           ZkFold.Protocol.Plonkup.Utils                       (getParams, getSecrectParams)
import           ZkFold.Symbolic.Compiler                            ()
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit.Internal (Arithmetic, ArithmeticCircuit (..))
import ZkFold.Algebra.Polynomial.Univariate (UnivariateFieldPolyVec (..))
import qualified Data.Vector as V

{-
    NOTE: we need to parametrize the type of transcripts because we use BuiltinByteString on-chain and ByteString off-chain.
    Additionally, we don't want this library to depend on Cardano libraries.
-}

data Plonkup i o (n :: Natural) g1 g2 transcript pv = Plonkup {
        omega :: ScalarFieldOf g1,
        k1    :: ScalarFieldOf g1,
        k2    :: ScalarFieldOf g1,
        ac    :: ArithmeticCircuit (ScalarFieldOf g1) i o,
        h1    :: g2,
        gs'   :: Vector (n + 5) g1
    }

type PlonkupPermutationSize n = 3 * n

-- The maximum degree of the polynomials we need in the protocol is `4 * n + 5`.
type PlonkupPolyExtendedLength n = 4 * n + 6

with4n6 :: forall n {r}. KnownNat n => (KnownNat (4 * n + 6) => r) -> r
with4n6 f = withDict (timesNat @4 @n) (withDict (plusNat @(4 * n) @6) f)

type PlonkupPolyExtended n g pv = pv (PlonkupPolyExtendedLength n)

instance (Show (ScalarFieldOf g1), Show (Rep i), Show1 o, Ord (Rep i), Show g1, Show g2) => Show (Plonkup i o n g1 g2 t pv) where
    show Plonkup {..} =
        "Plonkup: " ++ show omega ++ " " ++ show k1 ++ " " ++ show k2 ++ " " ++ show (acOutput ac)  ++ " " ++ show ac ++ " " ++ show h1 ++ " " ++ show gs'

instance
  ( KnownNat n
  , KnownNat (n + 5)
  , Arbitrary g1
  , Arbitrary g2
  , Arithmetic (ScalarFieldOf g1)
  , Arbitrary (ScalarFieldOf g1)
  , CyclicGroup g1
  , CyclicGroup g2
  , Scale (ScalarFieldOf g1) g2
  , Arbitrary (ArithmeticCircuit (ScalarFieldOf g1) i o)
  ) => Arbitrary (Plonkup i o n g1 g2 t pv) where
    arbitrary = do
        ac <- arbitrary
        x <- arbitrary
        let (omega, k1, k2) = getParams (value @n)
        let (gs, h1) = getSecrectParams x
        return $ Plonkup omega k1 k2 ac h1 gs

lagrangeBasisGroupElements :: forall n g1 pv .
    ( KnownNat n
    , KnownNat (PlonkupPolyExtendedLength n)
    , UnivariateFieldPolyVec (ScalarFieldOf g1) pv
    , Bilinear (V.Vector g1) (pv (PlonkupPolyExtendedLength n)) g1
    ) => ScalarFieldOf g1 -> V.Vector g1 -> [g1]
lagrangeBasisGroupElements omega gs =
    map (\i -> gs `bilinear` polyVecLagrange @_ @pv @(PlonkupPolyExtendedLength n) (value @n) i omega) [1 .. ]
