{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.Commit (
  Commit,
  oracleCommitment,
  HomomorphicCommit (..),
  PedersonSetup (..),
  PedersonCommit (..),
) where

import Data.Zip (Zip (..))
import System.Random (Uniform, mkStdGen, uniform)
import Prelude hiding (Num (..), sum, take, zipWith)

import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Data.Vector (Vector, unsafeToVector)
import ZkFold.Prelude (take)
import ZkFold.Protocol.IVC.Oracle

-- | Commit to the object @a@ with results of type @f@
type Commit a f = a -> f

oracleCommitment :: (OracleSource a b, Ring a) => Hasher -> Commit b a
oracleCommitment = oracle

-- | Homomorphic commitment scheme, i.e. (hcommit x) * (hcommit y) == hcommit (x + y)
class AdditiveGroup c => HomomorphicCommit a c where
  hcommit :: a -> c

class PedersonSetup s c where
  groupElements :: s c

type PedersonSetupMaxSize = 100

instance
  ( CyclicGroup (Weierstrass curve (Point field))
  , Uniform (ScalarFieldOf (Weierstrass curve (Point field)))
  )
  => PedersonSetup [] (Weierstrass curve (Point field))
  where
  groupElements =
    -- TODO: This is just for testing purposes! Not to be used in production
    let x = fst $ uniform $ mkStdGen 0 :: ScalarFieldOf (Weierstrass curve (Point field))
     in take (value @PedersonSetupMaxSize) $ iterate (scale x) pointGen

instance
  ( CyclicGroup (Weierstrass curve (Point field))
  , KnownNat n
  , Uniform (ScalarFieldOf (Weierstrass curve (Point field)))
  , n <= PedersonSetupMaxSize
  )
  => PedersonSetup (Vector n) (Weierstrass curve (Point field))
  where
  groupElements =
    -- TODO: This is just for testing purposes! Not to be used in production
    unsafeToVector $ take (value @n) $ groupElements @[]

newtype PedersonCommit g = PedersonCommit {pedersonCommit :: g}
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, Scale f)

instance (Functor s, PedersonSetup s g) => PedersonSetup s (PedersonCommit g) where
  groupElements = PedersonCommit <$> groupElements

instance
  ( AdditiveGroup g
  , Foldable s
  , PedersonSetup s g
  , Scale f g
  , Zip s
  )
  => HomomorphicCommit (s f) (PedersonCommit g)
  where
  hcommit v = sum $ zipWith scale v groupElements

deriving via
  (PedersonCommit (Weierstrass c (Point f)))
  instance
    ( CyclicGroup (Weierstrass c (Point f))
    , Scale s (Weierstrass c (Point f))
    , Uniform (ScalarFieldOf (Weierstrass c (Point f)))
    )
    => HomomorphicCommit [s] (Weierstrass c (Point f))
