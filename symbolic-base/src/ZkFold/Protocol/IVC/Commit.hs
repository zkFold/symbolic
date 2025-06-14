{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.Commit
    ( Commit
    , oracleCommitment
    , HomomorphicCommit (..)
    , PedersonSetup (..)
    , PedersonCommit (..)
    ) where

import           Data.Functor.Constant              (Constant (..))
import           Data.Zip                           (Zip (..))
import           Prelude                            hiding (Num (..), sum, take, zipWith)
import           System.Random                      (Random (..), mkStdGen)

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Algebra.Number
import           ZkFold.Data.Vector                 (Vector, unsafeToVector)
import           ZkFold.Prelude                     (take)
import           ZkFold.Protocol.IVC.Oracle

-- | Commit to the object @a@ with results of type @f@
--
type Commit a f = a -> f

oracleCommitment :: (OracleSource a b, Ring a) => Hasher -> Commit b a
oracleCommitment = oracle

-- | Homomorphic commitment scheme, i.e. (hcommit x) * (hcommit y) == hcommit (x + y)
--
class AdditiveGroup c => HomomorphicCommit a c where
    hcommit :: a -> c

class PedersonSetup s c where
    groupElements :: s c

type PedersonSetupMaxSize = 100

instance
  ( CyclicGroup (Weierstrass curve (Point field))
  , Random (ScalarFieldOf (Weierstrass curve (Point field)))
  ) => PedersonSetup [] (Weierstrass curve (Point field)) where
    groupElements =
        -- TODO: This is just for testing purposes! Not to be used in production
        let x = fst $ random $ mkStdGen 0 :: ScalarFieldOf (Weierstrass curve (Point field))
        in take (value @PedersonSetupMaxSize) $ iterate (scale x) pointGen

instance
  ( KnownNat n
  , CyclicGroup (Weierstrass curve (Point field))
  , Random (ScalarFieldOf (Weierstrass curve (Point field)))
  , n <= PedersonSetupMaxSize
  ) => PedersonSetup (Vector n) (Weierstrass curve (Point field)) where
    groupElements =
        -- TODO: This is just for testing purposes! Not to be used in production
        unsafeToVector $ take (value @n) $ groupElements @[]

instance (PedersonSetup s g, Functor s) => PedersonSetup s (Constant g a) where
    groupElements = Constant <$> groupElements @s

newtype PedersonCommit g = PedersonCommit { pedersonCommit :: g }
    deriving newtype (Scale f, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup)

instance
    (Functor s, PedersonSetup s g) => PedersonSetup s (PedersonCommit g) where
    groupElements = PedersonCommit <$> groupElements

instance
  ( PedersonSetup s g
  , Zip s
  , Foldable s
  , Scale f g
  , AdditiveGroup g
  ) => HomomorphicCommit (s f) (PedersonCommit g) where
    hcommit v = sum $ zipWith scale v groupElements
