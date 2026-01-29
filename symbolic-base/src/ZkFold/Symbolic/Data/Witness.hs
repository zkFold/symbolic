{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Witness where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Function (const, id, (.))
import Data.Functor (Functor, fmap, (<$>))
import Data.Functor.Identity (Identity (..))
import Data.Semialign (Semialign)
import Data.Type.Ord (type (<=))
import GHC.Generics (Par1 (..))
import Text.Show (Show)

import ZkFold.Algebra.Class
import qualified ZkFold.Algorithm.Interpolation as I
import ZkFold.Control.Conditional (Conditional)
import ZkFold.Data.Eq (Eq)
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..))

newtype Witness a = Witness {witness :: a}
  deriving
    ( AdditiveGroup
    , AdditiveSemigroup
    , Eq
    , Field
    , Functor
    , MultiplicativeMonoid
    , MultiplicativeSemigroup
    , NFData
    , PrimeField
    , Show
    , Zero
    )

deriving instance Finite a => Finite (Witness a)

instance Exponent a e => Exponent (Witness a) e where
  Witness x ^ e = Witness (x ^ e)

deriving instance
  {-# INCOHERENT #-}
  Conditional b a => Conditional b (Witness a)

deriving instance Scale k a => Scale k (Witness a)

deriving instance
  {-# INCOHERENT #-}
  FromConstant c a => FromConstant c (Witness a)

deriving instance AdditiveMonoid a => AdditiveMonoid (Witness a)

instance {-# OVERLAPPING #-} MultiplicativeSemigroup a => Scale (Witness a) (Witness a)

deriving instance Semiring a => Semiring (Witness a)

deriving instance Ring a => Ring (Witness a)

deriving via Identity instance Semialign Witness

instance (PrimeField a, 3 <= Order a) => Symbolic (Witness a) where
  constrain = const id

instance SymbolicData Witness where
  type Layout Witness _ = Par1
  type HasRep Witness _ = ()
  toLayout = Par1 . witness
  fromLayout = Witness . unPar1
  interpolate c xs = I.interpolate (Witness c) (first fromConstant <$> xs)

toWitness :: (Symbolic c, SymbolicData d) => d c -> d (Witness c)
toWitness = fromLayout . fmap Witness . toLayout
