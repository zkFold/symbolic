{-# LANGUAGE UndecidableInstances #-}

module ZkFold.UPLC.Data where

import Control.Applicative ((<*>))
import Data.ByteString (ByteString)
import Data.Functor ((<$>))
import Data.Word (Word64)
import Test.QuickCheck (Arbitrary (..), oneof)
import Text.Show (Show)
import Prelude (Integer)

-- | Constructor tags used on Cardano.
--
-- While theoretically unbounded, in practice it should fit in 64 bits as said in [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
type ConstructorTag = Word64

-- | Plutus Core's Data builtin type as a regular Haskell datatype.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
data Data
  = DConstr ConstructorTag [Data]
  | DMap [(Data, Data)]
  | DList [Data]
  | DI Integer
  | DB ByteString
  deriving Show

instance Arbitrary ByteString => Arbitrary Data where
  arbitrary =
    oneof
      [ DConstr <$> arbitrary <*> arbitrary
      , DMap <$> arbitrary
      , DList <$> arbitrary
      , DI <$> arbitrary
      , DB <$> arbitrary
      ]
