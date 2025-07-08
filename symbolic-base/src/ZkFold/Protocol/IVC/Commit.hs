{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.IVC.Commit (
  Commit,
  oracleCommitment,
  HomomorphicCommit (..),
  PedersonSetup (..),
) where

import Crypto.Hash.SHA256 (hash)
import Data.Maybe (fromJust)
import Data.Zip (Zip (..))
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Data.ByteString (LittleEndian (..), fromByteString)
import ZkFold.Protocol.IVC.Oracle
import Prelude hiding (Num (..), sum, take, zipWith, (^))

-- | Commit to the object @a@ with results of type @f@
type Commit a f = a -> f

oracleCommitment :: (OracleSource a b, Ring a) => Hasher -> Commit b a
oracleCommitment = oracle

class PedersonSetup g where
  groupElements :: [g]

instance
  CyclicGroup g
  => PedersonSetup g
  where
  groupElements =
    -- TODO: This is just for testing purposes! Not to be used in production
    let x = fromConstant @Natural $ unLittleEndian $ fromJust $ fromByteString $ hash "42" :: ScalarFieldOf g
     in iterate (scale x) pointGen

-- | Homomorphic commitment scheme, i.e. (hcommit x) * (hcommit y) == hcommit (x + y)
class CyclicGroup g => HomomorphicCommit g where
  hcommit :: [ScalarFieldOf g] -> g

instance
  CyclicGroup g
  => HomomorphicCommit g
  where
  hcommit v = sum $ zipWith (scale @(ScalarFieldOf g)) v groupElements
