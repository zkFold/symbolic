{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.IVC.Oracle where

import qualified Data.Vector                                    as V
import           GHC.Generics
import           Prelude                                        (map, (.))
import qualified Prelude                                        as P

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Symbolic.Algorithms.Hash.MiMC           (mimcHashN')
import           ZkFold.Symbolic.Algorithms.Hash.MiMC.Constants (mimcConstants)
import           ZkFold.Symbolic.Class                          (Arithmetic)

-- TODO: add more specific instances for efficiency

class HashAlgorithm algo a where
    hash :: [a] -> a

data MiMCHash a
instance forall a. (Ring a, Arithmetic a) => HashAlgorithm (MiMCHash a) a where
    hash :: forall x. (Arithmetic x, FromConstant a x) => [x] -> x
    hash = mimcHashN' @a mimcConstants zero

class RandomOracle algo x a where
    oracle :: x -> a
    default oracle :: (Generic x, RandomOracle' algo (Rep x) a) => x -> a
    oracle = oracle' @algo . from

instance (FromConstant P.Integer a, HashAlgorithm algo a) => RandomOracle algo P.Integer a where
    oracle = oracle @algo @a . fromConstant

instance HashAlgorithm algo a => RandomOracle algo a a where
    oracle x = hash @algo [x]

instance HashAlgorithm algo a => RandomOracle algo (a, a) a where
    oracle (x, y) = hash @algo [x, y]

instance HashAlgorithm algo a => RandomOracle algo [a] a where
    oracle = hash @algo

instance (HashAlgorithm algo b, RandomOracle algo a b) => RandomOracle algo [a] b where
    oracle = hash @algo . map (oracle @algo)

instance (HashAlgorithm algo b, RandomOracle algo a b) => RandomOracle algo (V.Vector a) b where
    oracle = (oracle @algo) . V.toList

instance {-# OVERLAPPABLE #-} (Generic x, RandomOracle' algo (Rep x) a) => RandomOracle algo x a

class RandomOracle' algo f a where
    oracle' :: f x -> a

-- TODO: fix this instance
instance (RandomOracle' algo f b, RandomOracle' algo g b, HashAlgorithm algo b, Ring b) => RandomOracle' algo (f :+: g) b where
    oracle' (L1 x) = hash @algo [zero, oracle' @algo x]
    oracle' (R1 x) = oracle' @algo x

instance (RandomOracle' algo f a, RandomOracle' algo g a, HashAlgorithm algo a) => RandomOracle' algo (f :*: g) a where
    oracle' (x :*: y) =
        let z1 = oracle' @algo x :: a
            z2 = oracle' @algo y :: a
        in hash @algo [z1, z2]

instance RandomOracle algo c a => RandomOracle' algo (Rec0 c) a where
    oracle' (K1 x) = oracle @algo x

-- | Handling constructors with no fields.
instance {-# OVERLAPPING #-}
    Ring a => RandomOracle' algo (M1 C ('MetaCons conName fixity selectors) U1) a where
    oracle' _ = zero

instance RandomOracle' algo f a => RandomOracle' algo (M1 c m f) a where
    oracle' (M1 x) = oracle' @algo x
