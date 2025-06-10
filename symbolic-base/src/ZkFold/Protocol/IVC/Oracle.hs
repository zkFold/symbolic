{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.Oracle where

import           Data.Foldable                                 (Foldable, foldMap)
import           Data.Function                                 ((.))
import           Data.List                                     ((++))
import           GHC.Generics

import           ZkFold.Algebra.Class
import           ZkFold.Data.Vector                            (Vector)
import           ZkFold.Symbolic.Algorithm.Hash.MiMC           (mimcHashN')
import           ZkFold.Symbolic.Algorithm.Hash.MiMC.Constants (mimcConstants)
import           ZkFold.Symbolic.Class                         (Arithmetic)

----------------------------- OracleSource class -------------------------------

-- TODO: add more specific instances for efficiency

-- | @OracleSource a b@ links together base field @a@ and source of randomness @b@ to use in 'oracle'.
--
-- 'Generic' deriving via @b@ is available.
class OracleSource a b where
    source :: b -> [a]
    -- ^ Extracts random seed from the source.
    default source :: (Generic b, GOracleSource a (Rep b)) => b -> [a]
    source = gsource . from

instance OracleSource a a where
    source = (:[])

instance (OracleSource a b, OracleSource a c) =>
    OracleSource a (b, c)

instance (OracleSource a b, OracleSource a c, OracleSource a d) =>
    OracleSource a (b, c, d)

instance ( OracleSource a b, OracleSource a c
         , OracleSource a d, OracleSource a e) => OracleSource a (b, c, d, e)

instance ( OracleSource a b, OracleSource a c, OracleSource a d
         , OracleSource a e, OracleSource a f) => OracleSource a (b, c, d, e, f)

-- | A newtype to derive 'OracleSource' for any 'Foldable'
-- as long as its element type is also 'OracleSource'.
newtype FoldableSource f a = FoldableSource { foldableSource :: f a }
    deriving (Foldable)

instance (Foldable f, OracleSource a b) =>
         OracleSource a (FoldableSource f b) where
    source = foldMap source

deriving via (FoldableSource [] b)
    instance OracleSource a b => OracleSource a [b]

deriving via (FoldableSource (Vector n) b)
    instance OracleSource a b => OracleSource a (Vector n b)

------------------------------ Hasher & Oracle ---------------------------------

type Hasher a = [a] -> a

mimcHash :: forall a. Arithmetic a => Hasher a
mimcHash = mimcHashN' mimcConstants (zero :: a)

oracle :: OracleSource a b => Hasher a -> b -> a
oracle hash = hash . source

------------------------ Generic OracleSource deriving -------------------------

class GOracleSource a f where
    gsource :: f x -> [a]

instance (Ring a, GOracleSource a f, GOracleSource a g) =>
         GOracleSource a (f :+: g) where
    gsource (L1 x) = zero : gsource x
    gsource (R1 x) = one  : gsource x

instance GOracleSource a U1 where
    gsource _ = []

instance (GOracleSource a f, GOracleSource a g) =>
         GOracleSource a (f :*: g) where
    gsource (x :*: y) = gsource x ++ gsource y

instance OracleSource a b => GOracleSource a (Rec0 b) where
    gsource (K1 x) = source x

instance GOracleSource a f => GOracleSource a (M1 c m f) where
    gsource (M1 x) = gsource x
