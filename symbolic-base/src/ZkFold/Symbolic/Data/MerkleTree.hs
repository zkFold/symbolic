{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.MerkleTree where



import ZkFold.Symbolic.Data.Class
import GHC.Generics
import ZkFold.Symbolic.Data.Payloaded (Payloaded)
import           GHC.TypeNats
import           Data.Functor.Rep                 (Representable, distributeRep)
import           Data.Functor                     (Functor)
import Data.Distributive (Distributive, distribute)

data Leaf l a = Leaf { leafHash    :: l a }
  deriving (Functor, Generic1)

instance (Representable l) => Representable (Leaf l)

instance (Representable l) => Distributive (Leaf l) where
  distribute = distributeRep



data MerkleTree d x = MerkleTree {
    mHash    :: Context x (Layout x)
  , mLeft    :: Payloaded (Par1 :*: Leaf (Layout x) :*: MerkleTree (d - 1)) (Context x)
  , mRight   :: Payloaded (Par1 :*: Leaf (Layout x) :*: MerkleTree (d - 1)) (Context x)
  }
  deriving (Generic)

-- instance Functor (MerkleTree d)
instance Representable (MerkleTree d)

-- instance (SymbolicData x, c ~ Context x) => SymbolicData (MerkleTree d x)
