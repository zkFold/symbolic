{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Conditional where

import qualified Data.Bool                        as H
import           Data.Function                    (($))
import           Data.Functor.Rep                 (mzipWithRep)
import           Data.Proxy
import           GHC.Generics                     (K1 (..), M1 (..), Par1 (..), Rec0, type (:*:) (..))
import qualified GHC.Generics                     as G
import qualified Prelude

import           ZkFold.Algebra.Class
import           ZkFold.Algebra.Field
import           ZkFold.Algebra.Number
import           ZkFold.Data.Vector
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool        (Bool (Bool), BoolType)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators (mzipWithMRep)
import           ZkFold.Symbolic.MonadCircuit     (newAssigned)

class BoolType b => Conditional b a where
    -- | Properties:
    --
    -- [On true] @bool onFalse onTrue 'true' == onTrue@
    --
    -- [On false] @bool onFalse onTrue 'false' == onFalse@
    bool :: a -> a -> b -> a
    default bool :: (G.Generic a, GConditional b (G.Rep a)) => a -> a -> b -> a
    bool f t b = G.to (gbool (G.from f) (G.from t) b)

ifThenElse :: Conditional b a => b -> a -> a -> a
ifThenElse b x y = bool y x b

(?) :: Conditional b a => b -> a -> a -> a
(?) = ifThenElse

instance (Symbolic c, LayoutFunctor f) => Conditional (Bool c) (c f) where
    bool x y (Bool b) = restore $ \s ->
      ( symbolic3F b (arithmetize x s) (arithmetize y s)
          (\(Par1 c) f t -> if c Prelude.== zero then f else t)
          \(Par1 c) -> mzipWithMRep $ \i j -> do
            i' <- newAssigned (\w -> (one - w c) * w i)
            j' <- newAssigned (\w -> w c * w j)
            newAssigned (\w -> w i' + w j')
      , let Par1 wb = witnessF b
         in mzipWithRep
              (\wx wy -> (one - wb) * wx + wb * wy)
              (payload x s)
              (payload y s)
      )

deriving newtype instance Symbolic c => Conditional (Bool c) (Bool c)

instance Symbolic c => Conditional (Bool c) (Proxy c) where
  bool _ _ _ = Proxy

instance Conditional Prelude.Bool Prelude.Bool where bool = H.bool
instance Conditional Prelude.Bool Prelude.String where bool = H.bool
instance Conditional Prelude.Bool Natural where bool = H.bool
instance Conditional Prelude.Bool (Zp n) where bool = H.bool

instance (KnownNat n, Conditional bool x) => Conditional bool (Vector n x) where
  bool fv tv b = mzipWithRep (\f t -> bool f t b) fv tv

instance Conditional bool field => Conditional bool (Ext2 field i)
instance Conditional bool field => Conditional bool (Ext3 field i)

instance
  ( Conditional b x0
  , Conditional b x1
  ) => Conditional b (x0,x1)

instance
  ( Conditional b x0
  , Conditional b x1
  , Conditional b x2
  ) => Conditional b (x0,x1,x2)

instance
  ( Conditional b x0
  , Conditional b x1
  , Conditional b x2
  , Conditional b x3
  ) => Conditional b (x0,x1,x2,x3)

class BoolType b => GConditional b u where
    gbool :: u x -> u x -> b -> u x

instance (BoolType b, GConditional b u, GConditional b v) => GConditional b (u :*: v) where
  gbool (f0 :*: f1) (t0 :*: t1) b = gbool f0 t0 b :*: gbool f1 t1 b

instance GConditional b v => GConditional b (M1 i c v) where
  gbool (M1 f) (M1 t) b = M1 (gbool f t b)

instance Conditional b x => GConditional b (Rec0 x) where
  gbool (K1 f) (K1 t) b = K1 (bool f t b)
