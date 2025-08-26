{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.Class where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData1)
import Data.Bifunctor (bimap)
import Data.Constraint (Dict (..), withDict)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Functor.Rep (Representable, pureRep)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Semialign (Semialign, Zip, zipWith)
import Data.Traversable (Traversable)
import Data.Tuple (curry)
import Data.Type.Equality (type (~))
import Data.Typeable (Proxy (..))
import qualified GHC.Generics as G

import ZkFold.Algebra.Class (Order, zero)
import ZkFold.Algebra.Number (Natural)
import ZkFold.Control.HApplicative (hliftA2, hpure)
import ZkFold.Data.HFunctor (hmap)
import ZkFold.Data.Orphans ()
import ZkFold.Data.Package (pack, unpack)
import ZkFold.Data.Product (fstP, sndP)
import qualified ZkFold.Symbolic.Algorithm.Interpolation as I
import ZkFold.Symbolic.Class

type PayloadFunctor f = Semialign f

type LayoutFunctor f = (PayloadFunctor f, Traversable f, NFData1 f)

type DataFunctor x n =
  (LayoutFunctor (Layout x n), PayloadFunctor (Payload x n))

type RepData x (n :: Natural) =
  (Representable (Layout x n), Representable (Payload x n))

-- | A class for Symbolic data types.
class SymbolicData x where
  type Layout x (n :: Natural) :: Type -> Type
  type Layout x n = Layout (G.Rep1 x) n

  type Payload x (n :: Natural) :: Type -> Type
  type Payload x n = Payload (G.Rep1 x) n

  type HasRep x (c :: Ctx) :: Constraint
  type HasRep x c = HasRep (G.Rep1 x) c

  dataFunctor :: Proxy '(x, n) -> Dict (DataFunctor x n)
  default dataFunctor
    :: ( SymbolicData (G.Rep1 x)
       , Layout x n ~ Layout (G.Rep1 x) n
       , Payload x n ~ Payload (G.Rep1 x) n
       )
    => Proxy '(x, n) -> Dict (DataFunctor x n)
  dataFunctor (_ :: Proxy '(x, n)) = dataFunctor @(G.Rep1 x) @n Proxy

  hasRep :: HasRep x c => Proxy '(x, c) -> Dict (RepData x (Order (BaseField c)))
  default hasRep
    :: ( HasRep x c
       , SymbolicData (G.Rep1 x)
       , HasRep x c ~ HasRep (G.Rep1 x) c
       , n ~ Order (BaseField c)
       , Layout x n ~ Layout (G.Rep1 x) n
       , Payload x n ~ Payload (G.Rep1 x) n
       )
    => Proxy '(x, c) -> Dict (RepData x (Order (BaseField c)))
  hasRep (_ :: Proxy '(x, c)) = hasRep @(G.Rep1 x) @c Proxy

  -- | Returns the circuit that makes up `x`.
  arithmetize :: Symbolic c => x c -> c (Layout x (Order (BaseField c)))
  default arithmetize
    :: ( Symbolic c
       , Order (BaseField c) ~ n
       , G.Generic1 x
       , SymbolicData (G.Rep1 x)
       , Layout x n ~ Layout (G.Rep1 x) n
       )
    => x c
    -> c (Layout x (Order (BaseField c)))
  arithmetize = arithmetize . G.from1

  payload :: Symbolic c => x c -> Payload x (Order (BaseField c)) (WitnessField c)
  default payload
    :: ( Symbolic c
       , Order (BaseField c) ~ n
       , G.Generic1 x
       , SymbolicData (G.Rep1 x)
       , Payload x n ~ Payload (G.Rep1 x) n
       )
    => x c
    -> Payload x (Order (BaseField c)) (WitnessField c)
  payload = payload . G.from1

  -- | Interpolates branch values between given points.
  interpolate :: Symbolic c => NonEmpty (BaseField c, x c) -> c G.Par1 -> x c
  default interpolate
    :: ( G.Generic1 x
       , SymbolicData (G.Rep1 x)
       , Symbolic c
       )
    => NonEmpty (BaseField c, x c)
    -> c G.Par1
    -> x c
  interpolate = (G.to1 .) . interpolate . fmap (G.from1 <$>)

  -- | Restores `x` from the circuit's outputs.
  restore
    :: (Symbolic c, Order (BaseField c) ~ n)
    => (c (Layout x n), Payload x n (WitnessField c))
    -> x c
  default restore
    :: ( Symbolic c
       , Order (BaseField c) ~ n
       , G.Generic1 x
       , SymbolicData (G.Rep1 x)
       , Layout x n ~ Layout (G.Rep1 x) n
       , Payload x n ~ Payload (G.Rep1 x) n
       )
    => (c (Layout x n), Payload x n (WitnessField c))
    -> x c
  restore = G.to1 . restore

withoutConstraints
  :: (SymbolicData x, Symbolic c, Traversable (Layout x (Order (BaseField c))))
  => x c -> x c
withoutConstraints x = restore (embedW $ witnessF $ arithmetize x, payload x)

dummy :: forall x c. (SymbolicData x, HasRep x c, Symbolic c) => x c
dummy =
  withDict (hasRep @x @c Proxy) $ restore (embed (pureRep zero), pureRep zero)

instance SymbolicData Proxy where
  type Layout Proxy _ = G.U1
  type Payload Proxy _ = G.U1
  type HasRep Proxy _ = ()

  dataFunctor _ = Dict
  hasRep _ = Dict
  arithmetize _ = hpure G.U1
  payload _ = G.U1
  interpolate _ _ = Proxy
  restore _ = Proxy

instance (SymbolicData x, SymbolicData y) => SymbolicData (x G.:*: y) where
  type Layout (x G.:*: y) n = Layout x n G.:*: Layout y n
  type Payload (x G.:*: y) n = Payload x n G.:*: Payload y n
  type HasRep (x G.:*: y) n = (HasRep x n, HasRep y n)

  dataFunctor (_ :: Proxy '(xy, n)) =
    case (dataFunctor @x @n Proxy, dataFunctor @y @n Proxy) of
      (Dict, Dict) -> Dict
  hasRep (_ :: Proxy '(xy, c)) =
    case (hasRep @x @c Proxy, hasRep @y @c Proxy) of
      (Dict, Dict) -> Dict
  arithmetize (a G.:*: b) = hliftA2 (G.:*:) (arithmetize a) (arithmetize b)
  payload (a G.:*: b) = payload a G.:*: payload b
  interpolate bs =
    liftA2
      (G.:*:)
      (interpolate (fmap fstP <$> bs))
      (interpolate (fmap sndP <$> bs))
  restore f =
    restore (bimap (hmap fstP) fstP f)
      G.:*: restore (bimap (hmap sndP) sndP f)

instance (Zip f, LayoutFunctor f, SymbolicData x) => SymbolicData (f G.:.: x) where
  type Layout (f G.:.: x) n = f G.:.: Layout x n
  type Payload (f G.:.: x) n = f G.:.: Payload x n
  type HasRep (f G.:.: x) n = (Representable f, HasRep x n)

  dataFunctor (_ :: Proxy '(fx, n)) = case dataFunctor @x @n Proxy of
    Dict -> Dict
  hasRep (_ :: Proxy '(fx, c)) = case hasRep @x @c Proxy of
    Dict -> Dict
  arithmetize (G.Comp1 xs) = pack (arithmetize <$> xs)
  payload (G.Comp1 xs) = G.Comp1 (payload <$> xs)
  interpolate (I.pushInterpolation . fmap (G.unComp1 <$>) -> bs) i =
    G.Comp1 $ (`interpolate` i) <$> bs
  restore (c, G.Comp1 ps) = G.Comp1 $ zipWith (curry restore) (unpack c) ps

instance SymbolicData x => SymbolicData (G.M1 i c x) where
  type Layout (G.M1 i c x) n = Layout x n
  type Payload (G.M1 i c x) n = Payload x n
  type HasRep (G.M1 i c x) n = HasRep x n
  dataFunctor (_ :: Proxy '(m, n)) = case dataFunctor @x @n Proxy of
    Dict -> Dict
  hasRep (_ :: Proxy '(m, d)) = case hasRep @x @d Proxy of
    Dict -> Dict
  arithmetize = arithmetize . G.unM1
  payload = payload . G.unM1
  interpolate = (G.M1 .) . interpolate . fmap (G.unM1 <$>)
  restore = G.M1 . restore

instance SymbolicData x => SymbolicData (G.Rec1 x) where
  type Layout (G.Rec1 x) n = Layout x n
  type Payload (G.Rec1 x) n = Payload x n
  type HasRep (G.Rec1 x) n = HasRep x n
  dataFunctor (_ :: Proxy '(r, n)) = case dataFunctor @x @n Proxy of
    Dict -> Dict
  hasRep (_ :: Proxy '(r, c)) = case hasRep @x @c Proxy of
    Dict -> Dict
  arithmetize (G.Rec1 x) = arithmetize x
  payload (G.Rec1 x) = payload x
  interpolate = (G.Rec1 .) . interpolate . fmap (G.unRec1 <$>)
  restore f = G.Rec1 (restore f)
