module ZkFold.Data.Utils where

import Control.Applicative (Applicative)
import Data.Traversable (Traversable (..))
import Data.Zip (Zip (..))
import Prelude (($))

zipWithM :: (Applicative m, Traversable f, Zip f) => (a -> b -> m c) -> f a -> f b -> m (f c)
zipWithM f a b = sequenceA $ zipWith f a b
