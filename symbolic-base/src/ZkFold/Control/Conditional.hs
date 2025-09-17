module ZkFold.Control.Conditional where

import Data.Bool (Bool)
import qualified Data.Bool as H

-- TODO: move to ZkFold.Data.Bool

class Conditional b a where
  -- | Properties:
  --
  -- [On true] @bool onFalse onTrue 'true' == onTrue@
  --
  -- [On false] @bool onFalse onTrue 'false' == onFalse@
  bool :: a -> a -> b -> a

ifThenElse :: Conditional b a => b -> a -> a -> a
ifThenElse b x y = bool y x b

instance Conditional Bool a where bool = H.bool
