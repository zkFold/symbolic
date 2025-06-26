module ZkFold.Symbolic.Data.Switch where

import Data.Functor (fmap, (<$>))
import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Payloaded (Payloaded (..))

-- | A 'Switch' of a 'SymbolicData' @x@ to context @c@
-- is a separate Symbolic datatype which has the same layout and payload as @x@,
-- but is defined in a context @c@ which can differ from @'Context' x@.
--
-- In other words, it is a useful default 'Replica' of @x@ in context @c@
-- when nothing else works.
data Switch c x = Switch
  { sLayout :: c (Layout x)
  , sPayload :: Payload x (WitnessField c)
  }

instance (Symbolic c, SymbolicData x) => SymbolicData (Switch c x) where
  type Context (Switch c x) = c
  type Layout (Switch c x) = Layout x
  type Payload (Switch c x) = Payload x
  arithmetize = sLayout
  payload = sPayload
  interpolate bs pt =
    let (sLayout, Payloaded sPayload) =
          interpolate (fmap (\(Switch l p) -> (l, Payloaded p)) <$> bs) pt
     in Switch {..}
  restore (sLayout, sPayload) = Switch {..}
