module ZkFold.Symbolic.Data.Switch where

import Data.Function (($))

import ZkFold.Symbolic.Class (Symbolic (..))
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Conditional (Conditional (..))
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
  restore (sLayout, sPayload) = Switch {..}

instance (Symbolic c, SymbolicData x) => Conditional (Bool c) (Switch c x) where
  bool (Switch fl fp) (Switch tl tp) b =
    Switch (bool fl tl b) $
      runPayloaded $
        bool (Payloaded fp :: Payloaded (Payload x) c) (Payloaded tp) b
