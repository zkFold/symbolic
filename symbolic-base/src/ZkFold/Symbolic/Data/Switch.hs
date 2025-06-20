module ZkFold.Symbolic.Data.Switch where


import           GHC.Generics               (Generic1)

import           ZkFold.Symbolic.Data.Class (SymbolicData (..))

-- | A 'Switch' of a 'SymbolicData' @x@ to context @c@
-- is a separate Symbolic datatype which has the same layout and payload as @x@,
-- but is defined in a context @c@ which can differ from @'Context' x@.
--
-- In other words, it is a useful default 'Replica' of @x@ in context @c@
-- when nothing else works.
data Switch x c = Switch { getSwitch :: x c }
  deriving Generic1

instance SymbolicData x => SymbolicData (Switch x)
