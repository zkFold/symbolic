module ZkFold.Symbolic.Data.Switch where

import           Data.Function                  (const, (.))
import           Data.Functor                   (fmap, (<$>))
import           Data.Proxy                     (Proxy (..))

import           ZkFold.Symbolic.Class          (Symbolic (..))
import           ZkFold.Symbolic.Data.Class     (SymbolicData (..))
import           ZkFold.Symbolic.Data.Payloaded (Payloaded (..))

-- | A 'Switch' of a 'SymbolicData' @x@ to context @c@
-- is a separate Symbolic datatype which has the same layout and payload as @x@,
-- but is defined in a context @c@ which can differ from @'Context' x@.
--
-- In other words, it is a useful default 'Replica' of @x@ in context @c@
-- when nothing else works.
data Switch c x = Switch
    { sLayout  :: c (Layout x)
    , sPayload :: Payload x (WitnessField c)
    }

instance (Symbolic c, SymbolicData x) => SymbolicData (Switch c x) where
    type Context (Switch c x) = c
    type Support (Switch c x) = Proxy c
    type Layout (Switch c x) = Layout x
    type Payload (Switch c x) = Payload x
    arithmetize = const . sLayout
    payload = const . sPayload
    interpolate bs pt =
        let (sLayout, Payloaded sPayload) =
                interpolate (fmap (\(Switch l p) -> (l, Payloaded p)) <$> bs) pt
         in Switch {..}
    restore f = let (sLayout, sPayload) = f Proxy in Switch {..}
