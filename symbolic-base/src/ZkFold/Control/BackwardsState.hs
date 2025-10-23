{-# LANGUAGE BlockArguments #-}

module ZkFold.Control.BackwardsState where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), ap)
import Data.Function ((.))
import Data.Functor (Functor)
import Data.Tuple (fst)

newtype BState s a = MkBState {runBState :: s -> (a, s)}
  deriving Functor

evalBState :: s -> BState s a -> a
evalBState s (MkBState f) = fst (f s)

instance Applicative (BState s) where
  pure = MkBState . (,)
  (<*>) = ap

instance Monad (BState s) where
  MkBState f >>= k = MkBState \s ->
    let ~(a, s'') = f s'
        ~(b, s') = runBState (k a) s
     in (b, s'')
