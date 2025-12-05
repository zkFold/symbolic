{-# LANGUAGE BlockArguments #-}

module ZkFold.Control.BackwardsState where

import Data.Tuple (fst)
import Data.Functor (Functor)
import Control.Applicative (Applicative (..))
import Data.Function ((.))
import Control.Monad (ap, Monad (..))

newtype BState s a = MkBState { runBState :: s -> (a, s) }
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
