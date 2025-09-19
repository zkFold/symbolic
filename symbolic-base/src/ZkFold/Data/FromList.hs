{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.FromList where

import Control.Monad.State (State, state, runState)
import GHC.Generics (Par1 (..), (:*:) (..))
import GHC.Err (error)
import Control.Applicative (liftA2)

class FromList f where
  parseList :: State [a] (f a)

instance FromList Par1 where
  parseList = state \case
    [] -> error "parseList @Par1: empty list"
    (x:xs) -> (Par1 x, xs)

instance (FromList f, FromList g) => FromList (f :*: g) where
  parseList = liftA2 (:*:) parseList parseList

fromList :: FromList f => [a] -> f a
fromList input = case runState parseList input of
  (result, []) -> result
  _ -> error "fromList: unconsumed elements"
