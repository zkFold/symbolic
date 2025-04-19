{-# LANGUAGE TypeOperators #-}

module ZkFold.Data.Type where

import           Prelude hiding (length)

type family (:++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[]       :++ ys = ys
    (x ': xs) :++ ys = x ': (xs :++ ys)

-- | Type list membership test.
type family Find x ys where
    Find x '[]       = 'False
    Find x (x ': ys) = 'True
    Find x (y ': ys) = Find x ys
