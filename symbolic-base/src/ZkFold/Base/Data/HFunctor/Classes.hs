{-# LANGUAGE QuantifiedConstraints #-}

module ZkFold.Base.Data.HFunctor.Classes where

import           Control.DeepSeq      (NFData, NFData1)
import           Data.Bool            (Bool)
import           Data.Eq              (Eq)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.Int             (Int)
import           Text.Show            (Show, ShowS)

class (forall f. Eq1 f => Eq (c f)) => HEq c where
  hliftEq ::
    (forall a. (a -> a -> Bool) -> f a -> f a -> Bool) -> c f -> c f -> Bool

class (forall f. Show1 f => Show (c f)) => HShow c where
  hliftShowsPrec ::
    (forall a. (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS) ->
    (forall a. (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS) ->
    Int -> c f -> ShowS

class (forall f. NFData1 f => NFData (c f)) => HNFData c where
  hliftRnf :: (forall a. (a -> ()) -> f a -> ()) -> c f -> ()
