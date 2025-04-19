{-# LANGUAGE TypeOperators #-}

module ZkFold.Protocol.IVC.StepFunction where

import           Prelude                           hiding (Num (..), drop, head, replicate, take, zipWith)

import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))

type StepFunctionAssumptions a f ctx =
    ( Symbolic ctx
    , BaseField ctx ~ a
    , FieldElement ctx ~ f
    )

type StepFunction a i p = forall f ctx . StepFunctionAssumptions a f ctx
    => i f -> p f -> i f
