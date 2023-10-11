{-# LANGUAGE TypeApplications #-}

module ZkFold.Crypto.Data.Conditional (
    GeneralizedConditional (..) 
) where

import           Control.Monad.State                   (MonadState (..), evalState)
import           Prelude                               hiding (Num(..), (/))

import           ZkFold.Crypto.Algebra.Basic.Class
import           ZkFold.Crypto.Data.Arithmetization
import           ZkFold.Crypto.Data.Bool               (SymbolicBool (..), GeneralizedBoolean)

class GeneralizedBoolean b => GeneralizedConditional b a where
    bool :: a -> a -> b -> a

    gif :: b -> a -> a -> a
    gif b x y = bool y x b

    (?) :: b -> a -> a -> a
    (?) = gif

instance GeneralizedConditional Bool a where
    bool f t b = if b then t else f

instance (Arithmetization ctx a, FiniteField ctx) => GeneralizedConditional (SymbolicBool ctx) a where
    bool brFalse brTrue (SymbolicBool b) = flip evalState b $ do
        f' <- atomic @ctx @a <$> (merge brFalse >> get)
        t' <- atomic @ctx @a <$> (merge brTrue >> get)
        put $ mconcat $ zipWith (\f t -> b * t + (one - b) * f) f' t'
        current