{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Compiler.CompileWith (specCompileWith) where

import           Control.Applicative             ((<*>))
import           Control.Monad                   (return)
import           Data.Binary                     (Binary)
import           Data.Function                   (($))
import           Data.Functor                    ((<$>))
import           GHC.Generics                    (U1 (..), (:*:) (..))
import           Test.Hspec                      (Spec, describe)
import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck                 (Arbitrary (..), (===))
import           Text.Show                       (Show)

import           ZkFold.Symbolic.Class           (Arithmetic, Symbolic)
import           ZkFold.Symbolic.Compiler        (compileWith, guessOutput, witnessGenerator)
import           ZkFold.Symbolic.Data.Bool       ((&&))
import           ZkFold.Symbolic.Data.ByteString (ByteString)

testFunction :: Symbolic c => ByteString 256 c -> ByteString 256 c -> ByteString 256 c
testFunction = (&&)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary ((f :*: g) a) where
  arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance Arbitrary (U1 a) where
  arbitrary = return U1

specCompileWith :: forall a. (Arbitrary a, Arithmetic a, Binary a, Show a) => Spec
specCompileWith = describe "CompileWith specification" $ do
  prop "Guessing with payload is constant in input" $
    let circuit = compileWith @a guessOutput
            (\(p :*: q) -> (U1 :*: U1 :*: U1, p :*: q :*: U1)) testFunction
     in \x p q -> witnessGenerator circuit (x :*: p)
              === witnessGenerator circuit (x :*: q)
