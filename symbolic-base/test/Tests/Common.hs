{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Common where

import Data.Typeable (Proxy (..), TypeRep, Typeable, typeRep)
import qualified Test.Hspec as Hspec
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Prelude (String)

import ZkFold.Algebra.Class ((-!))
import ZkFold.Algebra.Number (Natural)
import ZkFold.Prelude (chooseNatural)

it :: QC.Testable prop => String -> prop -> Hspec.SpecWith (Hspec.Arg QC.Property)
it desc prop = Hspec.it desc (QC.property prop)

typeAt :: forall a. Typeable a => TypeRep
typeAt = typeRep (Proxy :: Proxy a)

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x -! 1)

toss1 :: Natural -> Gen Natural
toss1 x = chooseNatural (1, x -! 1)
