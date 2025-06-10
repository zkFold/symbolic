{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Common where

import           Data.Typeable   (TypeRep, Typeable, Proxy (..), typeRep)
import           Prelude         (String)
import qualified Test.QuickCheck as QC
import qualified Test.Hspec      as Hspec
import ZkFold.Algebra.Number (Natural)
import Test.QuickCheck (Gen)
import ZkFold.Prelude (chooseNatural)
import ZkFold.Algebra.Class ((-!))
import ZkFold.Symbolic.Interpreter (Interpreter (..))
import GHC.Generics (Par1(..))
import ZkFold.Symbolic.Data.Bool (Bool(..))

it :: QC.Testable prop => String -> prop -> Hspec.SpecWith (Hspec.Arg QC.Property)
it desc prop = Hspec.it desc (QC.property prop)

typeAt :: forall a. Typeable a => TypeRep
typeAt = typeRep (Proxy :: Proxy a)

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x -! 1)

toss1 :: Natural -> Gen Natural
toss1 x = chooseNatural (1, x -! 1)

evalBool :: forall a . Bool (Interpreter a) -> a
evalBool (Bool (Interpreter (Par1 v))) = v
