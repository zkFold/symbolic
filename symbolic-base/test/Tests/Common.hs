{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Common where

import           Data.Typeable   (TypeRep, Typeable, Proxy (..), typeRep)
import           Prelude         (String)
import qualified Test.QuickCheck as QC
import qualified Test.Hspec      as Hspec

it :: QC.Testable prop => String -> prop -> Hspec.SpecWith (Hspec.Arg QC.Property)
it desc prop = Hspec.it desc (QC.property prop)

typeAt :: forall a. Typeable a => TypeRep
typeAt = typeRep (Proxy :: Proxy a)
