{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Symbolic.Ledger.E2E.One (
  specE2EOne,
  prevState,
  address,
  bridgeInOutput,
  batch,
  witness,
  newState,
  newState2,
  witness2,
  utxoPreimage3,
  batch2,
  I,
  Bi,
  Bo,
  Ud,
  A,
  Ixs,
  Oxs,
  TxCount,
) where

import Control.Applicative (pure)
import Data.Type.Equality (type (~))
import GHC.Generics ((:.:) (..))
import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.Data.Eq (BooleanOf, Eq, (==))
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude (($))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Examples.One
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field
import ZkFold.Symbolic.Ledger.Validation.State (validateStateUpdateIndividualChecks)

type Fq = RollupBF

instance
  {-# OVERLAPPABLE #-}
  (Eq a, ToConstant (BooleanOf a), Const (BooleanOf a) ~ Haskell.Bool)
  => Haskell.Eq a
  where
  x == y = toConstant (x == y)

-- End-to-end test for a very simplified case.
specE2EOne :: Spec
specE2EOne =
  it "E2E One" $ do
    sLength newState `shouldBe` (one :: FieldElement Fq)
    validateStateUpdateIndividualChecks prevState batch newState witness `shouldBe` Haskell.pure true
    validateStateUpdateIndividualChecks newState batch2 newState2 witness2 `shouldBe` Haskell.pure true
    unComp1 utxoPreimage3 `shouldBe` pure (nullUTxO @A @Fq)
