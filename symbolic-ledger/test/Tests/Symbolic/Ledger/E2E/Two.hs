module Tests.Symbolic.Ledger.E2E.Two (
  specE2ETwo,
  prevState,
  batch,
  witness,
  newState,
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
import GHC.Generics ((:.:) (..))
import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.Symbolic.Data.Bool (true)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import Prelude (($))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Examples.Two
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Validation.State (validateStateUpdateIndividualChecks)

-- End-to-end test for a slightly intricate case.
specE2ETwo :: Spec
specE2ETwo =
  it "E2E Two" $ do
    let
    sLength newState `shouldBe` (one :: FieldElement I)
    validateStateUpdateIndividualChecks prevState batch newState witness `shouldBe` Haskell.pure true
    unComp1 utxoPreimage2 `shouldBe` pure (nullUTxO @A @I)
