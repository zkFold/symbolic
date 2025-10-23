module Tests.Symbolic.Ledger.E2E.One (specE2EOne) where

import Control.Applicative (pure)
import Data.Function ((&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.IsList (IsList (..))
import GHC.Natural (Natural)
import Test.Hspec (Spec, it, shouldBe)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Compat (CompatData)
import ZkFold.Symbolic.Data.Bool (false, true)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import Prelude (($))
import Prelude qualified as Haskell

import ZkFold.Symbolic.Ledger.Offchain.State.Update (updateLedgerState)
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field
import ZkFold.Symbolic.Ledger.Validation.State (validateStateUpdateIndividualChecks)

type Fq = RollupBF

-- Small sizes for simplicity
type Bi = 1

type Bo = 1

type Ud = 2 -- Thus 2 ^ (2 - 1) = 2 leaves

type A = 1

type Ixs = 1

type Oxs = 1

type TxCount = 1

-- End-to-end test for a very simplified case.
specE2EOne :: Spec
specE2EOne =
  it "E2E One" $ do
    let
      emptyTree :: SymMerkle.MerkleTree Ud Fq
      emptyTree = SymMerkle.fromLeaves (pure (nullUTxOHash @A @Fq))

      prevState :: State Bi Bo Ud A Fq
      prevState =
        State
          { sPreviousStateHash = zero
          , sUTxO = emptyTree
          , sLength = zero
          , sBridgeIn = hash (Comp1 (pure (nullOutput @A @Fq)))
          , sBridgeOut = hash (Comp1 (pure (nullOutput @A @Fq)))
          }

      utxoPreimage :: Leaves Ud (UTxO A Fq)
      utxoPreimage = pure (nullUTxO @A @Fq)
      privateKey :: PrivateKey Fq
      privateKey = fromConstant (1 :: Natural)
      publicKey :: PublicKey Fq
      publicKey = privateKey `scale` pointGen @(EdDSAPoint Fq)

      address = hashFn publicKey

      adaAsset =
        Comp1 $
          fromList
            [AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (1_000_000 :: Natural)}]
      bridgeInOutput = Output {oAddress = address, oAssets = adaAsset}
      -- We bridge in an output and refer to it in transaction.
      bridgedIn :: (Vector Bi :.: Output A) Fq
      bridgedIn = Comp1 (fromList [bridgeInOutput])

      bridgeInHash :: HashSimple Fq
      bridgeInHash = (one :: CompatData FieldElement Fq) & hash & Base.hHash

      tx :: Transaction Ixs Oxs A Fq
      tx =
        Transaction
          { inputs = Comp1 (fromList [OutputRef {orTxId = bridgeInHash, orIndex = zero}])
          , outputs = Comp1 (fromList [bridgeInOutput :*: false])
          }

      batch :: TransactionBatch Ixs Oxs A TxCount Fq
      batch = TransactionBatch {tbTransactions = pure tx}

      sigs =
        let rPoint :*: s = signTransaction tx privateKey
         in Comp1 (fromList [Comp1 (fromList [rPoint :*: s :*: publicKey])])

      newState :*: witness :*: utxoPreimage2 = updateLedgerState prevState utxoPreimage bridgedIn batch sigs

    sLength newState `shouldBe` (one :: CompatData FieldElement Fq)
    validateStateUpdateIndividualChecks prevState batch newState witness `shouldBe` Haskell.pure true
    -- Now let's try to use this newly created output and bridge it out, leaving no UTxOs in the ledger.
    let
      tx2 :: Transaction Ixs Oxs A Fq
      tx2 =
        Transaction
          { inputs = Comp1 (fromList [OutputRef {orTxId = txId tx & Base.hHash, orIndex = zero}])
          , outputs = Comp1 (fromList [bridgeInOutput :*: true])
          }
      bridgedIn2 :: (Vector Bi :.: Output A) Fq
      bridgedIn2 = Comp1 (fromList [nullOutput @A @Fq])
      batch2 :: TransactionBatch Ixs Oxs A TxCount Fq
      batch2 = TransactionBatch {tbTransactions = pure tx2}
      sigs2 =
        let rPoint :*: s = signTransaction tx2 privateKey
         in Comp1 (fromList [Comp1 (fromList [rPoint :*: s :*: publicKey])])
      newState2 :*: witness2 :*: utxoPreimage3 = updateLedgerState newState (unComp1 utxoPreimage2) bridgedIn2 batch2 sigs2

    validateStateUpdateIndividualChecks newState batch2 newState2 witness2 `shouldBe` Haskell.pure true
    unComp1 utxoPreimage3 `shouldBe` pure (nullUTxO @A @Fq)
