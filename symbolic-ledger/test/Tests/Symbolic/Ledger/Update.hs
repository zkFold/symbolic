module Tests.Symbolic.Ledger.Update (specUpdateLedgerState) where

import GHC.Generics ((:*:) (..), (:.:) (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude (($), undefined)

import Control.Applicative (pure)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.BLS12_381 (Fr)
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.Bool (false)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import ZkFold.Symbolic.Interpreter (Interpreter)
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (Vector)

import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Offchain.State.Update (updateLedgerState)
import GHC.IsList (IsList(..))

type I = Interpreter Fr

-- Choose tiny sizes for simplicity
type Bi = 1
type Bo = 1
type Ud = 2 -- Thus 2 ^ (2 - 1) = 2 leaves
type A = 1
type Ixs = 1
type Oxs = 1
type TxCount = 1

specUpdateLedgerState :: Spec
specUpdateLedgerState = describe "updateLedgerState" $ do
  it "simple handcrafted test" $ do
    let 
        emptyTree :: SymMerkle.MerkleTree Ud I
        emptyTree = SymMerkle.fromLeaves (pure (nullUTxOHash @A @I))

        prevState :: State Bi Bo Ud A I
        prevState =
          State
            { sPreviousStateHash = zero
            , sUTxO = emptyTree
            , sLength = zero
            , sBridgeIn = hash (Comp1 (pure (nullOutput @A @I)))
            , sBridgeOut = hash (Comp1 (pure (nullOutput @A @I)))
            }

        utxoPreimage :: Leaves Ud (UTxO A I)
        utxoPreimage = pure (nullUTxO @A @I)
        
        adaAsset = Comp1 $ fromList [AssetValue {assetPolicy = undefined, assetName = undefined, assetQuantity = undefined}]
        bridgeInOutput = Output {oAddress = undefined, oAssets = adaAsset}
        -- We bridge in an output and refer to it in transaction.
        bridgedIn :: (Vector Bi :.: Output A) I
        bridgedIn = Comp1 (fromList [bridgeInOutput])
        
        bridgeInHash :: HashSimple I
        bridgeInHash = undefined

        tx :: Transaction Ixs Oxs A I
        tx =
          Transaction
            { inputs = Comp1 (fromList [OutputRef {orTxId = bridgeInHash, orIndex = zero}])
            , outputs = Comp1 (fromList [bridgeInOutput :*: false])
            }

        batch :: TransactionBatch Ixs Oxs A TxCount I
        batch = TransactionBatch {tbTransactions = pure tx}

        sigs = 
          let rPoint :*: s = signTransaction tx undefined
          in
          Comp1 (fromList [Comp1 (fromList [rPoint :*: s :*: undefined])])

        newState :*: _witness = updateLedgerState prevState utxoPreimage bridgedIn batch sigs

    -- sUTxO newState `shouldBe` emptyTree
    sLength newState `shouldBe` (one :: FieldElement I)

