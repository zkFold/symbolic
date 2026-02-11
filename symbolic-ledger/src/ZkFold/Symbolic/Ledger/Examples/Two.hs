{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ZkFold.Symbolic.Ledger.Examples.Two (
  prevState,
  batch,
  witness,
  newState,
  utxoPreimage2,
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
import Data.Function ((&))
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Data.Bool (false, true)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.Hash qualified as Base
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import Prelude (($))

import ZkFold.Symbolic.Ledger.Offchain.State.Update (updateLedgerState)
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

type I = RollupBFInterpreter

type Bi = 3

type Bo = 4

type Ud = 3 -- Thus 2 ^ (3 - 1) = 4 leaves

type A = 3

type Ixs = 3

type Oxs = 3

type TxCount = 3

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

privateKey :: PrivateKey I
privateKey = fromConstant (1 :: Natural)

privateKey2 :: PrivateKey I
privateKey2 = fromConstant (2 :: Natural)

privateKey3 :: PrivateKey I
privateKey3 = fromConstant (3 :: Natural)

publicKey :: PublicKey I
publicKey = privateKey `scale` pointGen @(EdDSAPoint I)

publicKey2 :: PublicKey I
publicKey2 = privateKey2 `scale` pointGen @(EdDSAPoint I)

publicKey3 :: PublicKey I
publicKey3 = privateKey3 `scale` pointGen @(EdDSAPoint I)

address = hashFn publicKey

address2 = hashFn publicKey2

address3 = hashFn publicKey3

asset2Policy :: AssetPolicy I = one + one

asset2Name :: AssetName I = adaName -- same as ADA.

asset3Policy :: AssetPolicy I = one + one + one

asset3Name :: AssetName I = one + one + one

bridgeInOutput =
  Output
    { oAddress = address
    , oAssets =
        Comp1 $
          unsafeToVector'
            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (10_000_000 :: Natural)}
            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
            ]
    }

-- "address" has 10 ADA and 100 asset2
bridgeInOutput2 :: Output A I = nullOutput

bridgeInOutput3 :: Output A I =
  Output
    { oAddress = address3
    , oAssets =
        Comp1 $
          unsafeToVector'
            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (10_000_000 :: Natural)}
            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
            , AssetValue {assetPolicy = asset3Policy, assetName = asset3Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
            ]
    }

-- "address3" has 10 ADA and 50 asset2 and 50 asset3.
-- We bridge in an output and refer to it in transaction.
bridgedIn :: (Vector Bi :.: Output A) I
bridgedIn = Comp1 (unsafeToVector' [bridgeInOutput, bridgeInOutput2, bridgeInOutput3])

-- Total 2 UTxOs.

bridgeInHash :: HashSimple I
bridgeInHash = (one :: FieldElement I) & hash & Base.hHash

two = one + one

tx1 :: Transaction Ixs Oxs A I
tx1 =
  Transaction
    { inputs = Comp1 (unsafeToVector' [nullOutputRef, OutputRef {orTxId = bridgeInHash, orIndex = two}, nullOutputRef])
    , outputs =
        Comp1
          ( unsafeToVector'
              [ ( Output
                    { oAddress = address2
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset3Policy, assetName = asset3Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            ]
                    }
                )
                  :*: false
              , nullOutput :*: false
              , ( Output
                    { oAddress = address3
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset3Policy, assetName = asset3Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            ]
                    }
                )
                  :*: false
              ]
          )
    }

-- "address2" has 5 ADA and 25 asset2 and 25 asset3.
-- "address3" has 5 ADA and 25 asset2 and 25 asset3.
-- Total 3 UTxOs.
tx1Id = txId tx1 & Base.hHash

tx2 :: Transaction Ixs Oxs A I
tx2 =
  Transaction
    { inputs =
        Comp1
          ( unsafeToVector'
              [OutputRef {orTxId = bridgeInHash, orIndex = zero}, nullOutputRef, OutputRef {orTxId = tx1Id, orIndex = two}]
          )
    , outputs =
        Comp1
          ( unsafeToVector'
              [ ( Output
                    { oAddress = address
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
                            , nullAssetValue
                            ]
                    }
                )
                  :*: false
              , ( Output
                    { oAddress = address2
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (9_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
                            , nullAssetValue
                            ]
                    }
                )
                  :*: false
              , ( Output
                    { oAddress = address3
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (1_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset3Policy, assetName = asset3Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            ]
                    }
                )
                  :*: true
              ]
          )
    }

-- "address" has 5 ADA and 50 asset2.
-- "address2" has one UTxO having 9 ADA and 50 asset2. And other UTxO having 5 ADA and 25 asset2 and 25 asset3.
-- Total 3 UTxOs.
tx2Id = txId tx2 & Base.hHash

tx3 :: Transaction Ixs Oxs A I
tx3 =
  Transaction
    { inputs =
        Comp1
          ( unsafeToVector'
              [ OutputRef {orTxId = tx1Id, orIndex = zero}
              , OutputRef {orTxId = tx2Id, orIndex = zero}
              , OutputRef {orTxId = tx2Id, orIndex = one}
              ]
          )
    , outputs =
        Comp1
          ( unsafeToVector'
              [ ( Output
                    { oAddress = address
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
                            , nullAssetValue
                            ]
                    }
                )
                  :*: true
              , ( Output
                    { oAddress = address2
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (9_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
                            ]
                    }
                )
                  :*: true
              , ( Output
                    { oAddress = address2
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset3Policy, assetName = asset3Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            , nullAssetValue
                            ]
                    }
                )
                  :*: true
              ]
          )
    }

-- Spent all UTxOs and bridged out all created outputs.

batch :: TransactionBatch Ixs Oxs A TxCount I
batch = TransactionBatch {tbTransactions = unsafeToVector' [tx1, tx2, tx3]}

sigs =
  let
    dummyRPoint :*: dummyS = signTransaction tx1 privateKey
    dummyPublicKey = publicKey
    -- Tx1
    rPointTx11 :*: sTx11 = dummyRPoint :*: dummyS
    publicKeyTx11 = dummyPublicKey
    rPointTx12 :*: sTx12 = signTransaction tx1 privateKey3
    publicKeyTx12 = publicKey3
    rPointTx13 :*: sTx13 = dummyRPoint :*: dummyS
    publicKeyTx13 = dummyPublicKey
    -- Tx2
    rPointTx21 :*: sTx21 = signTransaction tx2 privateKey
    publicKeyTx21 = publicKey
    rPointTx22 :*: sTx22 = dummyRPoint :*: dummyS
    publicKeyTx22 = dummyPublicKey
    rPointTx23 :*: sTx23 = signTransaction tx2 privateKey3
    publicKeyTx23 = publicKey3
    -- Tx3
    rPointTx31 :*: sTx31 = signTransaction tx3 privateKey2
    publicKeyTx31 = publicKey2
    rPointTx32 :*: sTx32 = signTransaction tx3 privateKey
    publicKeyTx32 = publicKey
    rPointTx33 :*: sTx33 = signTransaction tx3 privateKey2
    publicKeyTx33 = publicKey2
   in
    Comp1
      ( unsafeToVector'
          [ Comp1
              ( unsafeToVector'
                  [ rPointTx11 :*: sTx11 :*: publicKeyTx11
                  , rPointTx12 :*: sTx12 :*: publicKeyTx12
                  , rPointTx13 :*: sTx13 :*: publicKeyTx13
                  ]
              )
          , Comp1
              ( unsafeToVector'
                  [ rPointTx21 :*: sTx21 :*: publicKeyTx21
                  , rPointTx22 :*: sTx22 :*: publicKeyTx22
                  , rPointTx23 :*: sTx23 :*: publicKeyTx23
                  ]
              )
          , Comp1
              ( unsafeToVector'
                  [ rPointTx31 :*: sTx31 :*: publicKeyTx31
                  , rPointTx32 :*: sTx32 :*: publicKeyTx32
                  , rPointTx33 :*: sTx33 :*: publicKeyTx33
                  ]
              )
          ]
      )

newState :*: witness :*: utxoPreimage2 = updateLedgerState prevState utxoPreimage bridgedIn batch sigs
