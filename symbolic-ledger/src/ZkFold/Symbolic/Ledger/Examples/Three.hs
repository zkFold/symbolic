{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ZkFold.Symbolic.Ledger.Examples.Three (
  prevState,
  batch,
  batch2,
  witness,
  newState,
  utxoPreimage2,
  asset2Policy,
  asset2Name,
  address,
  address2,
  tx1,
  tx2,
  tx3,
  tx4,
  sigs,
  sigs2,
  bridgeInOutput,
  bridgedIn,
  bridgeOutOutput,
  bridgedIn2,
  newState2,
  witness2,
  utxoPreimage3,
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
import GHC.IsList (IsList(..))

type I = RollupBFInterpreter

type Bi = 1

type Bo = 1

type Ud = 2 -- Thus 2 ^ (2 - 1) = 2 leaves

type A = 2

type Ixs = 2

type Oxs = 2

type TxCount = 2

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

publicKey :: PublicKey I
publicKey = privateKey `scale` pointGen @(EdDSAPoint I)

publicKey2 :: PublicKey I
publicKey2 = privateKey2 `scale` pointGen @(EdDSAPoint I)

address = hashFn publicKey

address2 = hashFn publicKey2

{-

λ> import GeniusYield.Test.FakeCoin
λ> asset = fakeCoin (FakeCoin "zk-rollup")
λ> assetValue = valueSingleton asset 1
λ> assetValue
valueFromList [(GYToken "336ffa0fe9a48d341516e1ea7357a62622349fb84fbbeb8c15c972ac" "zk-rollup",1)]
λ> import ZkFold.Cardano.UPLC.RollupSimple.Utils
λ> toSymbolicValue 2 (valueToPlutus assetValue)
[5416991460958575053756211119605256293102838005726979983329238545068,2258225731751461549424,1,0,0,0]

-}
asset2Policy :: AssetPolicy I = fromConstant (5416991460958575053756211119605256293102838005726979983329238545068 :: Natural)

asset2Name :: AssetName I = fromConstant (2258225731751461549424 :: Natural)

bridgeInOutput =
  Output
    { oAddress = address
    , oAssets =
        Comp1 $
          unsafeToVector'
            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (10_000_000 :: Natural)}
            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (50_000_000 :: Natural)}
            ]
    }

-- "address" has 10 ADA and 50 asset2

-- We bridge in an output and refer to it in transaction.
bridgedIn :: (Vector Bi :.: Output A) I
bridgedIn = Comp1 (unsafeToVector' [bridgeInOutput])

-- Total 1 UTxOs.

bridgeInHash :: HashSimple I
bridgeInHash = (one :: FieldElement I) & hash & Base.hHash

tx1 :: Transaction Ixs Oxs A I
tx1 =
  Transaction
    { inputs = Comp1 (unsafeToVector' [OutputRef {orTxId = bridgeInHash, orIndex = zero}, nullOutputRef])
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
                            ]
                    }
                )
                  :*: false
              , ( Output
                    { oAddress = address
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            ]
                    }
                )
                  :*: false
              ]
          )
    }

-- "address" has 5 ADA and 25 asset2.
-- "address2" has 5 ADA and 25 asset2.
-- Total 2 UTxOs.
tx1Id = txId tx1 & Base.hHash

tx2 :: Transaction Ixs Oxs A I
tx2 =
  Transaction
    { inputs =
        Comp1
          ( unsafeToVector'
              [OutputRef {orTxId = tx1Id, orIndex = zero}, OutputRef {orTxId = tx1Id, orIndex = one}]
          )
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
                            ]
                    }
                )
                  :*: false
              , ( Output
                    { oAddress = address
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            ]
                    }
                )
                  :*: false
              ]
          )
    }

-- "address" has 5 ADA and 25 asset2.
-- "address2" has 5 ADA and 25 asset2.

-- Cardano address @addr_test1qpxsldf6hmp5vtdhhwzukm8x5q0m9t2xh8cftx8s6a43vll3t8hyc5syfx9lltq9dgr2xdkvwahr9humhpa9tae2jcjsxpxw2h@ (picked randomly from explorer), maps to @4d0fb53abec3462db7bb85cb6ce6a01fb2ad46b9f09598f0d76b167f@ payment credential and @f159ee4c5204498bffac056a06a336cc776e32df9bb87a55f72a9625@ staking credential. And this concatenated hex string (where we concatenate payment credential and staking credential) maps to following base field given the way we convert hex strings to base fields in our rollup plutus validator.
bridgeOutOutput =
  Output
    { oAddress = fromConstant (19889081452670861588114349990778949346404544631803352712004893411981264611445 :: Natural)
    , oAssets =
        Comp1 $
          unsafeToVector'
            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 :: Natural)}
            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
            ]
    }

tx2Id = txId tx2 & Base.hHash

tx3 :: Transaction Ixs Oxs A I
tx3 =
  Transaction
    { inputs =
        Comp1
          ( unsafeToVector'
              [OutputRef {orTxId = tx2Id, orIndex = zero}, OutputRef {orTxId = tx2Id, orIndex = one}]
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
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (25_000_000 :: Natural)}
                            ]
                    }
                )
                  :*: false
              , bridgeOutOutput
                  :*: true
              ]
          )
    }

-- "address" has 5 ADA and 25 asset2.
-- bridge-out output of 5 ADA and 25 asset2.
-- Total 1 UTxOs.

tx3Id = txId tx3 & Base.hHash

tx4 :: Transaction Ixs Oxs A I
tx4 =
  Transaction
    { inputs =
        Comp1
          ( unsafeToVector'
              [OutputRef {orTxId = tx3Id, orIndex = zero}, nullOutputRef]
          )
    , outputs =
        Comp1
          ( unsafeToVector'
              [ ( Output
                    { oAddress = address2
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (2_500_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (12_500_000 :: Natural)}
                            ]
                    }
                )
                  :*: false
              , ( Output
                    { oAddress = address
                    , oAssets =
                        Comp1 $
                          unsafeToVector'
                            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (2_500_000 :: Natural)}
                            , AssetValue {assetPolicy = asset2Policy, assetName = asset2Name, assetQuantity = fromConstant (12_500_000 :: Natural)}
                            ]
                    }
                )
                  :*: false
              ]
          )
    }

-- "address" has 2.5 ADA and 12.5 asset2.
-- "address2" has 2.5 ADA and 12.5 asset2.

batch :: TransactionBatch Ixs Oxs A TxCount I
batch = TransactionBatch {tbTransactions = unsafeToVector' [tx1, tx2]}

batch2 :: TransactionBatch Ixs Oxs A TxCount I
batch2 = TransactionBatch {tbTransactions = unsafeToVector' [tx3, tx4]}

sigs =
  let
    dummyRPoint :*: dummyS = signTransaction tx1 privateKey
    dummyPublicKey = publicKey
    -- Tx1
    rPointTx11 :*: sTx11 = dummyRPoint :*: dummyS
    publicKeyTx11 = dummyPublicKey
    rPointTx12 :*: sTx12 = dummyRPoint :*: dummyS
    publicKeyTx12 = dummyPublicKey
    -- Tx2
    rPointTx21 :*: sTx21 = signTransaction tx2 privateKey2
    publicKeyTx21 = publicKey2
    rPointTx22 :*: sTx22 = signTransaction tx2 privateKey
    publicKeyTx22 = publicKey
   in
    Comp1
      ( unsafeToVector'
          [ Comp1
              ( unsafeToVector'
                  [ rPointTx11 :*: sTx11 :*: publicKeyTx11
                  , rPointTx12 :*: sTx12 :*: publicKeyTx12
                  ]
              )
          , Comp1
              ( unsafeToVector'
                  [ rPointTx21 :*: sTx21 :*: publicKeyTx21
                  , rPointTx22 :*: sTx22 :*: publicKeyTx22
                  ]
              )
          ]
      )
      
sigs2 =
  let
    dummyRPoint :*: dummyS = signTransaction tx4 privateKey
    dummyPublicKey = publicKey
    -- Tx3
    rPointTx11 :*: sTx11 = signTransaction tx3 privateKey2
    publicKeyTx11 = publicKey2
    rPointTx12 :*: sTx12 = signTransaction tx3 privateKey
    publicKeyTx12 = publicKey
    -- Tx4
    rPointTx21 :*: sTx21 = dummyRPoint :*: dummyS
    publicKeyTx21 = dummyPublicKey
    rPointTx22 :*: sTx22 = dummyRPoint :*: dummyS
    publicKeyTx22 = dummyPublicKey
   in
    Comp1
      ( unsafeToVector'
          [ Comp1
              ( unsafeToVector'
                  [ rPointTx11 :*: sTx11 :*: publicKeyTx11
                  , rPointTx12 :*: sTx12 :*: publicKeyTx12
                  ]
              )
          , Comp1
              ( unsafeToVector'
                  [ rPointTx21 :*: sTx21 :*: publicKeyTx21
                  , rPointTx22 :*: sTx22 :*: publicKeyTx22
                  ]
              )
          ]
      )

newState :*: witness :*: utxoPreimage2 = updateLedgerState prevState utxoPreimage bridgedIn batch sigs

bridgedIn2 :: (Vector Bi :.: Output A) I
bridgedIn2 = Comp1 (fromList [nullOutput @A @I])

newState2 :*: witness2 :*: utxoPreimage3 = updateLedgerState newState (unComp1 utxoPreimage2) bridgedIn2 batch2 sigs2
