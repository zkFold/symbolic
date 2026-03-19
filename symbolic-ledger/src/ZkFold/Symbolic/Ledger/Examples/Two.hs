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
  S,
  N,
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

type Ud = 10 -- Thus 2 ^ (10 - 1) = 512 leaves

type A = 3

type S = 3

type N = 3

type TxCount = 3

emptyTree :: SymMerkle.MerkleTree Ud I
emptyTree = SymMerkle.fromLeaves (pure (nullUTxOHash @A @I))

prevState :: State Ud A I
prevState =
  State
    { sPreviousStateHash = zero
    , sUTxO = SymMerkle.mHash emptyTree
    , sLength = zero
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

tx1 :: Transaction N A I
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

tx2 :: Transaction N A I
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

tx3 :: Transaction N A I
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

batch :: TransactionBatch N A TxCount I
batch = TransactionBatch {tbTransactions = unsafeToVector' [tx1, tx2, tx3]}

sigs :: (Vector TxCount :.: (Vector S :.: (PublicKey :*: EdDSAPoint :*: EdDSAScalarField))) I
sigs =
  let
    -- Tx1: only input[1] is non-null (address3). Need valid sigs for tx1.
    -- Signer slots: pk3 (needed), pk1 (padding), pk1 (padding)
    rPointTx11 :*: sTx11 = signTransaction tx1 privateKey
    publicKeyTx11 = publicKey
    rPointTx12 :*: sTx12 = signTransaction tx1 privateKey3
    publicKeyTx12 = publicKey3
    rPointTx13 :*: sTx13 = signTransaction tx1 privateKey
    publicKeyTx13 = publicKey
    -- Tx2: input[0] is address (pk1), input[2] is address3 (pk3). Need valid sigs for tx2.
    rPointTx21 :*: sTx21 = signTransaction tx2 privateKey
    publicKeyTx21 = publicKey
    rPointTx22 :*: sTx22 = signTransaction tx2 privateKey
    publicKeyTx22 = publicKey
    rPointTx23 :*: sTx23 = signTransaction tx2 privateKey3
    publicKeyTx23 = publicKey3
    -- Tx3: input[0] is address2 (pk2), input[1] is address (pk1), input[2] is address2 (pk2).
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
                  [ publicKeyTx11 :*: rPointTx11 :*: sTx11
                  , publicKeyTx12 :*: rPointTx12 :*: sTx12
                  , publicKeyTx13 :*: rPointTx13 :*: sTx13
                  ]
              )
          , Comp1
              ( unsafeToVector'
                  [ publicKeyTx21 :*: rPointTx21 :*: sTx21
                  , publicKeyTx22 :*: rPointTx22 :*: sTx22
                  , publicKeyTx23 :*: rPointTx23 :*: sTx23
                  ]
              )
          , Comp1
              ( unsafeToVector'
                  [ publicKeyTx31 :*: rPointTx31 :*: sTx31
                  , publicKeyTx32 :*: rPointTx32 :*: sTx32
                  , publicKeyTx33 :*: rPointTx33 :*: sTx33
                  ]
              )
          ]
      )

newState :*: witness :*: _utxoTree2 :*: utxoPreimage2 = updateLedgerState @Bi @Bo prevState emptyTree utxoPreimage bridgedIn batch sigs
