{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Cardano.Contracts.BatchTransfer where

import Data.Maybe (fromJust)
import Data.Zip (zip)
import GHC.Generics ((:*:) (..))
import Numeric.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Data.Vector (Vector, fromVector, toVector)
import ZkFold.Symbolic.Algorithm.Hash.MiMC
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (BoolType (..), all)
import ZkFold.Symbolic.Data.ByteString (resize)
import ZkFold.Symbolic.Data.UInt
import Prelude hiding (Bool, Eq (..), all, length, splitAt, zip, (&&), (*), (+))

import ZkFold.Symbolic.Cardano.Types

type Tokens = 10

type TxOut = Output Tokens ()

type TxIn = Input Tokens ()

type Tx = Transaction 6 0 11 Tokens 0 ()

verifySignature
  :: forall context
   . (Symbolic context, KnownUInt 256 context)
  => ByteString 224 context
  -> (TxOut context, TxOut context)
  -> ByteString 256 context
  -> Bool context
verifySignature pub (pay, change) sig =
  beBSToUInt sig * base == strictFEToUInt mimc * beBSToUInt (resize @_ @_ @256 pub)
 where
  base :: UInt 256 context
  base = fromConstant (15112221349535400772501151409588531511454012693041857206046113283949847762202 :: Natural)

  mimc :: FieldElement context
  mimc = hash (pay :*: change)

batchTransfer
  :: forall context
   . ( Symbolic context
     , KnownUInt 256 context
     )
  => Tx context
  -> Vector 5 (TxOut context, TxOut context, ByteString 256 context)
  -> Bool context
batchTransfer tx transfers =
  let
    -- Extract the payment credentials and verify the signatures
    pkhs = fromJust $ toVector @5 $ map (paymentCredential . txoAddress . txiOutput) $ init $ fromVector $ txInputs tx
    condition1 = all (\(pkh, (payment, change, signature)) -> verifySignature pkh (payment, change) signature) $ zip pkhs transfers
    outputs = zip [0 ..] . init . fromVector $ txOutputs tx

    -- Extract the payments from the transaction and validate them
    payments = fromJust $ toVector @5 $ map snd $ filter (\(i, _) -> even @Integer i) outputs

    condition2 = all (\(p', (p, _, _)) -> p' == p) $ zip payments transfers

    -- Extract the changes from the transaction and validate them
    changes = fromJust $ toVector @5 $ map snd $ filter (\(i, _) -> odd @Integer i) outputs
    condition3 = all (\(c', (_, c, _)) -> c' == c) $ zip changes transfers
   in
    condition1 && condition2 && condition3
