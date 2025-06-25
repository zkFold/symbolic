{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Symbolic.Cardano.Contracts.BatchTransfer where

import Data.Maybe (fromJust)
import Data.Zip (zip)
import Numeric.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Data.Vector (Vector, fromVector, toVector)
import ZkFold.Symbolic.Algorithm.Hash.MiMC
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (BoolType (..), all)
import ZkFold.Symbolic.Data.Class (SymbolicData (..))
import ZkFold.Symbolic.Data.Combinators
import ZkFold.Symbolic.Data.Eq
import ZkFold.Symbolic.Data.FieldElement (fromFieldElement)
import ZkFold.Symbolic.Data.Input (SymbolicInput)
import ZkFold.Symbolic.Data.UInt (StrictConv (..))
import Prelude hiding (Bool, Eq (..), all, length, splitAt, zip, (&&), (*), (+))

import ZkFold.Symbolic.Cardano.Types

type Tokens = 10

type TxOut context = Output Tokens () context

type TxIn context = Input Tokens () context

type Tx context = Transaction 6 0 11 Tokens 0 () context

verifySignature
  :: forall context
   . ( Symbolic context
     , SymbolicData (TxOut context)
     , KnownRegisters context 256 'Auto
     )
  => ByteString 224 context -> (TxOut context, TxOut context) -> ByteString 256 context -> Bool context
verifySignature pub (pay, change) sig = (from sig * base) == (strictConv (fromFieldElement mimc) * from (resize pub :: ByteString 256 context))
 where
  base :: UInt 256 Auto context
  base = fromConstant (15112221349535400772501151409588531511454012693041857206046113283949847762202 :: Natural)

  mimc :: FieldElement context
  mimc = hash (pay, change)

batchTransfer
  :: forall context
   . ( Symbolic context
     , SymbolicInput (TxOut context)
     , KnownRegisters context 256 'Auto
     , KnownRegisters context 64 'Auto
     )
  => Tx context -> Vector 5 (TxOut context, TxOut context, ByteString 256 context) -> Bool context
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
