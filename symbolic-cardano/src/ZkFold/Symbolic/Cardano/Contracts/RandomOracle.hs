{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Cardano.Contracts.RandomOracle where

import GHC.Generics ((:*:) (..))
import Numeric.Natural (Natural)
import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Data.Vector ((!!))
import ZkFold.Symbolic.Algorithm.Hash.MiMC (hash)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.Bool (BoolType (..))
import ZkFold.Symbolic.Data.ByteString (feToBSbe, resize)
import ZkFold.Symbolic.Data.UInt (KnownUInt)
import Prelude hiding (
  Bool,
  Eq (..),
  all,
  length,
  maybe,
  splitAt,
  zip,
  (!!),
  (&&),
  (*),
  (+),
  (==),
 )

import ZkFold.Symbolic.Cardano.Types

type Tokens = 2

type TxOut context = Output Tokens () context

type TxIn context = Input Tokens () context

type Tx context = Transaction 1 0 2 Tokens 1 () context

randomOracle
  :: forall context
   . (Symbolic context, KnownUInt 64 context, NumberOfBits context ~ 256)
  => Natural -> Tx context -> FieldElement context -> Bool context
randomOracle c tx w =
  let
    -- The secret key is correct
    conditionSecretKey = fromConstant c == hash w

    -- Extracting information about the transaction
    seed = hash @context $ txiOutputRef $ txInputs tx !! 0
    Value vs = txoTokens $ txOutputs tx !! 0
    SingleAsset p name n = vs !! 1
    SingleAsset policyId _ _ = getValue (txMint tx) !! 0

    -- Computing the random number
    r = hash @context (w :*: seed)

    -- The token's policy is correct
    conditionPolicyId = p == policyId

    -- The token's name is correct
    conditionTokenName = name == resize (feToBSbe r)

    -- The token's quantity is correct
    conditionQuantity = n == one
   in
    conditionSecretKey && conditionPolicyId && conditionTokenName && conditionQuantity
