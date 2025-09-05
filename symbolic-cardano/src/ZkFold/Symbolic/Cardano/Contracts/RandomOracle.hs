{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Cardano.Contracts.RandomOracle where

import GHC.Generics ((:*:) (..))
import ZkFold.Algebra.Class
import ZkFold.Data.Eq
import ZkFold.Data.Vector (Vector, (!!))
import ZkFold.Symbolic.Algorithm.Hash.MiMC (hash)
import ZkFold.Symbolic.Class (Symbolic (BaseField))
import ZkFold.Symbolic.Data.Bool (BoolType (..))
import qualified ZkFold.Symbolic.Data.ByteString as Symbolic
import ZkFold.Symbolic.Data.Combinators
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
   . ( Symbolic context
     , Bits (FieldElement context) ~ context (Vector 256)
     )
  => BaseField context -> Tx context -> FieldElement context -> Bool context
randomOracle c tx w =
  let
    -- The secret key is correct
    conditionSecretKey = fromConstant c == hash w

    -- Extracting information about the transaction
    seed = hash $ txiOutputRef $ txInputs tx !! 0
    Value vs = txoTokens $ txOutputs tx !! 0
    SingleAsset p name n = vs !! 1
    SingleAsset policyId _ _ = getValue (txMint tx) !! 0

    -- Computing the random number
    r = hash (w :*: seed)

    -- The token's policy is correct
    conditionPolicyId = p == policyId

    -- The token's name is correct
    conditionTokenName = name == resize (Symbolic.ByteString $ binaryExpansion r)

    -- The token's quantity is correct
    conditionQuantity = n == one
   in
    conditionSecretKey && conditionPolicyId && conditionTokenName && conditionQuantity
