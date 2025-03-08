{-# LANGUAGE TypeOperators #-}

module ZkFold.Symbolic.Ledger.Types (
    module ZkFold.Symbolic.Ledger.Types.Address,
    module ZkFold.Symbolic.Ledger.Types.Contract,
    module ZkFold.Symbolic.Ledger.Types.Hash,
    module ZkFold.Symbolic.Ledger.Types.Input,
    module ZkFold.Symbolic.Ledger.Types.Output,
    module ZkFold.Symbolic.Ledger.Types.OutputRef,
    module ZkFold.Symbolic.Ledger.Types.Transaction,
    module ZkFold.Symbolic.Ledger.Types.Update,
    module ZkFold.Symbolic.Ledger.Types.Value,
    Signature
) where

-- Re-exports
import           Control.Applicative                      (Applicative)
import           Data.Foldable                            (Foldable)
import           Data.Functor                             (Functor)
import           Data.Functor.Rep                         (Representable)
import           Data.Proxy                               (Proxy)
import           Data.Traversable                         (Traversable)
import           Data.Zip                                 (Zip)
import           Prelude                                  (type (~))

import           ZkFold.Base.Algebra.Basic.Class          (AdditiveMonoid, MultiplicativeMonoid)
import           ZkFold.Symbolic.Data.Bool                (Bool)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators         (KnownRegisters, RegisterSize (Auto))
import           ZkFold.Symbolic.Data.Conditional         (Conditional)
import           ZkFold.Symbolic.Data.Eq                  (SymbolicEq)
import           ZkFold.Symbolic.Data.List                (List)
import           ZkFold.Symbolic.Data.UInt                (UInt)
import           ZkFold.Symbolic.Fold                     (SymbolicFold)
import           ZkFold.Symbolic.Ledger.Types.Address
import           ZkFold.Symbolic.Ledger.Types.Contract
import           ZkFold.Symbolic.Ledger.Types.Hash
import           ZkFold.Symbolic.Ledger.Types.Input
import           ZkFold.Symbolic.Ledger.Types.Output
import           ZkFold.Symbolic.Ledger.Types.OutputRef
import           ZkFold.Symbolic.Ledger.Types.Transaction
import           ZkFold.Symbolic.Ledger.Types.Update
import           ZkFold.Symbolic.Ledger.Types.Value

{-
    zkFold's ledger is a UTXO-based ledger. The architecture of the ledger is mostly similar to the Cardano ledger with some key differences:

    - Some transaction data is private and is kept off-chain by the concerned parties.

    - All UTXOs are locked by contracts.

    - Stake delegation and governance is implemented through contracts.
-}

type Signature context =
    ( SymbolicFold context
    , KnownRegisters context 32 Auto
    , AdditiveMonoid (UInt 32 Auto context)
    , AdditiveMonoid (Value context)
    , Conditional (Bool context) (Update context)
    , SymbolicEq (Hash context)
    , SymbolicEq (Input context)
    , SymbolicEq (Address context, Datum context)
    , SymbolicEq (CurrencySymbol context, Token context)
    , SymbolicEq (Update context)
    , SymbolicEq (Value context)
    , SymbolicEq (List context (Hash context))
    , SymbolicEq (List context (Address context, Datum context))
    , SymbolicEq (List context (ContractId context, Token context))
    , SymbolicEq (List context (Input context))
    , SymbolicEq (Output context)
    , SymbolicEq (List context (Transaction context))
    , SymbolicEq (List context (List context (TransactionId context)))
    , Foldable (List context)
    , Functor (List context)
    , MultiplicativeMonoid (UInt 32 Auto context)
    , Zip (List context)

    -- TODO: The instances below are most likely NOT NEEDED.
    -- They can be removed after properly declaring List, TransactionId, Input, Update and Hash
    -- instances of SymbolicData

    , Context (List context (TransactionId context)) ~ context
    , Support (List context (TransactionId context)) ~ Proxy context
    , Applicative (Layout (List context (TransactionId context)))
    , Traversable (Layout (List context (TransactionId context)))
    , Representable (Layout (List context (TransactionId context)))
    , SymbolicData (List context (TransactionId context))
    , Zip (Layout (List context (TransactionId context)))

    , Context (Input context) ~ context
    , Support (Input context) ~ Proxy context
    , SymbolicData (Input context)
    , Applicative (Layout (Input context))
    , Traversable (Layout (Input context))
    , Representable (Layout (Input context))
    , Zip (Layout (Input context))
    , Representable (Payload (Input context))

    , Context (Hash context) ~ context
    , Support (Hash context) ~ Proxy context
    , SymbolicData (Hash context)
    , Applicative (Layout (Hash context))
    , Traversable (Layout (Hash context))
    , Representable (Layout (Hash context))
    , Zip (Layout (Hash context))
    , Representable (Payload (Hash context))

    , SymbolicOutput (Transaction context)
    , Context (Transaction context) ~ context

    , SymbolicOutput (Update context)
    , Context (Update context) ~ context

    , SymbolicOutput (AddressIndex context)
    , Context (AddressIndex context) ~ context

    , SymbolicOutput (Output context)
    , Context (Output context) ~ context
    )
