module ZkFold.Symbolic.Ledger.Validation.Contract where

import           Prelude                      hiding (Bool, Eq, length, splitAt, (&&), (*), (+), (==))

import           ZkFold.Symbolic.Data.Bool    (Bool, (&&))
import           ZkFold.Symbolic.Data.Eq      ((==))
import           ZkFold.Symbolic.Ledger.Types

-- | Witness data for a spending contract.
data SpendingContractWitness context where
    SpendingContractWitness :: (SpendingContract (Transaction context) w context, Datum context, w) -> SpendingContractWitness context

-- | Witness data for a minting contract.
data MintingContractWitness context where
    MintingContractWitness :: (MintingContract (Transaction context) w context, Token context, w) -> MintingContractWitness context

-- | Checks if a contract is satisfied.
spendingContractIsSatisfied ::
       Signature context
    => Transaction context
    -> Address context
    -> Datum context
    -> SpendingContractWitness context
    -> Bool context
spendingContractIsSatisfied tx addr d (SpendingContractWitness (contract, datum, witness)) =
        (addr, d) == (contractId contract, datum)
    &&  contract tx datum witness

-- | Checks if a minting contract is satisfied.
mintingContractIsSatisfied ::
       Signature context
    => Transaction context
    -> CurrencySymbol context
    -> Token context
    -> MintingContractWitness context
    -> Bool context
mintingContractIsSatisfied tx s t (MintingContractWitness (contract, token, witness)) =
        (s, t) == (contractId contract, token)
    &&  contract tx token witness
