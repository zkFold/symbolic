module ZkFold.UPLC.Term where

import Numeric.Natural (Natural)

import ZkFold.UPLC.BuiltinFunction
import ZkFold.UPLC.Constant (Constant)
import ZkFold.UPLC.Data (ConstructorTag)

-- | Variables in UPLC terms.
--
-- While theoretically unspecified, binary representation of terms on Cardano
-- uses [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index), as said in [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
-- They also are most convenient for Converter, so we use them, too.
type DeBruijnIndex = Natural

-- | Terms of Plutus Core as a Haskell datatype.
-- According to [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf).
data Term
  = TVariable !DeBruijnIndex
  | forall t. TConstant !(Constant t)
  | forall s t. TBuiltin !(BuiltinFunction s t)
  | TLam {- DeBruijnIndex = 0 -} Term
  | TApp Term Term
  | TDelay Term
  | TForce Term
  | TConstr !ConstructorTag [Term]
  | TCase Term [Term]
  | TError

-- | Encoding of a UPLC program as a Haskell datatype, according to
-- [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Jul 2025).
data VersionedProgram = Program (Natural, Natural, Natural) Term
