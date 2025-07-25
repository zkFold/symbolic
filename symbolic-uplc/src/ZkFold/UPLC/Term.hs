{-# LANGUAGE LambdaCase #-}

module ZkFold.UPLC.Term where

import Control.Applicative ((<*>))
import Control.Monad (fail, fmap, return, (>>=))
import Data.Function ((.))
import Data.Functor ((<$>))
import Flat qualified
import Flat.Decoder qualified as Flat
import Numeric.Natural (Natural)
import Prelude (fromIntegral)

import ZkFold.UPLC.BuiltinFunction
import ZkFold.UPLC.BuiltinType
import ZkFold.UPLC.Constant (Constant, getConstant)
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

-- | A decoder of a 'Term', according to
-- [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Jul 2025).
getTerm :: Flat.Get Term
getTerm =
  Flat.dBEBits8 4 >>= \case
    0b0000 -> TVariable <$> Flat.decode
    0b0001 -> TDelay <$> getTerm
    0b0010 -> TLam <$> getTerm
    0b0011 -> TApp <$> getTerm <*> getTerm
    0b0100 -> getType >>= demoted (fmap TConstant . getConstant)
    0b0101 -> TForce <$> getTerm
    0b0110 -> return TError
    0b0111 -> getBuiltinFunction (return . TBuiltin)
    0b1000 -> TConstr <$> fmap (fromIntegral @Natural) Flat.decode <*> Flat.decodeListWith getTerm
    0b1001 -> TCase <$> getTerm <*> Flat.decodeListWith getTerm
    _ -> fail "unknown term tag"

-- | Encoding of a UPLC program as a Haskell datatype, according to
-- [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Jul 2025).
data VersionedProgram = Program (Natural, Natural, Natural) Term

-- | A decoder of a 'VersionedProgram', according to
-- [Plutus Core Spec](https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf)
-- (accessed in Jul 2025).
getProgram :: Flat.Get VersionedProgram
getProgram = Program <$> Flat.decode <*> getTerm
