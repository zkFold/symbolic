{-# LANGUAGE ExistentialQuantification #-}

module ZkFold.Symbolic.UPLC.Converter where

import           Data.Function                               (($))
import           GHC.Generics                                (Par1)

import           ZkFold.Base.Algebra.Basic.Field             (Zp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_Scalar)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit, compile)
import           ZkFold.Symbolic.Data.Bool                   (Bool)
import           ZkFold.Symbolic.Data.Maybe                  (isJust)
import           ZkFold.Symbolic.UPLC.Data                   (Data)
import           ZkFold.Symbolic.UPLC.Evaluation             (ExValue (ExValue), MaybeValue (..), Sym, eval)
import           ZkFold.UPLC.Term                            (Term)

-- | Different script types used on Cardano network.
data ScriptType
  -- | Plutus V1/V2 spending scripts.
  = SpendingV1
  -- | Plutus V1/V2 minting, certifying and rewarding scripts.
  | OtherV1
  -- | Plutus V3 scripts.
  | V3

-- | Result of a UPLC Converter is a circuit:
-- 1. with arbitrary input;
-- 2. with a single success/fail output;
-- 3. over BLS base field.
data SomeCircuit = forall p i. SomeCircuit (ArithmeticCircuit (Zp BLS12_381_Scalar) p i Par1)

-- | Converter itself.
convert ::
  Term -> -- ^ Term to convert.
  ScriptType -> -- ^ Type of a script.
  SomeCircuit -- ^ Result of a conversion.
convert t SpendingV1 = SomeCircuit $ compile (spendingContract t)
convert t OtherV1    = SomeCircuit $ compile (otherContract t)
convert t V3         = SomeCircuit $ compile (contractV3 t)

-- | Datum context.
type DatumCtx c = Data c

-- | Redeemer context.
type RedeemerCtx c = Data c

-- | Script context.
type ScriptCtx c = Data c

-- | As per [Plutus User Guide](https://plutus.cardano.intersectmbo.org/docs/working-with-scripts/ledger-language-version),
-- A v1/v2 spending script takes a datum context, a redeemer context
-- and a script context as input.
spendingContract :: Sym c => Term -> DatumCtx c -> RedeemerCtx c -> ScriptCtx c -> Bool c
spendingContract t d r s = contract t [ExValue d, ExValue r, ExValue s]

-- | As per [Plutus User Guide](https://plutus.cardano.intersectmbo.org/docs/working-with-scripts/ledger-language-version),
-- A v1/v2 minting, certifying and rewarding scripts each take
-- a redeemer context and a script context as input.
otherContract :: Sym c => Term -> RedeemerCtx c -> ScriptCtx c -> Bool c
otherContract t r s = contract t [ExValue r, ExValue s]

-- | As per [Plutus User Guide](https://plutus.cardano.intersectmbo.org/docs/working-with-scripts/ledger-language-version),
-- A v3 script takes a single script context as input.
contractV3 :: Sym c => Term -> ScriptCtx c -> Bool c
contractV3 t s = contract t [ExValue s]

-- | A generic script function.
contract :: Sym c => Term -> [ExValue c] -> Bool c
contract t s = case eval t s of
  MaybeValue v -> isJust v
