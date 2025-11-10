module ZkFold.Symbolic.Ledger.Types.Field (
  RollupBF,
  RollupBFInterpreter,
) where

import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.Symbolic.Interpreter (Interpreter)

-- | Base field used for the rollup ledger.
type RollupBF = Fq

-- | Interpreter for the rollup base field.
type RollupBFInterpreter = Interpreter RollupBF
