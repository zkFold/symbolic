module ZkFold.Symbolic.Ledger.Types.Field (
  RollupBF,
  RollupBFInterpreter,
) where
import ZkFold.Algebra.EllipticCurve.Jubjub (Fq)
import ZkFold.Symbolic.Interpreter (Interpreter)

type RollupBF = Fq

type RollupBFInterpreter = Interpreter RollupBF