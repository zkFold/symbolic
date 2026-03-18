Produce a serialised circuit in Haskell (for Fibonacci circuit from the examples):

```Haskell
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.Algebra.Class
import GHC.Generics
import ZkFold.Symbolic.Data.Vec (Vec (..), runVec)
import ZkFold.Algebra.EllipticCurve.BLS12_381
import qualified ZkFold.Symbolic.Compiler as C
import ZkFold.Protocol.Halo2.Export
import ZkFold.Symbolic.Examples.Fibonacci

fib = exampleFibonacciMod 10
ac = runVec $ C.compile @Fr $ fib
ir = exportHalo2Ir @_ @_ @2048 @_ @(PolyVec Fr) ac ((U1 :*: U1) :*: (Par1 (fromConstant 8) :*: U1))
writeHalo2IrFile "circuit.json" ir
```

Compile the Rust bridge:

```bash
cargo build --release
```

Run the Rust bridge executable:

```bash
./symbolic-halo2-bridge prove symbolic_circuit.json
./symbolic-halo2-bridge verify symbolic_circuit.json symbolic_circuit.proof.hex
./symbolic-halo2-bridge gen-plinth symbolic_circuit.json
./symbolic-halo2-bridge gen-aiken symbolic_circuit.json
```
