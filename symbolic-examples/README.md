# Symbolic Code Examples package
This package contains the examples generated using the Symbolic framework. It also contains a benchmark for compiling the examples into arithmetic circuits and evaluating those circuits on a set of inputs.

## Compiling to WASM

This library can be compiled into Web assembly and used in a browser.

You'll need to install GHC wasm backend first. See [full article](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) for more details.

The GHCup approach was tested and confirmed to work with some extra steps:

```bash
export CONFIGURE_ARGS="--host=x86_64-linux --with-intree-gmp --with-system-libffi"
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | SKIP_GHC=1 sh
source ~/.ghc-wasm/env
ghcup config add-release-channel https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml
ghcup install ghc wasm32-wasi-9.12 -- $CONFIGURE_ARGS
```

After that, the library can be compiled with

```bash
cabal --with-compiler=wasm32-wasi-ghc-9.12 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.12 --with-hsc2hs=wasm32-wasi-hsc2hs-9.12 build
```

The compiler will output a file `a.out` in the root directory. This is the WASM module that can be imported and used in a browser.
It currently exports two functions, [mkProofBytesWasm](src/ZkFold/Symbolic/Cardano/Contracts/SmartWallet.hs#L375) and [mkProofBytesMockWasm](src/ZkFold/Symbolic/Cardano/Contracts/SmartWallet.hs#L378), which produce Plonkup proofs that the user possesses a valid JSON Web Token issued by Google.

The library uses JS FFI, so one last step is required to generate a helper file:

```bash
$(wasm32-wasi-ghc-9.12 --print-libdir)/post-link.mjs -i a.out -o ghc_wasm_jsffi.js
```

## Testing

The whole pipeline can be run with

```bash
./compile_to_wasm.sh
cd wasm
deno index.mjs
```

This should print ProofBytes for some random input in `index.mjs`.


Further reading:

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html

https://finley.dev/blog/2024-08-24-ghc-wasm.html

