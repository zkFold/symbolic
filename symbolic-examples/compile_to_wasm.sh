#!/bin/bash

cabal clean

# Compile to wasm
cabal --with-compiler=wasm32-wasi-ghc-9.12 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.12 --with-hsc2hs=wasm32-wasi-hsc2hs-9.12 build

# Create a library for JS FFI
$(wasm32-wasi-ghc-9.12 --print-libdir)/post-link.mjs -i a.out -o ghc_wasm_jsffi.js

# Import blake2b
sed -i "1iimport { blake2b } from './blake2b.js'" ghc_wasm_jsffi.js

mv ghc_wasm_jsffi.js wasm/
mv a.out wasm/proof.wasm
