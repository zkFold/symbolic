#!/bin/bash

cabal clean

# Compile to wasm
cabal --with-compiler=wasm32-wasi-ghc-9.13 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.13 --with-hsc2hs=wasm32-wasi-hsc2hs-9.13 build

# Create a library for JS FFI
$(wasm32-wasi-ghc-9.13 --print-libdir)/post-link.mjs -i a.out -o ghc_wasm_jsffi.js

# Fix function call
sed -i -e 's/blake2b/blake2b($1,$2,$3,$4,$5)/g' ghc_wasm_jsffi.js

# Import blake2b
sed -i "1iimport { blake2b } from './blake2b.js'" ghc_wasm_jsffi.js

mv ghc_wasm_jsffi.js wasm/
mv a.out wasm/proof.wasm
