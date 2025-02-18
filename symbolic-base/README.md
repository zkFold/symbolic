# Symbolic base package
This package contains the Symbolic framework base library. It includes the Symbolic compiler and a number of zero knowledge proof protocols. The compiler translates code written using the zkFold Symbolic framework into arithmetic circuits. The zero knowledge proof protocols are used to verify statements about the circuits.

# Tests
To run the tests, execute
```bash
cabal test
```

To disable slow test groups, execute
```bash
cabal test --test-option="--skip=SLOW"
```

# Compile in JS

```bash
cabal build symbolic-base --with-hsc2hs=javascript-unknown-ghcjs-hsc2hs --with-ghc=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg
```