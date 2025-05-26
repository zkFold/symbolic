{-# LANGUAGE CPP #-}

module ZkFold.Protocol.NonInteractiveProof (
    module ZkFold.Protocol.NonInteractiveProof.Class,
    module ZkFold.Protocol.NonInteractiveProof.Testing,
) where

import           ZkFold.Protocol.NonInteractiveProof.Class
#if defined(javascript_HOST_ARCH)
import           ZkFold.Protocol.NonInteractiveProof.JS () 
#elif defined(wasm32_HOST_ARCH)
import           ZkFold.Protocol.NonInteractiveProof.WASM () 
#else
import           ZkFold.Protocol.NonInteractiveProof.Haskell () 
#endif
import           ZkFold.Protocol.NonInteractiveProof.Testing
