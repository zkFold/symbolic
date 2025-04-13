{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Base.Protocol.NonInteractiveProof.Testing where

import           Prelude

import           ZkFold.Base.Protocol.NonInteractiveProof.Internal

class (NonInteractiveProof a, NonInteractiveProof b) => CompatibleNonInteractiveProofs a b where
    nipSetupTransform    :: SetupVerify a -> SetupVerify b
    nipInputTransform    :: SetupVerify b -> Input a -> Input b
    nipProofTransform    :: SetupVerify b -> Proof a -> Proof b

nipCompatibility :: forall a b . CompatibleNonInteractiveProofs a b
    => a -> Witness a -> Bool
nipCompatibility a w =
    let (i, p) = prove @a (setupProve @a a) w
        s'     = nipSetupTransform @a @b (setupVerify @a a)
        i'     = nipInputTransform @a @b s' i
        p'     = nipProofTransform @a @b s' p
    in verify @b s' i' p'

instance NonInteractiveProof a => CompatibleNonInteractiveProofs a a where
    nipSetupTransform    = id
    nipInputTransform  _ = id
    nipProofTransform  _ = id
