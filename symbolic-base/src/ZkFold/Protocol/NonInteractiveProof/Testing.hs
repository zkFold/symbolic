{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Protocol.NonInteractiveProof.Testing where

import ZkFold.Protocol.NonInteractiveProof.Class
import Prelude

class (NonInteractiveProof a, NonInteractiveProof b) => CompatibleNonInteractiveProofs a b where
  nipSetupTransform :: SetupVerify a -> SetupVerify b
  nipInputTransform :: Input a -> Input b
  nipProofTransform :: Proof a -> Proof b

nipCompatibility
  :: forall a b
   . CompatibleNonInteractiveProofs a b
  => a -> Witness a -> Bool
nipCompatibility a w =
  let (i, p) = prove @a (setupProve @a a) w
      s' = nipSetupTransform @a @b (setupVerify @a a)
      i' = nipInputTransform @a @b i
      p' = nipProofTransform @a @b p
   in verify @b s' i' p'

instance NonInteractiveProof a => CompatibleNonInteractiveProofs a a where
  nipSetupTransform = id
  nipInputTransform = id
  nipProofTransform = id
