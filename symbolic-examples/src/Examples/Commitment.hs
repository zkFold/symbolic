module Examples.Commitment (
    exampleCommitment
  ) where

import           ZkFold.Base.Data.Vector                    (Vector)
import           ZkFold.Base.Protocol.IVC.Commit            (HomomorphicCommit (..))
import           ZkFold.Symbolic.Data.EllipticCurve.Ed25519 (Ed25519_Point)
import           ZkFold.Symbolic.Data.FieldElement          (FieldElement)

exampleCommitment
    :: HomomorphicCommit (Vector 1 (FieldElement c)) (Ed25519_Point c)
    => Vector 1 (FieldElement c)
    -> Ed25519_Point c
exampleCommitment = hcommit
