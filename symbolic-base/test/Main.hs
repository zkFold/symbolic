module Main where

import           Prelude                            hiding (Bool, Fractional (..), Num (..), drop, length, replicate,
                                                     take, (==))
import           System.Random                      (RandomGen, initStdGen)
import           Test.Hspec                         (Spec, hspec)
import           Tests.Algebra.EllipticCurve        (specEllipticCurve)
import           Tests.Algebra.Field                (specField)
import           Tests.Algebra.GroebnerBasis        (specGroebner)
import           Tests.Algebra.Group                (specAdditiveGroup)
import           Tests.Algebra.Pairing              (specPairing)
import           Tests.Algebra.Permutations         (specPermutations)
import           Tests.Algebra.ReedSolomon          (specReedSolomon)
import           Tests.Algebra.Univariate           (specUnivariate)
import           Tests.Data.Binary                  (specBinary)
import           Tests.Protocol.IVC                 (specIVC)
import           Tests.Protocol.NonInteractiveProof (specNonInteractiveProof)
import           Tests.Protocol.Plonkup             (specPlonkup)
import           Tests.Symbolic.Algorithm.Blake2b   (specBlake2b)
import           Tests.Symbolic.Algorithm.JWT       (specJWT)
import           Tests.Symbolic.Algorithm.RSA       (specRSA)
import           Tests.Symbolic.Algorithm.SHA2      (specSHA2, specSHA2Natural)
import           Tests.Symbolic.ArithmeticCircuit   (specArithmeticCircuit)
import           Tests.Symbolic.Compiler            (specCompiler)
import           Tests.Symbolic.Data.ByteString     (specByteString)
import           Tests.Symbolic.Data.FFA            (specFFA)
import           Tests.Symbolic.Data.Hash           (specHash)
import           Tests.Symbolic.Data.Int            (specInt)
import           Tests.Symbolic.Data.List           (specList)
import           Tests.Symbolic.Data.UInt           (specUInt)

spec :: RandomGen g => g -> Spec
spec gen = do
    -- Base.Algebra
    specField
    specAdditiveGroup
    specEllipticCurve
    specPairing
    specPermutations
    specUnivariate
    specReedSolomon
    specGroebner

    -- Base.Data
    specBinary

    -- Base.Protocol
    specPlonkup
    specNonInteractiveProof
    specIVC

    -- Compiler spec
    specArithmeticCircuit
    specCompiler

    -- Symbolic types and operations
    specHash
    specList

    specUInt
    specInt
    specFFA
    specByteString

    -- Symbolic cryptography
    specBlake2b
    specJWT
    specRSA gen
    specSHA2Natural
    specSHA2

main :: IO ()
main = hspec . spec =<< initStdGen
