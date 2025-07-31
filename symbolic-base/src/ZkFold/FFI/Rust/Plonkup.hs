{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.FFI.Rust.Plonkup where

import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Foreign
import GHC.IO (unsafePerformIO)
import GHC.Natural (naturalToInteger)
import Prelude

import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_JacobianPoint, BLS12_381_G2_JacobianPoint, Fr)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (ScalarFieldOf))
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate
import ZkFold.Data.Vector
import ZkFold.FFI.Rust.RustFunctions (rsPlonkupProve)
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover
import ZkFold.Protocol.Plonkup.Relation
import ZkFold.Protocol.Plonkup.Testing
import ZkFold.Protocol.Plonkup.Witness

instance Binary (PlonkupCircuitPolynomials n BLS12_381_G1_JacobianPoint (PolyVec Fr)) where
  put (PlonkupCircuitPolynomials {..}) =
    put qlX
      <> put qrX
      <> put qoX
      <> put qmX
      <> put qcX
      <> put qkX
      <> put t1X
      <> put t2X
      <> put t3X
      <> put s1X
      <> put s2X
      <> put s3X

  get = do
    qlX <- get
    qrX <- get
    qoX <- get
    qmX <- get
    qcX <- get
    qkX <- get
    t1X <- get
    t2X <- get
    t3X <- get
    s1X <- get
    s2X <- get
    s3X <- get
    pure $ PlonkupCircuitPolynomials {..}

instance Binary (PlonkupProverSetup i o n BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint (PolyVec Fr)) where
  put (PlonkupProverSetup {..}) =
    put omega
      <> put k1
      <> put k2
      <> put (V.toList gs)
      <> put sigma1s
      <> put sigma2s
      <> put sigma3s
      -- <> put relation
      <> put polynomials

  get = do
    omega <- get
    k1 <- get
    k2 <- get
    gs <- V.fromList <$> get
    sigma1s <- get
    sigma2s <- get
    sigma3s <- get
    relation <- undefined
    -- relation <- get
    polynomials <- get
    pure $ PlonkupProverSetup {..}

instance Binary (PlonkupProverSecret BLS12_381_G1_JacobianPoint) where
  put (PlonkupProverSecret v) = put $ V.toList $ toV v

  get = PlonkupProverSecret . Vector . V.fromList <$> get

instance Binary (PlonkupProof BLS12_381_G1_JacobianPoint) where
  put (PlonkupProof {..}) =
    put cmA
      <> put cmB
      <> put cmC
      <> put cmF
      <> put cmH1
      <> put cmH2
      <> put cmZ1
      <> put cmZ2
      <> put cmQlow
      <> put cmQmid
      <> put cmQhigh
      <> put proof1
      <> put proof2
      <> put a_xi
      <> put b_xi
      <> put c_xi
      <> put s1_xi
      <> put s2_xi
      <> put f_xi
      <> put t_xi
      <> put t_xi'
      <> put z1_xi'
      <> put z2_xi'
      <> put h1_xi'
      <> put h2_xi
      <> put l1_xi
      <> put l_xi

  get = do
    cmA <- get
    cmB <- get
    cmC <- get
    cmF <- get
    cmH1 <- get
    cmH2 <- get
    cmZ1 <- get
    cmZ2 <- get
    cmQlow <- get
    cmQmid <- get
    cmQhigh <- get
    proof1 <- get
    proof2 <- get
    a_xi <- get
    b_xi <- get
    c_xi <- get
    s1_xi <- get
    s2_xi <- get
    f_xi <- get
    t_xi <- get
    t_xi' <- get
    z1_xi' <- get
    z2_xi' <- get
    h1_xi' <- get
    h2_xi <- get
    l1_xi <- get
    l_xi <- get
    pure $ PlonkupProof {..}

instance Binary (PlonkupProverTestInfo n BLS12_381_G1_JacobianPoint (PolyVec Fr)) where
  put (PlonkupProverTestInfo {..}) =
    put omega
      <> put k1
      <> put k2
      <> put qlX
      <> put qrX
      <> put qoX
      <> put qmX
      <> put qcX
      <> put qkX
      <> put t1X
      <> put t2X
      <> put t3X
      <> put s1X
      <> put s2X
      <> put s3X
      <> put aX
      <> put bX
      <> put cX
      <> put piX
      <> put tX
      <> put z1X
      <> put z2X
      <> put fX
      <> put h1X
      <> put h2X
      <> put zhX
      <> put qX
      <> put qlowX
      <> put qmidX
      <> put qhighX
      <> put rX
      <> put alpha
      <> put beta
      <> put gamma
      <> put delta
      <> put epsilon
      <> put xi
      <> put zeta
      <> put f_zeta
      <> put t_zeta
      <> put omegas
      <> put omegas'
      <> put grandProduct1
      <> put grandProduct2
      <> put w1
      <> put w2
      <> put w3

  get = do
    omega <- get
    k1 <- get
    k2 <- get
    qlX <- get
    qrX <- get
    qoX <- get
    qmX <- get
    qcX <- get
    qkX <- get
    t1X <- get
    t2X <- get
    t3X <- get
    s1X <- get
    s2X <- get
    s3X <- get
    aX <- get
    bX <- get
    cX <- get
    piX <- get
    tX <- get
    z1X <- get
    z2X <- get
    fX <- get
    h1X <- get
    h2X <- get
    zhX <- get
    qX <- get
    qlowX <- get
    qmidX <- get
    qhighX <- get
    rX <- get
    alpha <- get
    beta <- get
    gamma <- get
    delta <- get
    epsilon <- get
    xi <- get
    zeta <- get
    f_zeta <- get
    t_zeta <- get
    omegas <- get
    omegas' <- get
    grandProduct1 <- get
    grandProduct2 <- get
    w1 <- get
    w2 <- get
    w3 <- get
    pure $ PlonkupProverTestInfo {..}

instance Binary (PlonkupRelation i o n Fr (PolyVec Fr)) where
  put PlonkupRelation {..} =
    put qM
      <> put qL
      <> put qR
      <> put qO
      <> put qC
      <> put qK
      <> put t1
      <> put t2
      <> put t3
      <> put (fromInteger $ naturalToInteger prvNum :: Int)
  get = error "Can't get relation"

rustPlonkupProve
  :: forall i o n
   . KnownNat n
  => PlonkupProverSetup
       i
       o
       n
       BLS12_381_G1_JacobianPoint
       BLS12_381_G2_JacobianPoint
       (PolyVec (ScalarFieldOf BLS12_381_G1_JacobianPoint))
  -> (PlonkupWitnessInput i BLS12_381_G1_JacobianPoint, PlonkupProverSecret BLS12_381_G1_JacobianPoint)
  -> ( PlonkupProof BLS12_381_G1_JacobianPoint
     , PlonkupProverTestInfo n BLS12_381_G1_JacobianPoint (PolyVec (ScalarFieldOf BLS12_381_G1_JacobianPoint))
     )
rustPlonkupProve
  proverSetup
  (PlonkupWitnessInput wInput, proverSecret) = unsafePerformIO $ do
    let proverSetup' = encode proverSetup
        proverSecret' = encode proverSecret
        proverRelation = encode (relation proverSetup)
        (!w1, !w2, !w3) = witness (relation proverSetup) wInput
        !wPub = encode (pubInput (relation proverSetup) wInput)
        !n = fromInteger $ naturalToInteger $ value @n
    BS.useAsCStringLen (BS.toStrict proverSetup') $ \(ptr1, len1) -> do
      BS.useAsCStringLen (BS.toStrict proverSecret') $ \(ptr2, len2) -> do
        BS.useAsCStringLen (BS.toStrict (proverRelation <> wPub)) $ \(ptr3, len3) -> do
          BS.useAsCStringLen (BS.toStrict (mconcat $ encode <$> [w1, w2, w3])) $ \(ptr4, len4) -> do
            ptr <- rsPlonkupProve n ptr1 len1 ptr2 len2 ptr3 len3 ptr4 len4
            len <- peek (castPtr ptr) :: IO Int
            bs <- BS.packCStringLen (ptr `plusPtr` 8, len)
            free ptr
            pure $ decode (BS.fromStrict bs)
