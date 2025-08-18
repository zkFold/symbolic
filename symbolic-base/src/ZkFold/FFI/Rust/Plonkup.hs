{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TypeOperators #-}

module ZkFold.FFI.Rust.Plonkup where

import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Foreign
import GHC.IO (unsafePerformIO)
import GHC.Natural (naturalToInteger)
import ZkFold.Algebra.EllipticCurve.BLS12_381 ( Fr, BLS12_381_G1_Point, BLS12_381_G2_Point)
import ZkFold.Algebra.EllipticCurve.Class (CyclicGroup (ScalarFieldOf))
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate
import ZkFold.Data.Vector
import ZkFold.FFI.Rust.RustFunctions (r_plonkup_prove_serialized, r_plonkup_prove)
import ZkFold.Protocol.Plonkup.Input (PlonkupInput (PlonkupInput))
import ZkFold.Protocol.Plonkup.Proof
import ZkFold.Protocol.Plonkup.Prover
import ZkFold.Protocol.Plonkup.Relation
import ZkFold.Protocol.Plonkup.Testing
import ZkFold.Protocol.Plonkup.Witness
import Prelude
import ZkFold.FFI.Rust.Types hiding (Fr)
import ZkFold.FFI.Rust.Conversion

instance Binary (PlonkupCircuitPolynomials n BLS12_381_G1_Point (PolyVec Fr)) where
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

instance Binary (PlonkupProverSetup i o n BLS12_381_G1_Point BLS12_381_G2_Point (PolyVec Fr)) where
  put (PlonkupProverSetup {..}) =
    put omega
      <> put k1
      <> put k2
      <> put (V.toList gs)
      <> put sigma1s
      <> put sigma2s
      <> put sigma3s
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
    polynomials <- get
    pure $ PlonkupProverSetup {..}

instance Binary (PlonkupProverSecret BLS12_381_G1_Point) where
  put (PlonkupProverSecret v) = put $ V.toList $ toV v

  get = PlonkupProverSecret . Vector . V.fromList <$> get

instance Binary (PlonkupProof BLS12_381_G1_Point) where
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

instance (ScalarFieldOf g ~ Fr) => Binary (PlonkupProverTestInfo n g (PolyVec Fr)) where
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
      BLS12_381_G1_Point
      BLS12_381_G2_Point
      (PolyVec (ScalarFieldOf BLS12_381_G1_Point))
  -> (PlonkupWitnessInput i BLS12_381_G1_Point, PlonkupProverSecret BLS12_381_G1_Point)
  -> ( PlonkupInput BLS12_381_G1_Point
     , PlonkupProof BLS12_381_G1_Point
     , PlonkupProverTestInfo n BLS12_381_G1_Point (PolyVec (ScalarFieldOf BLS12_381_G1_Point))
     )
rustPlonkupProve
  proverSetup
  (PlonkupWitnessInput wInput, proverSecret) = unsafePerformIO $ do
    let !proverSetup' = encode proverSetup
    let !proverSecret' = encode proverSecret
    let !proverRelation = encode (relation proverSetup)
    let !rel = relation proverSetup
    let (!w1, !w2, !w3) = witness rel wInput
    let !pubInput' = pubInput rel
    let !wPub' =  pubInput' wInput
    let !wPub = encode wPub'
    let !n = fromInteger $ naturalToInteger $ value @n

    BS.useAsCStringLen (BS.toStrict proverSetup') $ \(ptr1, len1) -> do
      BS.useAsCStringLen (BS.toStrict proverSecret') $ \(ptr2, len2) -> do
        BS.useAsCStringLen (BS.toStrict (proverRelation <> wPub)) $ \(ptr3, len3) -> do
          BS.useAsCStringLen (BS.toStrict (mconcat $ encode <$> [w1, w2, w3])) $ \(ptr4, len4) -> do
            ptr <- r_plonkup_prove_serialized n ptr1 len1 ptr2 len2 ptr3 len3 ptr4 len4
            len <- peek (castPtr ptr) :: IO Int
            bs <- BS.packCStringLen (ptr `plusPtr` 8, len)
            free ptr
            let (proof, testInfo) = decode (BS.fromStrict bs)
            pure (PlonkupInput wPub', proof, testInfo)

rustPlonkupProveNative
  :: forall i o n
   . KnownNat n
  => PlonkupProverSetup
      i
      o
      n
      Rust_BLS12_381_G1_Point
      Rust_BLS12_381_G2_Point
      (RustPolyVec (ScalarFieldOf Rust_BLS12_381_G1_Point))
  -> (PlonkupWitnessInput i Rust_BLS12_381_G1_Point, PlonkupProverSecret Rust_BLS12_381_G1_Point)
  -> ( PlonkupInput BLS12_381_G1_Point
     , PlonkupProof BLS12_381_G1_Point
     , PlonkupProverTestInfo n BLS12_381_G1_Point (PolyVec (ScalarFieldOf BLS12_381_G1_Point))
     )
rustPlonkupProveNative
  (PlonkupProverSetup {..})
  (PlonkupWitnessInput wInput, PlonkupProverSecret secret) = unsafePerformIO $ do
    let PlonkupRelation {..} = relation
    let PlonkupCircuitPolynomials {..} = polynomials
    let (!w1, !w2, !w3) = witness wInput
    let !wPub = pubInput wInput
    let !n = integral @n
    withForeignPtr (rawData $ rawType $ omega) $ \omega_ptr -> do
    withForeignPtr (rawData $ rawType $ k1) $ \k1_ptr -> do
    withForeignPtr (rawData $ rawType $ k2) $ \k2_ptr -> do
    withForeignPtr (rawData $ rawType $ h2r $ r2h <$> gs) $ \gs_ptr -> do
    withForeignPtr (rawData $ rawType $ sigma1s) $ \sigma1s_ptr -> do
    withForeignPtr (rawData $ rawType $ sigma2s) $ \sigma2s_ptr -> do
    withForeignPtr (rawData $ rawType $ sigma3s) $ \sigma3s_ptr -> do
    withForeignPtr (rawData $ rawType $ qlX) $ \qlX_ptr -> do
    withForeignPtr (rawData $ rawType $ qrX) $ \qrX_ptr -> do
    withForeignPtr (rawData $ rawType $ qoX) $ \qoX_ptr -> do
    withForeignPtr (rawData $ rawType $ qmX) $ \qmX_ptr -> do
    withForeignPtr (rawData $ rawType $ qcX) $ \qcX_ptr -> do
    withForeignPtr (rawData $ rawType $ qkX) $ \qkX_ptr -> do
    withForeignPtr (rawData $ rawType $ t1X) $ \t1X_ptr -> do
    withForeignPtr (rawData $ rawType $ t2X) $ \t2X_ptr -> do
    withForeignPtr (rawData $ rawType $ t3X) $ \t3X_ptr -> do
    withForeignPtr (rawData $ rawType $ s1X) $ \s1X_ptr -> do
    withForeignPtr (rawData $ rawType $ s2X) $ \s2X_ptr -> do
    withForeignPtr (rawData $ rawType $ s3X) $ \s3X_ptr -> do
    withForeignPtr (rawData $ rawType $ h2r $ (toPolyVec $ toV (r2h <$> secret) :: PolyVec Fr 19)) $ \proverSecret_ptr -> do
    withForeignPtr (rawData $ rawType $ qM) $ \qM_ptr -> do
    withForeignPtr (rawData $ rawType $ qL) $ \qL_ptr -> do
    withForeignPtr (rawData $ rawType $ qR) $ \qR_ptr -> do
    withForeignPtr (rawData $ rawType $ qO) $ \qO_ptr -> do
    withForeignPtr (rawData $ rawType $ qC) $ \qC_ptr -> do
    withForeignPtr (rawData $ rawType $ qK) $ \qK_ptr -> do
    withForeignPtr (rawData $ rawType $ t1) $ \t1_ptr -> do
    withForeignPtr (rawData $ rawType $ t2) $ \t2_ptr -> do
    withForeignPtr (rawData $ rawType $ t3) $ \t3_ptr -> do
    withForeignPtr (rawData $ rawType $ h2r $ V.fromList $ r2h <$> wPub) $ \wPub_ptr -> do
    withForeignPtr (rawData $ rawType $ w1) $ \w1_ptr -> do
    withForeignPtr (rawData $ rawType $ w2) $ \w2_ptr -> do
    withForeignPtr (rawData $ rawType $ w3) $ \w3_ptr -> do
      ptr <- r_plonkup_prove
                n
                omega_ptr
                k1_ptr
                k2_ptr
                gs_ptr
                sigma1s_ptr
                sigma2s_ptr
                sigma3s_ptr
                qlX_ptr
                qrX_ptr
                qoX_ptr
                qmX_ptr
                qcX_ptr
                qkX_ptr
                t1X_ptr
                t2X_ptr
                t3X_ptr
                s1X_ptr
                s2X_ptr
                s3X_ptr
                proverSecret_ptr
                qM_ptr
                qL_ptr
                qR_ptr
                qO_ptr
                qC_ptr
                qK_ptr
                t1_ptr
                t2_ptr
                t3_ptr
                (fromInteger $ naturalToInteger prvNum)
                wPub_ptr
                w1_ptr
                w2_ptr
                w3_ptr

      len <- peek (castPtr ptr) :: IO Int
      bs <- BS.packCStringLen (ptr `plusPtr` 8, len)
      free ptr
      let (proof, testInfo) = decode (BS.fromStrict bs)
      pure (PlonkupInput $ r2h <$> wPub, proof, testInfo)
