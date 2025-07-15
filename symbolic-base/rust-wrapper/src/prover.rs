use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_poly::univariate::DensePolynomial;
use ark_msm::msm::VariableBaseMSM;
use ark_poly::DenseUVPolynomial;
use core::slice;
use std::ops::Div;
use std::ops::Mul;

struct PlonkupCircuitPolynomials {
    qlX: DensePolynomial<ScalarField>,
    qrX: DensePolynomial<ScalarField>,
    qoX: DensePolynomial<ScalarField>,
    qmX: DensePolynomial<ScalarField>,
    qcX: DensePolynomial<ScalarField>,
    qkX: DensePolynomial<ScalarField>,
    t1X: DensePolynomial<ScalarField>,
    t2X: DensePolynomial<ScalarField>,
    t3X: DensePolynomial<ScalarField>,
    s1X: DensePolynomial<ScalarField>,
    s2X: DensePolynomial<ScalarField>,
    s3X: DensePolynomial<ScalarField>,
}

struct PlonkupProverSetup {
    omega: ScalarField,
    k1: ScalarField,
    k2: ScalarField,
    gs: Vec<GAffine>,
    sigma1s: DensePolynomial<ScalarField>,
    sigma2s: DensePolynomial<ScalarField>,
    sigma3s: DensePolynomial<ScalarField>,
    polynomials: PlonkupCircuitPolynomials,
}

struct PlonkupProverSecret {
    proverSecret: Vec<ScalarField>
}

struct PlonkupWitness {
    w1: DensePolynomial<ScalarField>,
    w2: DensePolynomial<ScalarField>,
    w3: DensePolynomial<ScalarField>,
}

struct PlonkupProof {
    cmA: GAffine,
    cmB: GAffine,
    cmC: GAffine,
    cmF: GAffine,
    cmH1: GAffine,
    cmH2: GAffine,
    cmZ1: GAffine,
    cmZ2: GAffine,
    cmQlow: GAffine,
    cmQmid: GAffine,
    cmQhigh: GAffine,
    proof1: GAffine,
    proof2: GAffine,
    a_xi: ScalarField,
    b_xi: ScalarField,
    c_xi: ScalarField,
    s1_xi: ScalarField,
    s2_xi: ScalarField,
    f_xi: ScalarField,
    t_xi: ScalarField,
    t_xi_tick: ScalarField,
    z1_xi_tick: ScalarField,
    z2_xi_tick: ScalarField,
    h1_xi_tick: ScalarField,
    h2_xi: ScalarField,
    l1_xi: ScalarField,
    l_xi: Vec<ScalarField>,
}

struct PlonkupProverTestInfo {
    omega: ScalarField,
    k1: ScalarField,
    k2: ScalarField,
    qlX: DensePolynomial<ScalarField>,
    qrX: DensePolynomial<ScalarField>,
    qoX: DensePolynomial<ScalarField>,
    qmX: DensePolynomial<ScalarField>,
    qcX: DensePolynomial<ScalarField>,
    qkX: DensePolynomial<ScalarField>,
    t1X: DensePolynomial<ScalarField>,
    t2X: DensePolynomial<ScalarField>,
    t3X: DensePolynomial<ScalarField>,
    s1X: DensePolynomial<ScalarField>,
    s2X: DensePolynomial<ScalarField>,
    s3X: DensePolynomial<ScalarField>,
    aX: DensePolynomial<ScalarField>,
    bX: DensePolynomial<ScalarField>,
    cX: DensePolynomial<ScalarField>,
    piX: DensePolynomial<ScalarField>,
    tX: DensePolynomial<ScalarField>,
    z1X: DensePolynomial<ScalarField>,
    z2X: DensePolynomial<ScalarField>,
    fX: DensePolynomial<ScalarField>,
    h1X: DensePolynomial<ScalarField>,
    h2X: DensePolynomial<ScalarField>,
    zhX: DensePolynomial<ScalarField>,
    qX: DensePolynomial<ScalarField>,
    qlowX: DensePolynomial<ScalarField>,
    qmidX: DensePolynomial<ScalarField>,
    qhighX: DensePolynomial<ScalarField>,
    rX: DensePolynomial<ScalarField>,
    alpha: ScalarField,
    beta: ScalarField,
    gamma: ScalarField,
    delta: ScalarField,
    epsilon: ScalarField,
    xi: ScalarField,
    zeta: ScalarField,
    f_zeta: DensePolynomial<ScalarField>,
    t_zeta: DensePolynomial<ScalarField>,
    omegas: DensePolynomial<ScalarField>,
    omegas_tick: DensePolynomial<ScalarField>,
    grandProduct1: DensePolynomial<ScalarField>,
    grandProduct2: DensePolynomial<ScalarField>,
    w1: DensePolynomial<ScalarField>,
    w2: DensePolynomial<ScalarField>,
    w3: DensePolynomial<ScalarField>,
}

fn polyVecZero(n: i64) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecLinear(a0: &ScalarField, a1: &ScalarField) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecQuadratic(a0: &ScalarField, a1: &ScalarField, a2: &ScalarField) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecInLagrangeBasis(n: i64, omega: &ScalarField, p: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    todo!()
}

fn secret(proverSecret: &PlonkupProverSecret, i: usize) -> ScalarField {
    proverSecret.proverSecret[i - 1]
}

pub fn plonkupProve(n: i64, proverSetup: &PlonkupProverSetup, proverSecret: &PlonkupProverSecret, witness: &PlonkupWitness) -> (PlonkupProof, PlonkupProverTestInfo) {
    todo!()
}

