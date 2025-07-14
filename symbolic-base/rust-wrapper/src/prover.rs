use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_poly::univariate::DensePolynomial;
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

}

struct PlonkupProverTestInfo {

}

pub fn plonkupProve(proverSetup: &PlonkupProverSetup, secret: &PlonkupProverSecret) -> (PlonkupProof, PlonkupProverTestInfo) {

}

