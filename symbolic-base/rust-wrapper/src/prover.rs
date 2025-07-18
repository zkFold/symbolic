use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_ff::Field;
use ark_ff::One;
use ark_msm::msm::VariableBaseMSM;
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use ark_poly::Polynomial;
use core::slice;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

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
    secret: Vec<ScalarField>,
}

struct PlonkupWitness {
    w1: DensePolynomial<ScalarField>,
    w2: DensePolynomial<ScalarField>,
    w3: DensePolynomial<ScalarField>,
}

struct Relation {
    qM: DensePolynomial<ScalarField>,
    qL: DensePolynomial<ScalarField>,
    qR: DensePolynomial<ScalarField>,
    qO: DensePolynomial<ScalarField>,
    qC: DensePolynomial<ScalarField>,
    qK: DensePolynomial<ScalarField>,
    t1: DensePolynomial<ScalarField>,
    t2: DensePolynomial<ScalarField>,
    t3: DensePolynomial<ScalarField>,
    wPub: Vec<ScalarField>,
    prvNum: usize,
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

fn toPolyVec(v: &[ScalarField]) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecZero(n: usize) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecConstant(a0: &ScalarField) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecLinear(a0: &ScalarField, a1: &ScalarField) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecQuadratic(
    a0: &ScalarField,
    a1: &ScalarField,
    a2: &ScalarField,
) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecLagrange(n: usize, pow: usize, omega: &ScalarField) -> DensePolynomial<ScalarField> {
    todo!()
}

fn polyVecInLagrangeBasis(
    n: usize,
    omega: &ScalarField,
    p: &DensePolynomial<ScalarField>,
) -> DensePolynomial<ScalarField> {
    todo!()
}

fn com(gs: &Vec<GAffine>, p: &DensePolynomial<ScalarField>) -> GAffine {
    todo!()
}

fn compress(pt: GAffine) -> Vec<u8> {
    todo!()
}

fn bytes(pt: ScalarField) -> Vec<u8> {
    todo!()
}

fn challenge(transcript: &Vec<u8>) -> ScalarField {
    todo!()
}

fn zip_with<A, B, C, F>(a: &[A], b: &[B], mut f: F) -> Vec<C>
where
    F: FnMut(&A, &B) -> C,
{
    a.iter().zip(b.iter()).map(|(x, y)| f(x, y)).collect()
}

fn zip_with3<'a, A, B, C, D, F>(v1: &'a [A], v2: &'a [B], v3: &'a [C], f: F) -> Vec<D>
where
    F: Fn(&A, &B, &C) -> D,
{
    v1.iter()
        .zip(v2)
        .zip(v3)
        .map(|((a, b), c)| f(a, b, c))
        .collect()
}

fn ifilter<T, F>(v: &[T], mut pred: F) -> Vec<T>
where
    T: Clone,
    F: FnMut(usize, &T) -> bool,
{
    v.iter()
        .enumerate()
        .filter(|(i, x)| pred(*i, x))
        .map(|(_, x)| x.clone())
        .collect()
}

fn iterate_n<T, F>(n: usize, mut f: F, x: T) -> Vec<T>
where
    T: Clone,
    F: FnMut(&T) -> T,
{
    let mut result = Vec::with_capacity(n);
    let mut current = x;

    for _ in 0..n {
        result.push(current.clone());
        current = f(&current);
    }

    result
}

fn sortByList(f: &Vec<ScalarField>, t: &Vec<ScalarField>) -> Vec<ScalarField> {
    todo!()
}

fn concat_vecs<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut result = Vec::with_capacity(a.len() + b.len());
    result.extend_from_slice(a);
    result.extend_from_slice(b);
    result
}

fn cumprod(pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    todo!()
}

fn rotR(pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    todo!()
}

fn rotL(pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    todo!()
}

trait Secret {
    fn get(&self, ix: usize) -> ScalarField;
}

impl Secret for PlonkupProverSecret {
    fn get(&self, ix: usize) -> ScalarField {
        self.secret[ix - 1]
    }
}

trait Elementwise<T> {
    fn elementwise<F>(&self, f: F) -> Self
    where
        F: FnMut(&T) -> T;
}

impl<T: Clone + ark_ff::Field> Elementwise<T> for DensePolynomial<T> {
    fn elementwise<F>(&self, mut f: F) -> Self
    where
        F: FnMut(&T) -> T,
    {
        let new_coeffs = self.coeffs.iter().map(|c| f(c)).collect();
        DensePolynomial::from_coefficients_vec(new_coeffs)
    }
}

pub trait ZipWith<T> {
    fn zip_with<F>(&self, other: &Self, f: F) -> Self
    where
        F: FnMut(&T, &T) -> T;
}

impl<T: Clone + ark_ff::Field> ZipWith<T> for DensePolynomial<T> {
    fn zip_with<F>(&self, other: &Self, mut f: F) -> Self
    where
        F: FnMut(&T, &T) -> T,
    {
        let min_len = self.coeffs.len().min(other.coeffs.len());
        let coeffs = self.coeffs[..min_len]
            .iter()
            .zip(&other.coeffs[..min_len])
            .map(|(a, b)| f(a, b))
            .collect();

        DensePolynomial::from_coefficients_vec(coeffs)
    }
}

pub fn plonkupProve(
    n: usize,
    ps: &PlonkupProverSetup,
    secret: &PlonkupProverSecret,
    relation: &Relation,
    witness: &PlonkupWitness,
) -> (PlonkupProof, PlonkupProverTestInfo) {
    let omega = ps.omega;
    let k1 = ps.k1;
    let k2 = ps.k2;
    let gs = ps.gs;
    let sigma1s = ps.sigma1s;
    let sigma2s = ps.sigma2s;
    let sigma3s = ps.sigma3s;

    let qlX = ps.polynomials.qlX;
    let qrX = ps.polynomials.qrX;
    let qoX = ps.polynomials.qoX;
    let qmX = ps.polynomials.qmX;
    let qcX = ps.polynomials.qcX;
    let qkX = ps.polynomials.qkX;
    let t1X = ps.polynomials.t1X;
    let t2X = ps.polynomials.t2X;
    let t3X = ps.polynomials.t3X;
    let s1X = ps.polynomials.s1X;
    let s2X = ps.polynomials.s2X;
    let s3X = ps.polynomials.s3X;

    let qM = relation.qM;
    let qL = relation.qL;
    let qR = relation.qR;
    let qO = relation.qO;
    let qC = relation.qC;
    let qK = relation.qK;
    let t1 = relation.t1;
    let t2 = relation.t2;
    let t3 = relation.t3;
    let wPub = relation.wPub;
    let prvNum = relation.prvNum;

    let zhX = polyVecZero(n);

    let w1X = polyVecInLagrangeBasis(n, &omega, &witness.w1);
    let w2X = polyVecInLagrangeBasis(n, &omega, &witness.w2);
    let w3X = polyVecInLagrangeBasis(n, &omega, &witness.w3);

    let pi = toPolyVec();
    let piX = polyVecInLagrangeBasis(n, &omega, &pi);

    let aX = polyVecLinear(&secret.get(1), &secret.get(2)).mul(&zhX) + w1X;
    let bX = polyVecLinear(&secret.get(3), &secret.get(4)).mul(&zhX) + w2X;
    let cX = polyVecLinear(&secret.get(5), &secret.get(6)).mul(&zhX) + w3X;

    let cmA = com(&gs, &aX);
    let cmB = com(&gs, &bX);
    let cmC = com(&gs, &cX);

    let mut ts1 = vec![];
    ts1.append(&mut compress(cmA));
    ts1.append(&mut compress(cmB));
    ts1.append(&mut compress(cmC));

    let zeta = challenge(&ts1);

    let f_zeta_tick = &witness.w1 + &(&witness.w2 + &witness.w3.mul(zeta)).mul(zeta);
    let t_zeta = &t1 + &(&t2 + &t3.mul(zeta)).mul(zeta);
    let f_zeta = toPolyVec(&zip_with3(
        &qK.coeffs,
        &t_zeta.coeffs,
        &f_zeta_tick.coeffs,
        |lk, ti, ai| if lk == &ScalarField::one() { *ai } else { *ti },
    ));

    let fX = polyVecLinear(&secret.get(7), &secret.get(8)).mul(&zhX)
        + polyVecInLagrangeBasis(n, &omega, &f_zeta);
    let tX = &t1X + &(t2X + t3X.mul(zeta)).mul(zeta);

    let s = sortByList(&concat_vecs(&f_zeta.coeffs, &t_zeta.coeffs), &t_zeta.coeffs);

    let h1 = toPolyVec(&ifilter(&s, |i, _| i % 2 == 0));
    let h2 = toPolyVec(&ifilter(&s, |i, _| i % 2 == 1));

    let h1X = polyVecQuadratic(&secret.get(9), &secret.get(10), &secret.get(11)).mul(&zhX)
        + polyVecInLagrangeBasis(n, &omega, &h1);

    let h2X = polyVecLinear(&secret.get(12), &secret.get(13)).mul(&zhX)
        + polyVecInLagrangeBasis(n, &omega, &h2);

    let cmF = com(&gs, &fX);
    let cmH1 = com(&gs, &h1X);
    let cmH2 = com(&gs, &h2X);

    let mut ts2 = ts1.clone();
    ts2.append(&mut compress(cmF));
    ts2.append(&mut compress(cmH1));
    ts2.append(&mut compress(cmH2));

    let ts21 = ts2.clone();
    ts21.push(1);
    let ts22 = ts2.clone();
    ts22.push(2);
    let ts23 = ts2.clone();
    ts23.push(3);
    let ts24 = ts2.clone();
    ts24.push(4);

    let beta = challenge(&ts21);
    let gamma = challenge(&ts22);
    let delta = challenge(&ts23);
    let epsilon = challenge(&ts24);

    let omegas = toPolyVec(&iterate_n(n, |x| x * &omega, omega));
    let omegas_tick = toPolyVec(&iterate_n((4 * n + 6), |x| x * &omega, ScalarField::one()));

    let gp1_1 = (&witness.w1 + &(omegas.mul(beta))).elementwise(|x| x + &gamma);
    let gp1_2 = (&witness.w2 + &(omegas.mul(beta * k1))).elementwise(|x| x + &gamma);
    let gp1_3 = (&witness.w3 + &(omegas.mul(beta * k2))).elementwise(|x| x + &gamma);
    let gp1_4 = (&witness.w1 + &(sigma1s.mul(beta))).elementwise(|x| x + &gamma);
    let gp1_5 = (&witness.w2 + &(sigma2s.mul(beta))).elementwise(|x| x + &gamma);
    let gp1_6 = (&witness.w3 + &(sigma3s.mul(beta))).elementwise(|x| x + &gamma);

    let grandProduct1 = rotR(&cumprod(
        &gp1_1
            .zip_with(&gp1_2, |a, b| a * b)
            .zip_with(&gp1_3, |a, b| a * b)
            .zip_with(&gp1_4, |a, b| a / b)
            .zip_with(&gp1_5, |a, b| a / b)
            .zip_with(&gp1_6, |a, b| a / b),
    ));

    let z1X = polyVecQuadratic(&secret.get(14), &secret.get(15), &secret.get(16)).mul(&zhX)
        + polyVecInLagrangeBasis(n, &omega, &grandProduct1);

    let eps_del = epsilon * (delta + ScalarField::one());
    let gp2_1 = f_zeta
        .elementwise(|x| x + &epsilon)
        .mul(delta + ScalarField::one());
    let gp2_2 = t_zeta.elementwise(|x| x + &eps_del) + rotL(&t_zeta).mul(delta);
    let gp2_3 = h1.elementwise(|x| x + &eps_del) + h2.mul(delta);
    let gp2_4 = h2.elementwise(|x| x + &eps_del) + rotL(&h1).mul(delta);
    let grandProduct2 = rotR(&cumprod(
        &gp2_1
            .zip_with(&gp2_2, |a, b| a * b)
            .zip_with(&gp2_3, |a, b| a / b)
            .zip_with(&gp2_4, |a, b| a / b),
    ));

    let z2X = polyVecQuadratic(&secret.get(17), &secret.get(18), &secret.get(19)).mul(&zhX)
        + polyVecInLagrangeBasis(n, &omega, &grandProduct2);

    let cmZ1 = com(&gs, &z1X);
    let cmZ2 = com(&gs, &z2X);

    let mut ts3 = ts2.clone();
    ts3.append(&mut compress(cmZ1));
    ts3.append(&mut compress(cmZ2));

    let alpha = challenge(&ts3);
    let alpha2 = alpha * alpha;
    let alpha3 = alpha2 * alpha;
    let alpha4 = alpha3 * alpha;
    let alpha5 = alpha4 * alpha;

    let gammaX = polyVecConstant(&gamma);
    let deltaX = polyVecConstant(&delta);
    let epsilonX = polyVecConstant(&epsilon);

    let qX = {
        let qXs1 = qmX.mul(&aX).mul(&bX) + qlX.mul(&aX) + qrX.mul(&bX) + qoX.mul(&cX) + piX + qcX;

        let qXs2 = (aX + polyVecLinear(&beta, &gamma))
            .mul(&(bX + polyVecLinear(&(beta * k1), &gamma)))
            .mul(&(cX + polyVecLinear(&(beta * k2), &gamma)))
            .mul(&z1X)
            .mul(alpha);

        let qXs3 = (aX + s1X.mul(beta) + gammaX)
            .mul(&(bX + s2X.mul(beta) + gammaX))
            .mul(&(cX + s3X.mul(beta) + gammaX))
            .mul(&(z1X.zip_with(&omegas_tick, |a, b| a * b).mul(alpha)));

        let qXs4 = (z1X.sub(&polyVecConstant(&ScalarField::one())))
            .mul(&polyVecLagrange(n, 1, &omega))
            .mul(alpha2);

        let qXs5 = qkX
            .mul(&(aX + (bX + cX.mul(zeta)).mul(zeta).sub(&fX)))
            .mul(alpha3);

        let qXs6 = z2X
            .mul(&(polyVecConstant(&ScalarField::one()) + deltaX))
            .mul(&(epsilonX + fX))
            .mul(&(epsilonX.mul(&(polyVecConstant(&ScalarField::one()) + deltaX))))
            + tX
            + deltaX
                .mul(&(tX.zip_with(&omegas_tick, |a, b| a * b)))
                .mul(alpha4);

        let qXs7 = z2X
            .zip_with(&omegas_tick, |a, b| a * b)
            .mul(
                &((epsilonX.mul(&(polyVecConstant(&ScalarField::one()) + deltaX)))
                    + h1X
                    + deltaX.mul(&h2X)),
            )
            .mul(
                &((epsilonX.mul(&(polyVecConstant(&ScalarField::one()) + deltaX)))
                    + h2X
                    + deltaX.mul(&(h1X.zip_with(&omegas_tick, |a, b| a * b)))),
            )
            .mul(alpha4);

        let qXs8 = (z2X.sub(&polyVecConstant(&ScalarField::one())))
            .mul(&polyVecLagrange(n, 1, &omega))
            .mul(alpha5);

        let qXNumerator = qXs1 + qXs2.sub(&qXs3) + qXs4 + qXs5 + qXs6.sub(&qXs7) + qXs8;

        qXNumerator.div(&zhX)
    };

    let qlowX = toPolyVec(&qX.coeffs[..n + 2]);
    let qmidX = toPolyVec(&qX.coeffs[n + 2..2 * (n + 2)]);
    let qhighX = toPolyVec(&qX.coeffs[2 * (n + 2)..]);

    let cmQlow = com(&gs, &qlowX);
    let cmQmid = com(&gs, &qmidX);
    let cmQhigh = com(&gs, &qhighX);

    let mut ts4 = ts3.clone();
    ts4.append(&mut compress(cmQlow));
    ts4.append(&mut compress(cmQmid));
    ts4.append(&mut compress(cmQhigh));

    let xi = challenge(&ts4);

    let a_xi = aX.evaluate(&xi);
    let b_xi = bX.evaluate(&xi);
    let c_xi = cX.evaluate(&xi);
    let s1_xi = s1X.evaluate(&xi);
    let s2_xi = s2X.evaluate(&xi);
    let f_xi = fX.evaluate(&xi);
    let t_xi = tX.evaluate(&xi);
    let t_xi_tick = tX.evaluate(&(xi * omega));
    let z1_xi_tick = z1X.evaluate(&(xi * omega));
    let z2_xi_tick = z2X.evaluate(&(xi * omega));
    let h1_xi_tick = h1X.evaluate(&(xi * omega));
    let h2_xi = h2X.evaluate(&xi);
    let lag1_xi = polyVecLagrange(n, 1, &omega).evaluate(&xi);
    let l1_xi = ((xi - omega) * ScalarField::from(n as i32))
        .inverse()
        .unwrap();

    let l_range: Vec<usize> = (prvNum + 1..=wPub.len()).collect();
    let l_xi = l_range
        .iter()
        .map(|i| ((xi - omega.pow([*i as u64])) * ScalarField::from(n as i32)).inverse())
        .collect();

    let mut ts5 = ts4.clone();
    ts5.append(&mut bytes(a_xi));
    ts5.append(&mut bytes(b_xi));
    ts5.append(&mut bytes(c_xi));
    ts5.append(&mut bytes(s1_xi));
    ts5.append(&mut bytes(s2_xi));
    ts5.append(&mut bytes(f_xi));
    ts5.append(&mut bytes(t_xi));
    ts5.append(&mut bytes(t_xi_tick));
    ts5.append(&mut bytes(z1_xi_tick));
    ts5.append(&mut bytes(z2_xi_tick));
    ts5.append(&mut bytes(h1_xi_tick));
    ts5.append(&mut bytes(h2_xi));
    let v = challenge(&ts5);

    let pi_xi  = piX.evaluate(&xi);
    let zhX_xi = zhX.evaluate(&xi);



    todo!()
}
