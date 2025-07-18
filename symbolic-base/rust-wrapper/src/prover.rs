use crate::scale::get_opt_window_size;
use crate::utils::unpack_scalar;
use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_ff::{Field, One, PrimeField};
use ark_msm::msm::VariableBaseMSM;
use ark_poly::domain::Radix2EvaluationDomain;
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use ark_poly::EvaluationDomain;
use ark_poly::Polynomial;
use ark_serialize::CanonicalSerialize;
use ark_std::log2;
use ark_std::Zero;
use core::slice;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, MulAssign, Neg, Sub};

pub struct PlonkupCircuitPolynomials {
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

pub struct PlonkupProverSetup {
    omega: ScalarField,
    k1: ScalarField,
    k2: ScalarField,
    gs: Vec<GAffine>,
    sigma1s: DensePolynomial<ScalarField>,
    sigma2s: DensePolynomial<ScalarField>,
    sigma3s: DensePolynomial<ScalarField>,
    polynomials: PlonkupCircuitPolynomials,
}

pub struct PlonkupProverSecret {
    secret: Vec<ScalarField>,
}

pub struct PlonkupWitness {
    w1: DensePolynomial<ScalarField>,
    w2: DensePolynomial<ScalarField>,
    w3: DensePolynomial<ScalarField>,
}

pub struct Relation {
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

pub struct PlonkupProof {
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

pub struct PlonkupProverTestInfo {
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
    DensePolynomial::from_coefficients_vec((*v).to_vec())
}

fn polyVecZero(n: usize) -> DensePolynomial<ScalarField> {
    let coeffs = std::iter::once(ScalarField::one().neg())
        .chain(std::iter::repeat(ScalarField::zero()).take(n - 1))
        .chain(std::iter::once(ScalarField::one()))
        .collect();
    DensePolynomial::from_coefficients_vec(coeffs)
}

fn polyVecConstant(a0: &ScalarField) -> DensePolynomial<ScalarField> {
    toPolyVec(&[*a0])
}

fn polyVecLinear(a1: &ScalarField, a0: &ScalarField) -> DensePolynomial<ScalarField> {
    toPolyVec(&[*a0, *a1])
}

fn polyVecQuadratic(
    a0: &ScalarField,
    a1: &ScalarField,
    a2: &ScalarField,
) -> DensePolynomial<ScalarField> {
    toPolyVec(&[*a0, *a1, *a2])
}

fn polyVecLagrange(n: usize, i: usize, omega: &ScalarField) -> DensePolynomial<ScalarField> {
    let wi = omega.pow([i as u64]);

    let wInv = wi.inverse().unwrap();

    let norm = wi * ScalarField::from(n as i32).inverse().unwrap();

    let mut v = Vec::with_capacity(n);

    let v0 = norm * wi.pow([(n - 1) as u64]);

    v.push(v0);

    for i in 1..n {
        let prev = v[i - 1];
        v.push(prev * wInv);
    }
    DensePolynomial::from_coefficients_vec(v)
}

// taken from https://github.com/dusk-network/plonk/blob/master/src/fft/domain.rs
//
fn bitreverse(mut n: u32, l: u32) -> u32 {
    let mut r = 0;
    for _ in 0..l {
        r = (r << 1) | (n & 1);
        n >>= 1;
    }
    r
}

fn serial_fft(a: &mut [ScalarField], omega: ScalarField, log_n: u32) {
    let n = a.len() as u32;
    assert_eq!(n, 1 << log_n);

    for k in 0..n {
        let rk = bitreverse(k, log_n);
        if k < rk {
            a.swap(rk as usize, k as usize);
        }
    }

    let mut m = 1;
    for _ in 0..log_n {
        let w_m = omega.pow(&[(n / (2 * m)) as u64, 0, 0, 0]);

        let mut k = 0;
        while k < n {
            let mut w = ScalarField::one();
            for j in 0..m {
                let mut t = a[(k + j + m) as usize];
                t *= &w;
                let mut tmp = a[(k + j) as usize];
                tmp -= &t;
                a[(k + j + m) as usize] = tmp;
                a[(k + j) as usize] += &t;
                w.mul_assign(&w_m);
            }

            k += 2 * m;
        }

        m *= 2;
    }
}

fn polyVecInLagrangeBasis(
    n: usize,
    omega: &ScalarField,
    p: &DensePolynomial<ScalarField>,
) -> DensePolynomial<ScalarField> {
    let v = &mut p.coeffs.clone();

    let next_pow2 = v.len().next_power_of_two();

    if next_pow2 > v.len() {
        v.resize(next_pow2, ScalarField::zero());
    }

    let norm = ScalarField::from(n as i32).inverse().unwrap();

    let mut norms = Vec::with_capacity(n);
    for i in 0..n {
        norms.push(omega.pow([i as u64]) * norm);
    }
    v.rotate_right(1);

    let mut result = zip_with(&v, &norms, |a, b| a * b);

    serial_fft(&mut result, *omega, log2(n));
    result.reverse();

    DensePolynomial::from_coefficients_vec(result)
}

fn com(gs: &Vec<GAffine>, p: &DensePolynomial<ScalarField>) -> GAffine {
    let scalars: Vec<_> = p.coeffs.iter().map(|i| i.into_bigint()).collect();
    let opt_window_size = get_opt_window_size(log2(gs.len()));
    VariableBaseMSM::multi_scalar_mul_custom(gs, &scalars, opt_window_size, 2048, 256, false).into()
}

fn compress(pt: &GAffine) -> Vec<u8> {
    let mut buf = Vec::new();
    pt.serialize_compressed(&mut buf)
        .expect("Serialization failed");
    buf
}

fn bytes(pt: &ScalarField) -> Vec<u8> {
    unpack_scalar(*pt)
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
    let order: HashMap<&ScalarField, usize> = t.iter().enumerate().map(|(i, v)| (v, i)).collect();
    let mut f_copy = f.clone();
    f_copy.sort_by_key(|x| order.get(x).cloned().unwrap());
    f_copy
}

fn concat_vecs<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut result = Vec::with_capacity(a.len() + b.len());
    result.extend_from_slice(a);
    result.extend_from_slice(b);
    result
}

fn cumprod(pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    let v = &pv.coeffs;
    let mut result = Vec::with_capacity(v.len());

    result[0] = v[0].clone();
    for i in 1..v.len() {
        result[i] = &result[i - 1] * &v[i];
    }

    DensePolynomial::from_coefficients_vec(result)
}

fn rotR(pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    let mut v = pv.coeffs.clone();
    v.rotate_right(1);
    DensePolynomial::from_coefficients_vec(v)
}

fn rotL(pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    let mut v = pv.coeffs.clone();
    v.rotate_left(1);
    DensePolynomial::from_coefficients_vec(v)
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

fn vn(v: &ScalarField, n: u64) -> ScalarField {
    v.pow([n])
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
    let gs = &ps.gs;
    let sigma1s = &ps.sigma1s;
    let sigma2s = &ps.sigma2s;
    let sigma3s = &ps.sigma3s;

    let qlX = &ps.polynomials.qlX;
    let qrX = &ps.polynomials.qrX;
    let qoX = &ps.polynomials.qoX;
    let qmX = &ps.polynomials.qmX;
    let qcX = &ps.polynomials.qcX;
    let qkX = &ps.polynomials.qkX;
    let t1X = &ps.polynomials.t1X;
    let t2X = &ps.polynomials.t2X;
    let t3X = &ps.polynomials.t3X;
    let s1X = &ps.polynomials.s1X;
    let s2X = &ps.polynomials.s2X;
    let s3X = &ps.polynomials.s3X;

    let qM = &relation.qM;
    let qL = &relation.qL;
    let qR = &relation.qR;
    let qO = &relation.qO;
    let qC = &relation.qC;
    let qK = &relation.qK;
    let t1 = &relation.t1;
    let t2 = &relation.t2;
    let t3 = &relation.t3;
    let wPub = &relation.wPub;
    let prvNum = relation.prvNum;

    let w1 = &witness.w1;
    let w2 = &witness.w2;
    let w3 = &witness.w3;

    let one = &ScalarField::one();

    let zhX = &polyVecZero(n);

    let piX = {
        let mut pi_v = vec![ScalarField::zero(); prvNum];
        pi_v.append(&mut wPub.iter().map(|x| x.neg()).collect());
        let pi = toPolyVec(&pi_v);

        &polyVecInLagrangeBasis(n, &omega, &pi)
    };

    let aX = {
        let w1X = polyVecInLagrangeBasis(n, &omega, &witness.w1);
        &(&polyVecLinear(&secret.get(1), &secret.get(2)).mul(zhX) + &w1X)
    };
    let bX = {
        let w2X = polyVecInLagrangeBasis(n, &omega, &witness.w2);
        &(&polyVecLinear(&secret.get(3), &secret.get(4)).mul(zhX) + &w2X)
    };
    let cX = {
        let w3X = polyVecInLagrangeBasis(n, &omega, &witness.w3);
        &(&polyVecLinear(&secret.get(5), &secret.get(6)).mul(zhX) + &w3X)
    };

    let cmA = com(&gs, aX);
    let cmB = com(&gs, bX);
    let cmC = com(&gs, cX);

    let mut ts1 = vec![];
    ts1.append(&mut compress(&cmA));
    ts1.append(&mut compress(&cmB));
    ts1.append(&mut compress(&cmC));

    let zeta = challenge(&ts1);

    let f_zeta_tick = &(w1 + &(w2 + &w3.mul(zeta)).mul(zeta));
    let t_zeta = &(t1 + &(t2 + &t3.mul(zeta)).mul(zeta));
    let f_zeta = &toPolyVec(&zip_with3(
        &qK.coeffs,
        &t_zeta.coeffs,
        &f_zeta_tick.coeffs,
        |lk, ti, ai| if lk == one { *ai } else { *ti },
    ));

    let fX = &(polyVecLinear(&secret.get(7), &secret.get(8)).mul(zhX)
        + polyVecInLagrangeBasis(n, &omega, f_zeta));
    let tX = &(t1X + &(t2X + &t3X.mul(zeta)).mul(zeta));

    let s = &sortByList(&concat_vecs(&f_zeta.coeffs, &t_zeta.coeffs), &t_zeta.coeffs);

    let h1 = &toPolyVec(&ifilter(s, |i, _| i % 2 == 0));
    let h2 = &toPolyVec(&ifilter(s, |i, _| i % 2 == 1));

    let h1X = &(polyVecQuadratic(&secret.get(9), &secret.get(10), &secret.get(11)).mul(zhX)
        + polyVecInLagrangeBasis(n, &omega, h1));

    let h2X = &(polyVecLinear(&secret.get(12), &secret.get(13)).mul(zhX)
        + polyVecInLagrangeBasis(n, &omega, h2));

    let cmF = com(&gs, fX);
    let cmH1 = com(&gs, h1X);
    let cmH2 = com(&gs, h2X);

    let mut ts2 = ts1.clone();
    ts2.append(&mut compress(&cmF));
    ts2.append(&mut compress(&cmH1));
    ts2.append(&mut compress(&cmH2));

    let mut ts21 = ts2.clone();
    ts21.push(1);
    let mut ts22 = ts2.clone();
    ts22.push(2);
    let mut ts23 = ts2.clone();
    ts23.push(3);
    let mut ts24 = ts2.clone();
    ts24.push(4);

    let beta = challenge(&ts21);
    let gamma = challenge(&ts22);
    let delta = challenge(&ts23);
    let epsilon = challenge(&ts24);

    let omegas = &toPolyVec(&iterate_n(n, |x| *x * &omega, omega));
    let omegas_tick = &toPolyVec(&iterate_n((4 * n + 6), |x| *x * &omega, *one));

    let grandProduct1 = {
        let gp1_1 = (&witness.w1 + &(omegas.mul(beta))).elementwise(|x| x + &gamma);
        let gp1_2 = (&witness.w2 + &(omegas.mul(beta * k1))).elementwise(|x| x + &gamma);
        let gp1_3 = (&witness.w3 + &(omegas.mul(beta * k2))).elementwise(|x| x + &gamma);
        let gp1_4 = (&witness.w1 + &(sigma1s.mul(beta))).elementwise(|x| x + &gamma);
        let gp1_5 = (&witness.w2 + &(sigma2s.mul(beta))).elementwise(|x| x + &gamma);
        let gp1_6 = (&witness.w3 + &(sigma3s.mul(beta))).elementwise(|x| x + &gamma);

        &rotR(&cumprod(
            &gp1_1
                .zip_with(&gp1_2, |a, b| a * b)
                .zip_with(&gp1_3, |a, b| a * b)
                .zip_with(&gp1_4, |a, b| a / b)
                .zip_with(&gp1_5, |a, b| a / b)
                .zip_with(&gp1_6, |a, b| a / b),
        ))
    };

    let z1X = &(polyVecQuadratic(&secret.get(14), &secret.get(15), &secret.get(16)).mul(zhX)
        + polyVecInLagrangeBasis(n, &omega, &grandProduct1));

    let grandProduct2 = {
        let eps_del = epsilon * (delta + one);
        let gp2_1 = f_zeta.elementwise(|x| x + &epsilon).mul(delta + one);
        let gp2_2 = t_zeta.elementwise(|x| x + &eps_del) + rotL(t_zeta).mul(delta);
        let gp2_3 = h1.elementwise(|x| x + &eps_del) + h2.mul(delta);
        let gp2_4 = h2.elementwise(|x| x + &eps_del) + rotL(h1).mul(delta);

        &rotR(&cumprod(
            &gp2_1
                .zip_with(&gp2_2, |a, b| a * b)
                .zip_with(&gp2_3, |a, b| a / b)
                .zip_with(&gp2_4, |a, b| a / b),
        ))
    };

    let z2X = &(polyVecQuadratic(&secret.get(17), &secret.get(18), &secret.get(19)).mul(zhX)
        + polyVecInLagrangeBasis(n, &omega, &grandProduct2));

    let cmZ1 = com(&gs, z1X);
    let cmZ2 = com(&gs, z2X);

    let mut ts3 = ts2.clone();
    ts3.append(&mut compress(&cmZ1));
    ts3.append(&mut compress(&cmZ2));

    let alpha = challenge(&ts3);
    let alpha2 = alpha * alpha;
    let alpha3 = alpha2 * alpha;
    let alpha4 = alpha3 * alpha;
    let alpha5 = alpha4 * alpha;

    let gammaX = &polyVecConstant(&gamma);
    let deltaX = &polyVecConstant(&delta);
    let epsilonX = &polyVecConstant(&epsilon);

    let qX = {
        let qXs1 = &(&(qmX.mul(aX).mul(bX) + qlX.mul(aX) + qrX.mul(bX) + qoX.mul(cX)) + piX) + qcX;

        let qXs2 = (aX + &polyVecLinear(&beta, &gamma))
            .mul(&(bX + &polyVecLinear(&(beta * k1), &gamma)))
            .mul(&(cX + &polyVecLinear(&(beta * k2), &gamma)))
            .mul(z1X)
            .mul(alpha);

        let qXs3 = (&(aX + &s1X.mul(beta)) + gammaX)
            .mul(&(&(bX + &s2X.mul(beta)) + gammaX))
            .mul(&(&(cX + &s3X.mul(beta)) + gammaX))
            .mul(&(z1X.zip_with(&omegas_tick, |a, b| a * b).mul(alpha)));

        let qXs4 = (z1X.sub(&polyVecConstant(one)))
            .mul(&polyVecLagrange(n, 1, &omega))
            .mul(alpha2);

        let qXs5 = qkX
            .mul(&(aX + &(bX + &cX.mul(zeta)).mul(zeta).sub(fX)))
            .mul(alpha3);

        let qXs6 = &z2X
            .mul(&(&polyVecConstant(one) + deltaX))
            .mul(&(epsilonX + fX))
            .mul(&(epsilonX.mul(&(&polyVecConstant(one) + deltaX))))
            + tX
            + deltaX
                .mul(&(tX.zip_with(&omegas_tick, |a, b| a * b)))
                .mul(alpha4);

        let qXs7 = z2X
            .zip_with(&omegas_tick, |a, b| a * b)
            .mul(&(&(epsilonX.mul(&(&polyVecConstant(one) + deltaX))) + h1X + deltaX.mul(h2X)))
            .mul(
                &(&(epsilonX.mul(&(&polyVecConstant(one) + deltaX)))
                    + h2X
                    + deltaX.mul(&(h1X.zip_with(&omegas_tick, |a, b| a * b)))),
            )
            .mul(alpha4);

        let qXs8 = (z2X.sub(&polyVecConstant(one)))
            .mul(&polyVecLagrange(n, 1, &omega))
            .mul(alpha5);

        let qXNumerator = qXs1 + qXs2.sub(&qXs3) + qXs4 + qXs5 + qXs6.sub(&qXs7) + qXs8;

        &qXNumerator.div(zhX)
    };

    let qlowX = &toPolyVec(&qX.coeffs[..n + 2]);
    let qmidX = &toPolyVec(&qX.coeffs[n + 2..2 * (n + 2)]);
    let qhighX = &toPolyVec(&qX.coeffs[2 * (n + 2)..]);

    let cmQlow = com(&gs, &qlowX);
    let cmQmid = com(&gs, &qmidX);
    let cmQhigh = com(&gs, &qhighX);

    let mut ts4 = ts3.clone();
    ts4.append(&mut compress(&cmQlow));
    ts4.append(&mut compress(&cmQmid));
    ts4.append(&mut compress(&cmQhigh));

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

    let l_xi = {
        let l_range: Vec<usize> = (prvNum + 1..=wPub.len()).collect();
        l_range
            .iter()
            .map(|i| {
                ((xi - omega.pow([*i as u64])) * ScalarField::from(n as i32))
                    .inverse()
                    .unwrap()
            })
            .collect()
    };

    let mut ts5 = ts4.clone();
    ts5.append(&mut bytes(&a_xi));
    ts5.append(&mut bytes(&b_xi));
    ts5.append(&mut bytes(&c_xi));
    ts5.append(&mut bytes(&s1_xi));
    ts5.append(&mut bytes(&s2_xi));
    ts5.append(&mut bytes(&f_xi));
    ts5.append(&mut bytes(&t_xi));
    ts5.append(&mut bytes(&t_xi_tick));
    ts5.append(&mut bytes(&z1_xi_tick));
    ts5.append(&mut bytes(&z2_xi_tick));
    ts5.append(&mut bytes(&h1_xi_tick));
    ts5.append(&mut bytes(&h2_xi));
    let v = challenge(&ts5);

    let pi_xi = &piX.evaluate(&xi);
    let zhX_xi = &zhX.evaluate(&xi);

    let rX = {
        let eps_del = epsilon * (delta + one);

        let rX1 = &(qmX.mul(a_xi * b_xi)
            + qlX.mul(a_xi)
            + qrX.mul(b_xi)
            + qoX.mul(c_xi)
            + polyVecConstant(&pi_xi))
            + qcX;

        let rX21 = z1X.mul(
            (a_xi + beta * xi + gamma)
                * (b_xi + beta * k1 * xi + gamma)
                * (c_xi + beta * k2 * xi + gamma),
        );
        let rX22 = (polyVecConstant(&c_xi) + s3X.mul(beta) + polyVecConstant(&gamma))
            .mul((a_xi + beta * s1_xi + gamma) * (b_xi + beta * s2_xi + gamma) * z1_xi_tick);
        let rX2 = rX21.sub(&rX22).mul(alpha);

        let rX3 = z1X.sub(&polyVecConstant(one)).mul(alpha2 * lag1_xi);
        let rX4 = qkX.mul(alpha3 * (a_xi + zeta * (b_xi + zeta * c_xi) - f_xi));

        let rX51 =
            z2X.mul((*one + delta) * (epsilon + f_xi) * (eps_del + t_xi + delta * t_xi_tick));
        let rX52 = (&polyVecConstant(&(eps_del)) + h1X + polyVecConstant(&(delta * h2_xi)))
            .mul(z2_xi_tick * ((eps_del) + h2_xi + delta * h1_xi_tick));
        let rX5 = rX51.sub(&rX52).mul(alpha4);

        let rX6 = (z2X.sub(&polyVecConstant(one))).mul(alpha5 * lag1_xi);
        let rX7 = (qlowX
            + &(qmidX.mul(xi.pow([(n + 2) as u64])))
            + qhighX.mul(xi.pow([(2 * n + 4) as u64])))
        .mul(*zhX_xi);

        &(rX1 + rX2 + rX3 + rX4 + rX5 + rX6.sub(&rX7))
    };

    let proofX1 = {
        let pf1 = aX.sub(&polyVecConstant(&a_xi)).mul(vn(&v, 1));
        let pf2 = bX.sub(&polyVecConstant(&b_xi)).mul(vn(&v, 2));
        let pf3 = cX.sub(&polyVecConstant(&c_xi)).mul(vn(&v, 3));
        let pf4 = s1X.sub(&polyVecConstant(&s1_xi)).mul(vn(&v, 4));
        let pf5 = s2X.sub(&polyVecConstant(&s2_xi)).mul(vn(&v, 5));
        let pf6 = fX.sub(&polyVecConstant(&f_xi)).mul(vn(&v, 6));
        let pf7 = tX.sub(&polyVecConstant(&t_xi)).mul(vn(&v, 7));
        let pf8 = h2X.sub(&polyVecConstant(&h2_xi)).mul(vn(&v, 8));

        let pfNumerator = rX + &(pf1 + pf2 + pf3 + pf4 + pf5 + pf6 + pf7 + pf8);

        pfNumerator.div(&polyVecLinear(&one, &xi.neg()))
    };

    let proofX2 = {
        let pf1 = z1X.sub(&polyVecConstant(&z1_xi_tick));
        let pf2 = tX.sub(&polyVecConstant(&t_xi_tick)).mul(vn(&v, 1));
        let pf3 = z2X.sub(&polyVecConstant(&z2_xi_tick)).mul(vn(&v, 2));
        let pf4 = h1X.sub(&polyVecConstant(&h1_xi_tick)).mul(vn(&v, 3));

        let pfNumerator = pf1 + pf2 + pf3 + pf4;

        pfNumerator.div(&polyVecLinear(&one, &((xi * omega).neg())))
    };

    let proof1 = com(&gs, &proofX1);
    let proof2 = com(&gs, &proofX2);

    let plonkupProof = PlonkupProof {
        cmA: cmA,
        cmB: cmB,
        cmC: cmC,
        cmF: cmF,
        cmH1: cmH1,
        cmH2: cmH2,
        cmZ1: cmZ1,
        cmZ2: cmZ2,
        cmQlow: cmQlow,
        cmQmid: cmQmid,
        cmQhigh: cmQhigh,
        proof1: proof1,
        proof2: proof2,
        a_xi: a_xi,
        b_xi: b_xi,
        c_xi: c_xi,
        s1_xi: s1_xi,
        s2_xi: s2_xi,
        f_xi: f_xi,
        t_xi: t_xi,
        t_xi_tick: t_xi_tick,
        z1_xi_tick: z1_xi_tick,
        z2_xi_tick: z2_xi_tick,
        h1_xi_tick: h1_xi_tick,
        h2_xi: h2_xi,
        l1_xi: l1_xi,
        l_xi: l_xi,
    };

    let testInfo = PlonkupProverTestInfo {
        omega: omega,
        k1: k1,
        k2: k2,
        qlX: qlX.clone(),
        qrX: qrX.clone(),
        qoX: qoX.clone(),
        qmX: qmX.clone(),
        qcX: qcX.clone(),
        qkX: qkX.clone(),
        t1X: t1X.clone(),
        t2X: t2X.clone(),
        t3X: t3X.clone(),
        s1X: s1X.clone(),
        s2X: s2X.clone(),
        s3X: s3X.clone(),
        aX: aX.clone(),
        bX: bX.clone(),
        cX: cX.clone(),
        piX: piX.clone(),
        tX: tX.clone(),
        z1X: z1X.clone(),
        z2X: z2X.clone(),
        fX: fX.clone(),
        h1X: h1X.clone(),
        h2X: h2X.clone(),
        zhX: zhX.clone(),
        qX: qX.clone(),
        qlowX: qlowX.clone(),
        qmidX: qmidX.clone(),
        qhighX: qhighX.clone(),
        rX: rX.clone(),
        alpha: alpha,
        beta: beta,
        gamma: gamma,
        delta: delta,
        epsilon: epsilon,
        xi: xi,
        zeta: zeta,
        f_zeta: f_zeta.clone(),
        t_zeta: t_zeta.clone(),
        omegas: omegas.clone(),
        omegas_tick: omegas_tick.clone(),
        grandProduct1: grandProduct1.clone(),
        grandProduct2: grandProduct2.clone(),
        w1: w1.clone(),
        w2: w2.clone(),
        w3: w3.clone(),
    };

    (plonkupProof, testInfo)
}
