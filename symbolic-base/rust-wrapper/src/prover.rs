#![allow(non_snake_case)]
use crate::utils::unpack_scalar;
use ark_bls12_381::G1Projective;
use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_ec::{CurveGroup, VariableBaseMSM};
use ark_ff::{AdditiveGroup, Field, One, PrimeField};
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use ark_poly::Polynomial;
use ark_serialize::CanonicalDeserialize;
use ark_serialize::CanonicalSerialize;
use ark_std::log2;
use ark_std::Zero;
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use core::slice;
use itertools::Itertools;
use num_bigint::BigUint;
use std::collections::HashMap;
use std::ops::{Div, Mul, MulAssign, Neg, Sub};

use crate::utils::c_char;
#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct PlonkupProverSecret {
    secret: Vec<ScalarField>,
}

#[derive(Debug)]
pub struct PlonkupWitness {
    w1: DensePolynomial<ScalarField>,
    w2: DensePolynomial<ScalarField>,
    w3: DensePolynomial<ScalarField>,
}

#[derive(Debug)]
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
    prvNum: usize,
    wPub: Vec<ScalarField>,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

fn blake2b_224(data: &[u8]) -> Vec<u8> {
    // Create hasher configured for a 28-byte (224-bit) digest
    let mut hasher = Blake2bVar::new(28).expect("Invalid output size");
    hasher.update(data);

    let mut output = vec![0u8; 28];
    hasher
        .finalize_variable(&mut output)
        .expect("Hashing failed");
    output
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
    a2: &ScalarField,
    a1: &ScalarField,
    a0: &ScalarField,
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
    v.resize(n, ScalarField::ZERO);

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

    let mut result = zip_with_longest(&v, &norms, |a, b| a * b);
    let res_len = result.len();
    serial_fft(&mut result, *omega, log2(res_len));
    result.reverse();

    DensePolynomial::from_coefficients_vec(result)
}

fn com(gs: &Vec<GAffine>, p: &DensePolynomial<ScalarField>) -> GAffine {
    G1Projective::msm_unchecked(&gs, &p).into_affine()
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
    let digest = blake2b_224(transcript);
    ScalarField::from_le_bytes_mod_order(&digest)
}

fn zip_with_longest<A: Default, B: Default, C, F>(a: &[A], b: &[B], mut f: F) -> Vec<C>
where
    F: FnMut(&A, &B) -> C,
{
    let a_default = A::default();
    let b_default = B::default();
    a.iter()
        .zip_longest(b.iter())
        .map(|pair| {
            let (a, b) = pair.or(&a_default, &b_default);
            f(a, b)
        })
        .collect()
}

fn zip_with3_longest<A: Default, B: Default, C: Default, D, F>(
    v1: &[A],
    v2: &[B],
    v3: &[C],
    f: F,
) -> Vec<D>
where
    F: Fn(&A, &B, &C) -> D,
{
    let a_default = A::default();
    let b_default = B::default();
    let c_default = C::default();

    v1.iter()
        .zip_longest(v2)
        .zip_longest(v3)
        .map(|triplet| {
            let ((a, b), c) = triplet
                .map_left(|x| x.or(&a_default, &b_default))
                .or((&a_default, &b_default), &c_default);
            f(&a, &b, &c)
        })
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

fn cumprod(n:usize, pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    let mut v = pv.coeffs.clone();
    v.resize(n, ScalarField::ZERO);
    let mut result = Vec::with_capacity(v.len());
    if v.is_empty() {
        return DensePolynomial::zero();
    }
    result.push(v[0].clone());
    for i in 1..v.len() {
        result.push(&result[i - 1] * &v[i]);
    }

    DensePolynomial::from_coefficients_vec(result)
}

fn rotR(n:usize, pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    let mut v = pv.coeffs.clone();
    v.resize(n, ScalarField::ZERO);
    v.rotate_right(1);
    DensePolynomial::from_coefficients_vec(v)
}

fn rotL(n:usize, pv: &DensePolynomial<ScalarField>) -> DensePolynomial<ScalarField> {
    let mut v = pv.coeffs.clone();
    v.resize(n, ScalarField::ZERO);
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

trait DivMono {
    fn div_mono(&self, other: &Self) -> Self;
}

fn is_shifted_mono(p: &DensePolynomial<ScalarField>) -> Option<(usize, ScalarField, ScalarField)> {
    let coeffs = &p.coeffs;
    if coeffs.len() < 2 {
        return None;
    }
    if coeffs[0] == ScalarField::zero() {
        return None;
    }
    let mut count: usize = 0;
    let mut c: ScalarField = ScalarField::zero();
    let mut cix: usize = 0;
    for i in 1..coeffs.len() {
        if coeffs[i] != ScalarField::zero() {
            count += 1;
            c = coeffs[i];
            cix = i;
        }
    }
    if count == 1 {
        return Some((cix, c, coeffs[0]));
    }
    None
}

impl DivMono for DensePolynomial<ScalarField> {
    fn div_mono(&self, other: &Self) -> Self {
        if self.is_zero() {
            return DensePolynomial::zero();
        }
        let mono_coeff = is_shifted_mono(other);
        match mono_coeff {
            None => return self.div(other),
            Some((m, cm, c0)) => {
                let mut coeffs_mut = self.coeffs.clone();
                let mut result = vec![ScalarField::zero(); self.coeffs.len()];
                for i in (m..self.coeffs.len()).rev() {
                    let ci = coeffs_mut[i];
                    result[i - m] = ci;
                    coeffs_mut[i - m] -= ci * c0;
                }
                return DensePolynomial::from_coefficients_vec(result) * cm.inverse().unwrap();
            }
        }
    }
}

trait Elementwise<T> {
    fn elementwise<F>(&self, n:usize, f: F) -> Self
    where
        F: FnMut(&T) -> T;
}

impl<T: Clone + ark_ff::Field> Elementwise<T> for DensePolynomial<T> {
    fn elementwise<F>(&self, n: usize, mut f: F) -> Self
    where
        F: FnMut(&T) -> T,
    {
        let mut new_coeffs = self.coeffs.clone();
        new_coeffs.resize(n, T::ZERO);
        let result_coeffs = new_coeffs.iter().map(|c| f(c)).collect();
        DensePolynomial::from_coefficients_vec(result_coeffs)
    }
}

pub trait ZipWith<T> {
    fn zip_with<F>(&self, other: &Self, f: F) -> Self
    where
        F: FnMut(&T, &T) -> T;
}

impl<T: Clone + ark_ff::Field> ZipWith<T> for DensePolynomial<T> {
    fn zip_with<F>(&self, other: &Self, f: F) -> Self
    where
        F: FnMut(&T, &T) -> T,
    {
        DensePolynomial {
            coeffs: zip_with_longest(&self.coeffs, &other.coeffs, f),
        }
    }
}

fn vn(v: &ScalarField, n: u64) -> ScalarField {
    v.pow([n])
}

fn div_or_zero<T: AdditiveGroup>(a: &T, b: &T) -> T
where
    for<'a, 'b> &'a T: Div<&'b T, Output = T>,
{
    if b == &T::ZERO {
        T::ZERO
    } else {
        a / b
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

    let _qM = &relation.qM;
    let _qL = &relation.qL;
    let _qR = &relation.qR;
    let _qO = &relation.qO;
    let _qC = &relation.qC;
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
    let f_zeta = &toPolyVec(&zip_with3_longest(
        &qK.coeffs,
        &t_zeta.coeffs,
        &f_zeta_tick.coeffs,
        |lk, ti, ai| if lk == one { *ai } else { *ti },
    ));

    let fX = &(polyVecLinear(&secret.get(7), &secret.get(8)).mul(zhX)
        + polyVecInLagrangeBasis(n, &omega, f_zeta));
    let tX = &(t1X + &(t2X + &t3X.mul(zeta)).mul(zeta));

    let mut f_zeta_coeffs = f_zeta.coeffs.clone();
    f_zeta_coeffs.resize(n, ScalarField::ZERO);
    let mut t_zeta_coeffs = t_zeta.coeffs.clone();
    t_zeta_coeffs.resize(n, ScalarField::ZERO);

    let s = &sortByList(&concat_vecs(&f_zeta_coeffs, &t_zeta_coeffs), &t_zeta_coeffs);

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
    let omegas_tick = &toPolyVec(&iterate_n(4 * n + 6, |x| *x * &omega, *one));

    let grandProduct1 = {
        let gp1_1 = (&witness.w1 + &(omegas.mul(beta))).elementwise(n,|x| x + &gamma);
        let gp1_2 = (&witness.w2 + &(omegas.mul(beta * k1))).elementwise(n,|x| x + &gamma);
        let gp1_3 = (&witness.w3 + &(omegas.mul(beta * k2))).elementwise(n,|x| x + &gamma);
        let gp1_4 = (&witness.w1 + &(sigma1s.mul(beta))).elementwise(n,|x| x + &gamma);
        let gp1_5 = (&witness.w2 + &(sigma2s.mul(beta))).elementwise(n,|x| x + &gamma);
        let gp1_6 = (&witness.w3 + &(sigma3s.mul(beta))).elementwise(n,|x| x + &gamma);

        &rotR(n, &cumprod(n,
            &gp1_1
                .zip_with(&gp1_2, |a, b| a * b)
                .zip_with(&gp1_3, |a, b| a * b)
                .zip_with(&gp1_4, div_or_zero)
                .zip_with(&gp1_5, div_or_zero)
                .zip_with(&gp1_6, div_or_zero),
        ))
    };

    let z1X = &(polyVecQuadratic(&secret.get(14), &secret.get(15), &secret.get(16)).mul(zhX)
        + polyVecInLagrangeBasis(n, &omega, &grandProduct1));

    let grandProduct2 = {
        let eps_del = epsilon * (delta + one);
        let gp2_1 = f_zeta.elementwise(n,|x| x + &epsilon).mul(delta + one);
        let gp2_2 = t_zeta.elementwise(n,|x| x + &eps_del) + rotL(n, t_zeta).mul(delta);
        let gp2_3 = h1.elementwise(n,|x| x + &eps_del) + h2.mul(delta);
        let gp2_4 = h2.elementwise(n,|x| x + &eps_del) + rotL(n, h1).mul(delta);

        &rotR(n, &cumprod(n,
            &gp2_1
                .zip_with(&gp2_2, |a, b| a * b)
                .zip_with(&gp2_3, div_or_zero)
                .zip_with(&gp2_4, div_or_zero),
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
            .mul(&(z1X.zip_with(&omegas_tick, |a, b| a * b)))
            .mul(alpha);

        let qXs4 = (z1X.sub(&polyVecConstant(one)))
            .mul(&polyVecLagrange(n, 1, &omega))
            .mul(alpha2);

        let qXs5 = qkX
            .mul(&(aX + &(bX + &cX.mul(zeta)).mul(zeta).sub(fX)))
            .mul(alpha3);

        let qXs6 = &z2X
            .mul(&(&polyVecConstant(one) + deltaX))
            .mul(&(epsilonX + fX))
            .mul(
                &(epsilonX.mul(&(&polyVecConstant(one) + deltaX)))
                    + tX
                    + deltaX.mul(&(tX.zip_with(&omegas_tick, |a, b| a * b))),
            )
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

        &qXNumerator.div_mono(zhX)
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

        pfNumerator.div_mono(&polyVecLinear(&one, &xi.neg()))
    };

    let proofX2 = {
        let pf1 = z1X.sub(&polyVecConstant(&z1_xi_tick));
        let pf2 = tX.sub(&polyVecConstant(&t_xi_tick)).mul(vn(&v, 1));
        let pf3 = z2X.sub(&polyVecConstant(&z2_xi_tick)).mul(vn(&v, 2));
        let pf4 = h1X.sub(&polyVecConstant(&h1_xi_tick)).mul(vn(&v, 3));

        let pfNumerator = pf1 + pf2 + pf3 + pf4;

        pfNumerator.div_mono(&polyVecLinear(&one, &((xi * omega).neg())))
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

fn deserialize_scalar(buffer: &[u8]) -> (ScalarField, &[u8]) {
    let (left, right) = buffer.split_at(32);
    (PrimeField::from_le_bytes_mod_order(left), right)
}

fn serialize_scalar(scalar: ScalarField) -> Vec<u8> {
    let mut v = BigUint::from(scalar.into_bigint()).to_bytes_le();
    v.resize(std::mem::size_of::<ScalarField>(), 0);
    v
}

fn deserialize_point(buffer: &[u8]) -> (GAffine, &[u8]) {
    let (left, right) = buffer.split_at(96);
    (
        GAffine::deserialize_uncompressed_unchecked(left).unwrap(),
        right,
    )
}

fn serialize_point(point: GAffine) -> Vec<u8> {
    let mut res = Vec::new();
    point.serialize_uncompressed(&mut res).unwrap();
    res
}

fn deserialize_scalar_vec(buffer: &[u8]) -> (Vec<ScalarField>, &[u8]) {
    let (len_bytes, buffer) = buffer.split_at(8);
    let mut v = Vec::new();
    let len = u64::from_be_bytes(len_bytes.try_into().unwrap());
    if len < usize::MAX as u64 {
        v.reserve(len as usize);
    }
    let mut mut_buffer = buffer;
    for _ in 0..len {
        let (scalar, res1) = deserialize_scalar(mut_buffer);
        mut_buffer = res1;
        v.push(scalar);
    }
    (v, mut_buffer)
}

fn deserialize_point_vec(buffer: &[u8]) -> (Vec<GAffine>, &[u8]) {
    let (len_bytes, buffer) = buffer.split_at(8);
    let mut v = Vec::new();
    let len = u64::from_be_bytes(len_bytes.try_into().unwrap());
    if len < usize::MAX as u64 {
        v.reserve(len as usize);
    }
    let mut mut_buffer = buffer;
    for _ in 0..len {
        let (point, res1) = deserialize_point(mut_buffer);
        mut_buffer = res1;
        v.push(point);
    }
    (v, mut_buffer)
}

fn deserialize_scalar_poly(buffer: &[u8]) -> (DensePolynomial<ScalarField>, &[u8]) {
    let (coeffs, bytes) = deserialize_scalar_vec(buffer);
    (DensePolynomial::from_coefficients_vec(coeffs), bytes)
}

fn serialize_scalar_vec(vector: Vec<ScalarField>) -> Vec<u8> {
    let mut res = Vec::new();
    let len = vector.len() as u64;
    res.extend_from_slice(&u64::to_be_bytes(len));
    for scalar in vector {
        res.extend_from_slice(&serialize_scalar(scalar));
    }
    res
}

fn serialize_scalar_poly(poly: DensePolynomial<ScalarField>) -> Vec<u8> {
    serialize_scalar_vec(poly.coeffs)
}

fn deserialize_PlonkupCircuitPolynomials(buffer: &[u8]) -> (PlonkupCircuitPolynomials, &[u8]) {
    let (qlX, buffer) = deserialize_scalar_poly(buffer);
    let (qrX, buffer) = deserialize_scalar_poly(buffer);
    let (qoX, buffer) = deserialize_scalar_poly(buffer);
    let (qmX, buffer) = deserialize_scalar_poly(buffer);
    let (qcX, buffer) = deserialize_scalar_poly(buffer);
    let (qkX, buffer) = deserialize_scalar_poly(buffer);
    let (t1X, buffer) = deserialize_scalar_poly(buffer);
    let (t2X, buffer) = deserialize_scalar_poly(buffer);
    let (t3X, buffer) = deserialize_scalar_poly(buffer);
    let (s1X, buffer) = deserialize_scalar_poly(buffer);
    let (s2X, buffer) = deserialize_scalar_poly(buffer);
    let (s3X, buffer) = deserialize_scalar_poly(buffer);
    (
        PlonkupCircuitPolynomials {
            qlX,
            qrX,
            qoX,
            qmX,
            qcX,
            qkX,
            t1X,
            t2X,
            t3X,
            s1X,
            s2X,
            s3X,
        },
        buffer,
    )
}

fn deserialize_PlonkupProverSetup(buffer: &[u8]) -> (PlonkupProverSetup, &[u8]) {
    let (omega, buffer) = deserialize_scalar(buffer);
    let (k1, buffer) = deserialize_scalar(buffer);
    let (k2, buffer) = deserialize_scalar(buffer);
    let (gs, buffer) = deserialize_point_vec(buffer);
    let (sigma1s, buffer) = deserialize_scalar_poly(buffer);
    let (sigma2s, buffer) = deserialize_scalar_poly(buffer);
    let (sigma3s, buffer) = deserialize_scalar_poly(buffer);
    let (polynomials, buffer) = deserialize_PlonkupCircuitPolynomials(buffer);

    (
        PlonkupProverSetup {
            omega,
            k1,
            k2,
            gs,
            sigma1s,
            sigma2s,
            sigma3s,
            polynomials,
        },
        buffer,
    )
}

fn deserialize_relation(buffer: &[u8]) -> (Relation, &[u8]) {
    let (qM, buffer) = deserialize_scalar_poly(buffer);
    let (qL, buffer) = deserialize_scalar_poly(buffer);
    let (qR, buffer) = deserialize_scalar_poly(buffer);
    let (qO, buffer) = deserialize_scalar_poly(buffer);
    let (qC, buffer) = deserialize_scalar_poly(buffer);
    let (qK, buffer) = deserialize_scalar_poly(buffer);
    let (t1, buffer) = deserialize_scalar_poly(buffer);
    let (t2, buffer) = deserialize_scalar_poly(buffer);
    let (t3, buffer) = deserialize_scalar_poly(buffer);
    let (prv_number_bytes, buffer) = buffer.split_at(8);
    let prvNum = u64::from_be_bytes(prv_number_bytes.try_into().unwrap()) as usize;
    let (wPub, buffer) = deserialize_scalar_vec(buffer);
    return (
        Relation {
            qM,
            qL,
            qR,
            qO,
            qC,
            qK,
            t1,
            t2,
            t3,
            prvNum,
            wPub,
        },
        buffer,
    );
}

#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_plonkup_prove(
    n: u64,

    setup_ptr: *const c_char,
    setup_len: usize,

    secret_ptr: *const c_char,
    secret_len: usize,

    relation_ptr: *const c_char,
    relation_len: usize,

    witness_ptr: *const c_char,
    witness_len: usize,
) -> *const c_char {
    let setup_buffer = slice::from_raw_parts(setup_ptr as *const u8, setup_len);
    let secret_buffer = slice::from_raw_parts(secret_ptr as *const u8, secret_len);
    let relation_buffer = slice::from_raw_parts(relation_ptr as *const u8, relation_len);
    let witness_buffer = slice::from_raw_parts(witness_ptr as *const u8, witness_len);

    let (prover_setup, _) = deserialize_PlonkupProverSetup(setup_buffer);
    let (prover_secret, _) = deserialize_scalar_vec(secret_buffer);
    let (relation, _) = deserialize_relation(relation_buffer);
    let (w1, witness_buffer) = deserialize_scalar_poly(witness_buffer);
    let (w2, witness_buffer) = deserialize_scalar_poly(witness_buffer);
    let (w3, _) = deserialize_scalar_poly(witness_buffer);

    let (proof, test_info) = plonkupProve(
        n as usize,
        &prover_setup,
        &PlonkupProverSecret {
            secret: prover_secret,
        },
        &relation,
        &PlonkupWitness { w1, w2, w3 },
    );

    let mut byte_result = vec![
        serialize_point(proof.cmA),
        serialize_point(proof.cmB),
        serialize_point(proof.cmC),
        serialize_point(proof.cmF),
        serialize_point(proof.cmH1),
        serialize_point(proof.cmH2),
        serialize_point(proof.cmZ1),
        serialize_point(proof.cmZ2),
        serialize_point(proof.cmQlow),
        serialize_point(proof.cmQmid),
        serialize_point(proof.cmQhigh),
        serialize_point(proof.proof1),
        serialize_point(proof.proof2),
        serialize_scalar(proof.a_xi),
        serialize_scalar(proof.b_xi),
        serialize_scalar(proof.c_xi),
        serialize_scalar(proof.s1_xi),
        serialize_scalar(proof.s2_xi),
        serialize_scalar(proof.f_xi),
        serialize_scalar(proof.t_xi),
        serialize_scalar(proof.t_xi_tick),
        serialize_scalar(proof.z1_xi_tick),
        serialize_scalar(proof.z2_xi_tick),
        serialize_scalar(proof.h1_xi_tick),
        serialize_scalar(proof.h2_xi),
        serialize_scalar(proof.l1_xi),
        serialize_scalar_vec(proof.l_xi),
        serialize_scalar(test_info.omega),
        serialize_scalar(test_info.k1),
        serialize_scalar(test_info.k2),
        serialize_scalar_poly(test_info.qlX),
        serialize_scalar_poly(test_info.qrX),
        serialize_scalar_poly(test_info.qoX),
        serialize_scalar_poly(test_info.qmX),
        serialize_scalar_poly(test_info.qcX),
        serialize_scalar_poly(test_info.qkX),
        serialize_scalar_poly(test_info.t1X),
        serialize_scalar_poly(test_info.t2X),
        serialize_scalar_poly(test_info.t3X),
        serialize_scalar_poly(test_info.s1X),
        serialize_scalar_poly(test_info.s2X),
        serialize_scalar_poly(test_info.s3X),
        serialize_scalar_poly(test_info.aX),
        serialize_scalar_poly(test_info.bX),
        serialize_scalar_poly(test_info.cX),
        serialize_scalar_poly(test_info.piX),
        serialize_scalar_poly(test_info.tX),
        serialize_scalar_poly(test_info.z1X),
        serialize_scalar_poly(test_info.z2X),
        serialize_scalar_poly(test_info.fX),
        serialize_scalar_poly(test_info.h1X),
        serialize_scalar_poly(test_info.h2X),
        serialize_scalar_poly(test_info.zhX),
        serialize_scalar_poly(test_info.qX),
        serialize_scalar_poly(test_info.qlowX),
        serialize_scalar_poly(test_info.qmidX),
        serialize_scalar_poly(test_info.qhighX),
        serialize_scalar_poly(test_info.rX),
        serialize_scalar(test_info.alpha),
        serialize_scalar(test_info.beta),
        serialize_scalar(test_info.gamma),
        serialize_scalar(test_info.delta),
        serialize_scalar(test_info.epsilon),
        serialize_scalar(test_info.xi),
        serialize_scalar(test_info.zeta),
        serialize_scalar_poly(test_info.f_zeta),
        serialize_scalar_poly(test_info.t_zeta),
        serialize_scalar_poly(test_info.omegas),
        serialize_scalar_poly(test_info.omegas_tick),
        serialize_scalar_poly(test_info.grandProduct1),
        serialize_scalar_poly(test_info.grandProduct2),
        serialize_scalar_poly(test_info.w1),
        serialize_scalar_poly(test_info.w2),
        serialize_scalar_poly(test_info.w3),
    ]
    .concat();

    let result_len = byte_result.len();
    let ptr = libc::malloc(result_len + 8) as *mut u8;
    let mut res = result_len.to_le_bytes().to_vec();
    res.append(&mut byte_result);
    std::ptr::copy(res.as_ptr(), ptr as *mut u8, res.len());
    ptr as *const i8
}
