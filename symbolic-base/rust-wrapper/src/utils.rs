use std::slice;

use ark_bls12_381::{Fq12, Fr as ScalarField, G1Affine, G2Affine};
use ark_ff::PrimeField;
use ark_poly::{univariate::DensePolynomial, DenseUVPolynomial};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use cfg_if::cfg_if;
use num_bigint::BigUint;

// c_char platform-specific type alias
// Was taken from
// https://docs.rs/libc/0.2.172/src/libc/primitives.rs.html#20
cfg_if! {
    if #[cfg(all(
        not(windows),
        not(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "tvos",
            target_os = "watchos",
            target_os = "visionos",
        )),
        not(target_os = "vita"),
        any(
            target_arch = "aarch64",
            target_arch = "arm",
            target_arch = "csky",
            target_arch = "hexagon",
            target_arch = "msp430",
            target_arch = "powerpc",
            target_arch = "powerpc64",
            target_arch = "riscv32",
            target_arch = "riscv64",
            target_arch = "s390x",
            target_arch = "xtensa",
        )
    ))] {
        #[allow(non_camel_case_types)]
        pub type c_char = u8;
    } else {
        #[allow(non_camel_case_types)]
        pub type c_char = i8;
    }
}

pub struct Wrapper<T>(pub T);

fn deserialize_vector<T>(
    vector: &[u8],
    object_size: usize,
    deserialize: fn(&[u8]) -> T,
) -> Wrapper<Vec<T>> {
    Wrapper(vector.chunks_exact(object_size).map(deserialize).collect())
}

fn deserialize_vector_scalar_field(buffer: &[u8]) -> Vec<ScalarField> {
    deserialize_vector(buffer, std::mem::size_of::<ScalarField>(), pack_scalar).0
}

fn deserialize_vector_points(buffer: &[u8]) -> Vec<G1Affine> {
    deserialize_vector(buffer, G1Affine::identity().uncompressed_size(), pack_point).0
}

fn fix_point_vector(vector: &mut [u8]) {
    let len = vector.len();
    vector[0..len >> 1].reverse();
    vector[(len >> 1)..len].reverse();
}

fn pack_point(bytes: &[u8]) -> G1Affine {
    let mut bytes: Vec<u8> = bytes.to_vec();
    fix_point_vector(&mut bytes);
    G1Affine::deserialize_uncompressed_unchecked(&*bytes).unwrap()
}

fn unpack_point(r: &G1Affine) -> Vec<u8> {
    let mut res = Vec::new();
    r.serialize_uncompressed(&mut res).unwrap();
    fix_point_vector(&mut res);
    res
}

fn pack_scalar(bytes: &[u8]) -> ScalarField {
    PrimeField::from_le_bytes_mod_order(bytes)
}

pub fn unpack_scalar(scalar: &ScalarField) -> Vec<u8> {
    let mut v = BigUint::from(scalar.into_bigint()).to_bytes_le();
    v.resize(std::mem::size_of::<ScalarField>(), 0);
    v
}

unsafe fn h2r_ptr<T>(var: *const c_char, len: usize, pack: fn(&[u8]) -> T) -> *mut c_char {
    let buffer: &[u8] = slice::from_raw_parts(var as *const u8, len);
    let res: T = pack(buffer);
    poke(res)
}

unsafe fn r2h_ptr<T>(scalars_var: *mut c_char, out_ptr: *mut c_char, unpack: fn(&T) -> Vec<u8>) {
    let a = &*(scalars_var as *mut T);
    let res = unpack(a);
    std::ptr::copy(res.as_ptr(), out_ptr as *mut u8, res.len());
}

// Scalar

#[no_mangle]
pub unsafe fn r_h2r_scalar(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<ScalarField>(var, len, pack_scalar)
}

#[no_mangle]
pub unsafe fn r_r2h_scalar(var: *mut c_char, out_ptr: *mut c_char) {
    r2h_ptr::<ScalarField>(var, out_ptr, unpack_scalar);
}

// Point G1

#[no_mangle]
pub unsafe fn r_h2r_g1(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<G1Affine>(var, len, pack_point)
}

#[no_mangle]
pub unsafe fn r_r2h_g1(var: *mut c_char, out_ptr: *mut c_char) {
    r2h_ptr::<G1Affine>(var, out_ptr, unpack_point);
}

// Point G2

#[no_mangle]
pub unsafe fn r_h2r_g2(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<G2Affine>(var, len, |bytes| {
        G2Affine::deserialize_uncompressed_unchecked(bytes).unwrap()
    })
}

#[no_mangle]
pub unsafe fn r_r2h_g2(var: *mut c_char, out_ptr: *mut c_char) {
    r2h_ptr::<G2Affine>(var, out_ptr, |x| {
        let mut res = Vec::new();
        x.serialize_uncompressed(&mut res).unwrap();
        res
    });
}

// GT

#[no_mangle]
pub unsafe fn r_h2r_gt(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<Fq12>(var, len, |bytes| {
        Fq12::deserialize_uncompressed_unchecked(bytes).unwrap()
    })
}

#[no_mangle]
pub unsafe fn r_r2h_gt(var: *mut c_char, out_ptr: *mut c_char) {
    r2h_ptr::<Fq12>(var, out_ptr, |x| {
        let mut res = Vec::new();
        x.serialize_uncompressed(&mut res).unwrap();
        res
    });
}

// Scalar vec

#[no_mangle]
pub unsafe fn r_h2r_scalar_vec(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<Vec<ScalarField>>(var, len, deserialize_vector_scalar_field)
}

#[no_mangle]
pub unsafe fn r_r2h_scalar_vec(var: *mut c_char, out_ptr: *mut c_char) {
    r2h_ptr::<Vec<ScalarField>>(var, out_ptr, |x| x.iter().flat_map(unpack_scalar).collect());
}

// Scalar poly

#[no_mangle]
pub unsafe fn r_h2r_scalar_poly(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<DensePolynomial<ScalarField>>(var, len, |x| {
        DenseUVPolynomial::from_coefficients_vec(deserialize_vector_scalar_field(x))
    })
}

#[no_mangle]
pub unsafe fn r_r2h_scalar_poly(var: *mut c_char, out_ptr: *mut c_char) {
    r2h_ptr::<DensePolynomial<ScalarField>>(var, out_ptr, |x| {
        x.coeffs.iter().flat_map(unpack_scalar).collect()
    });
}

// Point vec

#[no_mangle]
pub unsafe fn r_h2r_point_vec(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<Vec<G1Affine>>(var, len, deserialize_vector_points)
}

#[no_mangle]
pub unsafe fn r_r2h_point_vec(var: *mut c_char, out_ptr: *mut c_char) {
    r2h_ptr::<Vec<G1Affine>>(var, out_ptr, |x| x.iter().flat_map(unpack_point).collect());
}

pub unsafe fn poke<T>(b: T) -> *mut c_char {
    Box::into_raw(Box::new(b)) as *mut c_char
}

#[no_mangle]
pub unsafe fn r_free_ptr(ptr: *mut c_char) {
    let _ = Box::from_raw(ptr);
}

pub unsafe fn constant<R>(a: R) -> *mut c_char {
    poke(a)
}

pub unsafe fn unary<T1: 'static, R>(a_ptr: *mut c_char, f: fn(a: &T1) -> R) -> *mut c_char {
    let a = &*(a_ptr as *mut T1);

    let res = f(a);

    poke(res)
}

pub unsafe fn binary<T1: 'static, T2: 'static, R>(
    a_ptr: *mut c_char,
    b_ptr: *mut c_char,
    f: fn(a: &T1, b: &T2) -> R,
) -> *mut c_char {
    let a = &*(a_ptr as *mut T1);
    let b = &*(b_ptr as *mut T2);

    let res = f(&a, &b);

    poke(res)
}

pub unsafe fn ternary<T1: 'static, T2: 'static, T3: 'static, R>(
    a_ptr: *mut c_char,
    b_ptr: *mut c_char,
    c_ptr: *mut c_char,
    f: fn(a: &T1, b: &T2, c: &T3) -> R,
) -> *mut c_char {
    let a = &*(a_ptr as *mut T1);
    let b = &*(b_ptr as *mut T2);
    let c = &*(c_ptr as *mut T3);

    let res = f(&a, &b, &c);

    poke(res)
}
