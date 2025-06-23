use std::slice;

use ark_bls12_381::{Fr as ScalarField, G1Affine as GAffine};
use ark_ff::PrimeField;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, SerializationError};
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

struct Buffer<T>(pub Vec<T>);

fn deserialize_vector<T>(
    vector: &[u8],
    object_size: usize,
    deserialize: fn(&[u8]) -> T,
) -> Buffer<T> {
    Buffer(vector.chunks_exact(object_size).map(deserialize).collect())
}

fn deserialize_vector_scalar_field(buffer: &[u8]) -> Vec<ScalarField> {
    deserialize_vector(buffer, std::mem::size_of::<ScalarField>(), pack_scalar).0
}

fn deserialize_vector_points(buffer: &[u8]) -> Vec<GAffine> {
    deserialize_vector(buffer, GAffine::identity().uncompressed_size(), pack_point).0
}

fn fix_point_vector(vector: &mut [u8]) {
    let len = vector.len();
    vector[0..len >> 1].reverse();
    vector[(len >> 1)..len].reverse();
}

fn pack_point(bytes: &[u8]) -> GAffine {
    let mut bytes: Vec<u8> = bytes.to_vec();
    fix_point_vector(&mut bytes);
    GAffine::deserialize_uncompressed_unchecked(&*bytes).unwrap()
}

fn unpack_point(r: &GAffine) -> Vec<u8> {
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

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
unsafe fn h2r_ptr<T>(var: *const c_char, len: usize, pack: fn(&[u8]) -> T) -> *mut c_char {
    let buffer: &[u8] = slice::from_raw_parts(var as *const u8, len);
    let res: T = pack(buffer);
    // println!("Rust  | HsPtrToRsPtr: {:?}", scalar);
    Box::into_raw(Box::new(res)) as *mut c_char
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
unsafe fn r2h_ptr<T>(scalars_var: *const c_char, out_ptr: *mut c_char, unpack: fn(&T) -> Vec<u8>) {
    let a = Box::from_raw(scalars_var as *mut T);
    let res = unpack(a.as_ref());
    let _ = Box::into_raw(a);
    std::ptr::copy(res.as_ptr(), out_ptr as *mut u8, res.len());
}

#[no_mangle]
pub unsafe fn r_h2r_scalar(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<ScalarField>(var, len, pack_scalar)
}

#[no_mangle]
pub unsafe fn r_r2h_scalar(var: *const c_char, out_ptr: *mut c_char) {
    r2h_ptr::<ScalarField>(var, out_ptr, unpack_scalar);
}

#[no_mangle]
pub unsafe fn r_h2r_point(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<GAffine>(var, len, pack_point)
}

#[no_mangle]
pub unsafe fn r_r2h_point(var: *const c_char, out_ptr: *mut c_char) {
    r2h_ptr::<GAffine>(var, out_ptr, unpack_point);
}

#[no_mangle]
pub unsafe fn r_h2r_scalar_vec(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<Vec<ScalarField>>(var, len, deserialize_vector_scalar_field)
}

#[no_mangle]
pub unsafe fn r_r2h_scalar_vec(var: *const c_char, out_ptr: *mut c_char) {
    r2h_ptr::<Vec<ScalarField>>(var, out_ptr, |x| x.iter().flat_map(unpack_scalar).collect());
}

#[no_mangle]
pub unsafe fn r_h2r_point_vec(var: *const c_char, len: usize) -> *mut c_char {
    h2r_ptr::<Vec<GAffine>>(var, len, deserialize_vector_points)
}

#[no_mangle]
pub unsafe fn r_r2h_point_vec(var: *const c_char, out_ptr: *mut c_char) {
    r2h_ptr::<Vec<GAffine>>(var, out_ptr, |x| x.iter().flat_map(unpack_point).collect());
}

pub unsafe fn peek<T>(ptr: *mut c_char) -> Box<T> {
    Box::from_raw(ptr as *mut T)
}

pub unsafe fn poke<T>(b: T) -> *mut c_char {
    Box::into_raw(Box::new(b)) as *mut c_char
}

pub unsafe fn drop<T>(b: Box<T>) {
    let _ = Box::into_raw(b);
}

pub unsafe fn binary<T1, T2, R>(
    a_ptr: *mut c_char,
    b_ptr: *mut c_char,
    f: fn(a: &T1, b: &T2) -> R,
) -> *mut c_char {
    let a = peek(a_ptr);
    let b = peek(b_ptr);

    let res = f(&a, &b);

    drop(a);
    drop(b);

    poke(res)
}
