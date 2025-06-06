use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use core::slice;
use std::ops::Div;
use std::ops::Mul;

use crate::utils::{c_char, deserialize_vector_scalar_field, pack_scalar, unpack_scalar};

pub fn mul_fft(l: &[u8], r: &[u8]) -> Vec<u8> {
    let l = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(l));
    let r = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(r));

    l.mul(&r)
        .to_vec()
        .iter()
        .flat_map(|x| unpack_scalar(*x))
        .collect()
}

pub fn div_fft(l: &[u8], r: &[u8]) -> Vec<u8> {
    let l = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(l));
    let r = DensePolynomial::from_coefficients_vec(deserialize_vector_scalar_field(r));

    l.div(&r)
        .to_vec()
        .iter()
        .flat_map(|x| unpack_scalar(*x))
        .collect()
}

pub fn hmul(l: &[u8], r: &[u8]) -> Vec<u8> {
    let l = deserialize_vector_scalar_field(l);
    let r = deserialize_vector_scalar_field(r);

    l.iter()
        .zip(r)
        .map(|(x, y)| *x * y)
        .flat_map(unpack_scalar)
        .collect()
}

pub fn scalar_mul(l: &[u8], r: &[u8]) -> Vec<u8> {
    let a = pack_scalar(l).unwrap();
    let pv = deserialize_vector_scalar_field(r);

    pv.iter().map(|x| *x * a).flat_map(unpack_scalar).collect()
}

pub fn scalar_add(l: &[u8], r: &[u8]) -> Vec<u8> {
    let a = pack_scalar(l).unwrap();
    let pv = deserialize_vector_scalar_field(r);

    pv.iter().map(|x| *x + a).flat_map(unpack_scalar).collect()
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_scalar_add(
    l_var: *const c_char,
    l_len: usize,
    r_var: *const c_char,
    r_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let l = slice::from_raw_parts(l_var as *const u8, l_len);
    let r = slice::from_raw_parts(r_var as *const u8, r_len);

    let res = scalar_add(l, r);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_scalar_mul(
    l_var: *const c_char,
    l_len: usize,
    r_var: *const c_char,
    r_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let l = slice::from_raw_parts(l_var as *const u8, l_len);
    let r = slice::from_raw_parts(r_var as *const u8, r_len);

    let res = scalar_mul(l, r);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_hmul(
    l_var: *const c_char,
    l_len: usize,
    r_var: *const c_char,
    r_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let l = slice::from_raw_parts(l_var as *const u8, l_len);
    let r = slice::from_raw_parts(r_var as *const u8, r_len);

    let res = hmul(l, r);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_div_fft(
    l_var: *const c_char,
    l_len: usize,
    r_var: *const c_char,
    r_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let l = slice::from_raw_parts(l_var as *const u8, l_len);
    let r = slice::from_raw_parts(r_var as *const u8, r_len);

    let res = div_fft(l, r);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_mul_fft(
    l_var: *const c_char,
    l_len: usize,
    r_var: *const c_char,
    r_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let l = slice::from_raw_parts(l_var as *const u8, l_len);
    let r = slice::from_raw_parts(r_var as *const u8, r_len);

    let res = mul_fft(l, r);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}
