use std::ops::Mul;

use crate::utils::{binary, c_char, unary};
use crate::utils::{constant, Wrapper};
use ark_bls12_381::Fq12;
use ark_bls12_381::{Bls12_381, Config, Fr as ScalarField, G1Affine, G2Affine};
use ark_ec::pairing::{self, Pairing, PairingOutput};
use ark_ff::{
    AdditiveGroup, CubicExtConfig, CubicExtField, Field, PrimeField, QuadExtConfig, QuadExtField,
};
use num_traits::pow;

#[no_mangle]
pub unsafe extern "C" fn r_pairing(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &G1Affine, b: &G2Affine| -> Fq12 {
        Bls12_381::pairing(a, b).0
    })
}

#[no_mangle]
pub unsafe extern "C" fn r_gt_exp(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &Fq12, b: &ScalarField| -> Fq12 {
        a.pow(b.0)
    })
}

#[no_mangle]
pub unsafe extern "C" fn r_gt_exp_natural(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &Fq12, bytes: &[u64; 4]| -> Fq12 {
        a.pow(bytes)
    })
}

#[no_mangle]
pub unsafe extern "C" fn r_gt_mul(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &Fq12, b: &Fq12| -> Fq12 { a * b })
}

#[no_mangle]
pub unsafe extern "C" fn r_gt_one() -> *mut c_char {
    constant(Fq12::ONE)
}

#[no_mangle]
pub unsafe extern "C" fn r_gt_div(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &Fq12, b: &Fq12| -> Fq12 { a / b })
}

#[no_mangle]
pub unsafe extern "C" fn r_gt_invert(p1_ptr: *mut c_char) -> *mut c_char {
    unary(p1_ptr, |a: &Fq12| -> Fq12 {
        a.inverse().unwrap_or(Fq12::ZERO)
    })
}
