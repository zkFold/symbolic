use ark_bls12_381::{Fr as ScalarField, G2Affine};
use ark_ec::AffineRepr;
use ark_ff::PrimeField;
use std::ops::Neg;

use crate::utils::{binary, c_char, constant, unary};

#[no_mangle]
pub unsafe extern "C" fn r_g2_add(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &G2Affine, b: &G2Affine| -> G2Affine {
        (*a + b).into()
    })
}

#[no_mangle]
pub unsafe extern "C" fn r_g2_scale_natural(
    p1_ptr: *mut c_char,
    p2_ptr: *mut c_char,
) -> *mut c_char {
    binary(
        p1_ptr,
        p2_ptr,
        |bytes: &[u8; 32], point: &G2Affine| -> G2Affine {
            let scalar = ScalarField::from_le_bytes_mod_order(bytes);
            (*point * scalar).into()
        },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_g2_zero() -> *mut c_char {
    constant(G2Affine::zero())
}

#[no_mangle]
pub unsafe extern "C" fn r_g2_sub(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &G2Affine, b: &G2Affine| -> G2Affine {
        (*a - b).into()
    })
}

#[no_mangle]
pub unsafe extern "C" fn r_g2_negate(p1_ptr: *mut c_char) -> *mut c_char {
    unary(p1_ptr, |a: &G2Affine| -> G2Affine { a.clone().neg() })
}

#[no_mangle]
pub unsafe extern "C" fn r_g2_scale(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(
        p1_ptr,
        p2_ptr,
        |scalar: &ScalarField, point: &G2Affine| -> G2Affine { (*point * scalar).into() },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_g2_gen() -> *mut c_char {
    constant(G2Affine::generator())
}
