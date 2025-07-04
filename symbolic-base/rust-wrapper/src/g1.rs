use ark_bls12_381::{Fr as ScalarField, G1Affine};
use ark_ec::AffineRepr;
use ark_ff::PrimeField;
use std::ops::Neg;

use crate::utils::{binary, c_char, constant, unary};

#[no_mangle]
pub unsafe extern "C" fn r_g1_add(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &G1Affine, b: &G1Affine| -> G1Affine {
        (*a + b).into()
    })
}

#[no_mangle]
pub unsafe extern "C" fn r_g1_scale(
    scalar_ptr: *mut c_char,
    point_ptr: *mut c_char,
) -> *mut c_char {
    binary(
        scalar_ptr,
        point_ptr,
        |scalar: &ScalarField, point: &G1Affine| -> G1Affine { (*point * scalar).into() },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_g1_scale_natural(
    p1_ptr: *mut c_char,
    p2_ptr: *mut c_char,
) -> *mut c_char {
    binary(
        p1_ptr,
        p2_ptr,
        |bytes: &[u8; 32], point: &G1Affine| -> G1Affine {
            let scalar = ScalarField::from_le_bytes_mod_order(bytes);
            (*point * scalar).into()
        },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_g1_zero() -> *mut c_char {
    constant(G1Affine::zero())
}

#[no_mangle]
pub unsafe extern "C" fn r_g1_sub(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &G1Affine, b: &G1Affine| -> G1Affine {
        (*a - b).into()
    })
}

#[no_mangle]
pub unsafe extern "C" fn r_g1_negate(p1_ptr: *mut c_char) -> *mut c_char {
    unary(p1_ptr, |a: &G1Affine| -> G1Affine { a.clone().neg() })
}

#[no_mangle]
pub unsafe extern "C" fn r_g1_gen() -> *mut c_char {
    constant(G1Affine::generator())
}
