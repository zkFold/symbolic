use crate::utils::{binary, c_char, constant, unary};
use ark_bls12_381::Fr as ScalarField;
use ark_ff::{AdditiveGroup, Field, PrimeField};
use std::ops::Neg;

#[no_mangle]
pub unsafe fn r_scalar_add(s1_ptr: *mut c_char, s2_ptr: *mut c_char) -> *mut c_char {
    binary(
        s1_ptr,
        s2_ptr,
        |a: &ScalarField, b: &ScalarField| -> ScalarField { a + b },
    )
}

#[no_mangle]
pub unsafe fn r_scalar_mul(s1_ptr: *mut c_char, s2_ptr: *mut c_char) -> *mut c_char {
    binary(
        s1_ptr,
        s2_ptr,
        |a: &ScalarField, b: &ScalarField| -> ScalarField { a * b },
    )
}

#[no_mangle]
pub unsafe fn r_scalar_scale_natural(s1_ptr: *mut c_char, s2_ptr: *mut c_char) -> *mut c_char {
    binary(
        s1_ptr,
        s2_ptr,
        |bytes: &[u8; 32], b: &ScalarField| -> ScalarField {
            let scalar = ScalarField::from_le_bytes_mod_order(bytes);

            scalar * b
        },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_scalar_zero() -> *mut c_char {
    constant(ScalarField::ZERO)
}

#[no_mangle]
pub unsafe extern "C" fn r_scalar_sub(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(
        p1_ptr,
        p2_ptr,
        |a: &ScalarField, b: &ScalarField| -> ScalarField { a - b },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_scalar_negate(p1_ptr: *mut c_char) -> *mut c_char {
    unary(p1_ptr, |a: &ScalarField| -> ScalarField { a.clone().neg() })
}

#[no_mangle]
pub unsafe extern "C" fn r_scalar_one() -> *mut c_char {
    constant(ScalarField::ONE)
}

#[no_mangle]
pub unsafe fn r_scalar_from_natural(a_ptr: *mut c_char) -> *mut c_char {
    unary(a_ptr, |a: &[u8; 32]| -> ScalarField {
        ScalarField::from_le_bytes_mod_order(a)
    })
}

#[no_mangle]
pub unsafe fn r_scalar_exp_natural(s1_ptr: *mut c_char, s2_ptr: *mut c_char) -> *mut c_char {
    binary(
        s1_ptr,
        s2_ptr,
        |a: &ScalarField, bytes: &[u64; 4]| -> ScalarField { a.pow(bytes) },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_scalar_div(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(
        p1_ptr,
        p2_ptr,
        |a: &ScalarField, b: &ScalarField| -> ScalarField {
            if *b == ScalarField::ZERO {
                return ScalarField::ZERO;
            }
            a / b
        },
    )
}

#[no_mangle]
pub unsafe extern "C" fn r_scalar_invert(p1_ptr: *mut c_char) -> *mut c_char {
    unary(p1_ptr, |a: &ScalarField| -> ScalarField {
        a.inverse().unwrap_or(ScalarField::ZERO)
    })
}
