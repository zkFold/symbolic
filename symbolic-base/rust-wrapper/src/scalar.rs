use crate::utils::{binary, c_char};
use ark_bls12_381::Fr as ScalarField;

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe fn r_scalar_add(s1_ptr: *mut c_char, s2_ptr: *mut c_char) -> *mut c_char {
    binary(
        s1_ptr,
        s2_ptr,
        |a: &ScalarField, b: &ScalarField| -> ScalarField { a + b },
    )
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe fn r_scalar_mul(s1_ptr: *mut c_char, s2_ptr: *mut c_char) -> *mut c_char {
    binary(
        s1_ptr,
        s2_ptr,
        |a: &ScalarField, b: &ScalarField| -> ScalarField { a * b },
    )
}
