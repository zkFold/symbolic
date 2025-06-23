use ark_bls12_381::G1Affine as GAffine;

use crate::utils::{binary, c_char};

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn r_point_add(p1_ptr: *mut c_char, p2_ptr: *mut c_char) -> *mut c_char {
    binary(p1_ptr, p2_ptr, |a: &GAffine, b: &GAffine| -> GAffine {
        (*a + b).into()
    })
}
