use std::slice;

use ark_bls12_381::G1Affine as GAffine;
use ark_ec::CurveGroup;

use crate::utils::{c_char, pack_point, unpack_point};

pub fn sum(point1_buffer: &[u8], point2_buffer: &[u8]) -> Vec<u8> {
    let p1 = pack_point(point1_buffer).unwrap();
    let p2 = pack_point(point2_buffer).unwrap();

    let r: GAffine = (p1 + p2).into_affine();

    unpack_point(r)
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_sum(
    point1_var: *const c_char,
    point1_len: usize,
    point2_var: *const c_char,
    point2_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let p1_buffer = slice::from_raw_parts(point1_var as *const u8, point1_len);
    let p2_buffer = slice::from_raw_parts(point2_var as *const u8, point2_len);

    let res = sum(p1_buffer, p2_buffer);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}
