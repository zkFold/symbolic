use std::slice;

use crate::utils::{c_char, pack_scalar, unpack_scalar};

pub fn mul(s1_buffer: &[u8], s2_buffer: &[u8]) -> Vec<u8> {
    let s1 = pack_scalar(s1_buffer).unwrap();
    let s2 = pack_scalar(s2_buffer).unwrap();

    let r = s1 * s2;

    unpack_scalar(r)
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_mul(
    s1_var: *const c_char,
    s1_len: usize,
    s2_var: *const c_char,
    s2_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let s1_buffer = slice::from_raw_parts(s1_var as *const u8, s1_len);
    let s2_buffer = slice::from_raw_parts(s2_var as *const u8, s2_len);

    let res = mul(s1_buffer, s2_buffer);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}
