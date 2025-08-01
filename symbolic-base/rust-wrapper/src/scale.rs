use std::slice;

use ark_bls12_381::Fr as ScalarField;
use ark_bls12_381::G1Affine as GAffine;
use ark_bls12_381::G1Projective;
use ark_ec::CurveGroup;
use ark_ec::VariableBaseMSM;

use crate::utils::{
    c_char, deserialize_vector_points, deserialize_vector_scalar_field, pack_point, pack_scalar,
    unpack_point,
};

pub fn msm(scalar_buffer: &[u8], point_buffer: &[u8]) -> Vec<u8> {
    let scalars: Vec<_> = deserialize_vector_scalar_field(scalar_buffer);
    let points = deserialize_vector_points(point_buffer);
    let r: GAffine = G1Projective::msm_unchecked(&points, &scalars).into_affine();

    unpack_point(r)
}

pub fn scale(scalar_buffer: &[u8], point_buffer: &[u8]) -> Vec<u8> {
    let scalar: ScalarField = pack_scalar(scalar_buffer).unwrap();
    let point: GAffine = pack_point(point_buffer).unwrap();

    let r: GAffine = (point * scalar).into_affine();

    unpack_point(r)
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_msm(
    points_var: *const c_char,
    points_len: usize,
    scalars_var: *const c_char,
    scalars_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let scalar_buffer = slice::from_raw_parts(scalars_var as *const u8, scalars_len);
    let point_buffer = slice::from_raw_parts(points_var as *const u8, points_len);

    let res = msm(scalar_buffer, point_buffer);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}

///
/// # Safety
/// The caller must ensure that valid pointers and sizes are passed.
/// .
#[no_mangle]
pub unsafe extern "C" fn rust_wrapper_scale(
    points_var: *const c_char,
    points_len: usize,
    scalars_var: *const c_char,
    scalars_len: usize,
    _out_len: usize,
    out: *mut c_char,
) {
    let scalar_buffer = slice::from_raw_parts(scalars_var as *const u8, scalars_len);
    let point_buffer = slice::from_raw_parts(points_var as *const u8, points_len);

    let res = scale(scalar_buffer, point_buffer);

    std::ptr::copy(res.as_ptr(), out as *mut u8, res.len());
}
