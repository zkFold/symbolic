use std::slice;

use ark_bls12_381::Fr as ScalarField;
use ark_bls12_381::G1Affine;
use ark_bls12_381::G1Projective;
use ark_ec::CurveGroup;
use ark_ec::VariableBaseMSM;
use ark_poly::univariate::DensePolynomial;

use crate::utils::binary;
use crate::utils::c_char;

#[no_mangle]
pub unsafe extern "C" fn r_msm(scalars_ptr: *mut c_char, points_ptr: *mut c_char) -> *mut c_char {
    binary(
        scalars_ptr,
        points_ptr,
        |scalars: &DensePolynomial<ScalarField>, points: &Vec<G1Affine>| -> G1Affine {
            G1Projective::msm_unchecked(points, &scalars.coeffs).into_affine()
        },
    )
}
