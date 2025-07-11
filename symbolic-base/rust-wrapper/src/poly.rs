use ark_bls12_381::Fr as ScalarField;
use ark_ff::{FftField, Field, One, PrimeField, Zero};
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use num_bigint::BigUint;
use num_traits::pow;
use std::ops::{Div, Mul, Neg};

use crate::utils::ternary;
use crate::utils::Wrapper;
use crate::utils::{binary, c_char, constant, unary};

#[no_mangle]
pub unsafe fn r_poly_add(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |a: &DensePolynomial<ScalarField>,
         b: &DensePolynomial<ScalarField>|
         -> DensePolynomial<ScalarField> { a + b },
    )
}

#[no_mangle]
pub unsafe fn r_poly_sub(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |a: &DensePolynomial<ScalarField>,
         b: &DensePolynomial<ScalarField>|
         -> DensePolynomial<ScalarField> { a - b },
    )
}

#[no_mangle]
pub unsafe fn r_poly_mul(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |a: &DensePolynomial<ScalarField>,
         b: &DensePolynomial<ScalarField>|
         -> DensePolynomial<ScalarField> { a * b },
    )
}

#[no_mangle]
pub unsafe fn r_poly_div(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |a: &DensePolynomial<ScalarField>,
         b: &DensePolynomial<ScalarField>|
         -> DensePolynomial<ScalarField> { a.div(b) },
    )
}

#[no_mangle]
pub unsafe fn r_poly_hmul(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |a: &DensePolynomial<ScalarField>,
         b: &DensePolynomial<ScalarField>|
         -> DensePolynomial<ScalarField> {
            DensePolynomial::from_coefficients_vec(
                a.coeffs
                    .iter()
                    .zip(&b.coeffs)
                    .map(|(ax, bx)| ax * bx)
                    .collect(),
            )
        },
    )
}

#[no_mangle]
pub unsafe fn r_poly_hdiv(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |a: &DensePolynomial<ScalarField>,
         b: &DensePolynomial<ScalarField>|
         -> DensePolynomial<ScalarField> {
            DensePolynomial::from_coefficients_vec(
                a.coeffs
                    .iter()
                    .zip(&b.coeffs)
                    .map(|(ax, bx)| ax / bx)
                    .collect(),
            )
        },
    )
}

#[no_mangle]
pub unsafe fn r_poly_mul_scalar(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |s: &ScalarField, p: &DensePolynomial<ScalarField>| -> DensePolynomial<ScalarField> {
            DensePolynomial::from_coefficients_vec(p.coeffs.iter().map(|i| s * i).collect())
        },
    )
}

#[no_mangle]
pub unsafe fn r_poly_add_scalar(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |s: &ScalarField, p: &DensePolynomial<ScalarField>| -> DensePolynomial<ScalarField> {
            DensePolynomial::from_coefficients_vec(p.coeffs.iter().map(|i| s + i).collect())
        },
    )
}

pub fn poly_one() -> DensePolynomial<ScalarField> {
    DensePolynomial {
        coeffs: vec![ScalarField::ONE],
    }
}

#[no_mangle]
pub unsafe fn r_poly_one() -> *mut c_char {
    constant(poly_one())
}

// struct DensePolynomialWrapper<T: ark_ff::Field>(pub DensePolynomial<T>);

impl<T> Mul for Wrapper<DensePolynomial<T>>
where
    T: ark_ff::Field,
    DensePolynomial<T>: Mul<DensePolynomial<T>, Output = DensePolynomial<T>>,
{
    type Output = Wrapper<DensePolynomial<T>>;

    fn mul(self, rhs: Self) -> Self::Output {
        Wrapper(self.0 * rhs.0)
    }
}

impl<T: FftField> One for Wrapper<DensePolynomial<T>> {
    fn one() -> Self {
        Wrapper(DensePolynomial {
            coeffs: vec![T::ONE],
        })
    }
}

impl<T: FftField> Clone for Wrapper<DensePolynomial<T>> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[no_mangle]
pub unsafe fn r_poly_exp(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |base: &DensePolynomial<ScalarField>,
         exp_bytes: &[u8; 32]|
         -> DensePolynomial<ScalarField> {
            let exp = BigUint::from_bytes_le(exp_bytes);
            #[cfg(target_pointer_width = "64")]
            let digits = exp
                .to_u64_digits()
                .iter()
                .map(|x| *x as usize)
                .collect::<Vec<_>>();
            #[cfg(target_pointer_width = "32")]
            let digits = exp
                .to_u32_digits()
                .iter()
                .map(|x| *x as usize)
                .collect::<Vec<_>>();
            match digits.len() {
                0 => poly_one(),
                1 => pow(Wrapper { 0: base.clone() }, digits[0]).0,
                _ => digits
                    .into_iter()
                    .map(|i| pow(Wrapper { 0: base.clone() }, i).0)
                    .fold(poly_one(), |acc, e| {
                        pow(Wrapper { 0: acc.clone() }, usize::MAX).0 * acc * e
                    }),
            }
        },
    )
}

#[no_mangle]
pub unsafe fn r_poly_negate(a_ptr: *mut c_char) -> *mut c_char {
    unary(
        a_ptr,
        |p: &DensePolynomial<ScalarField>| -> DensePolynomial<ScalarField> { p.clone().neg() },
    )
}

#[no_mangle]
pub unsafe fn r_poly_zero() -> *mut c_char {
    constant(DensePolynomial::<ScalarField>::zero())
}

#[no_mangle]
pub unsafe fn r_poly_scale_natural(a_ptr: *mut c_char, b_ptr: *mut c_char) -> *mut c_char {
    binary(
        a_ptr,
        b_ptr,
        |a_bytes: &[u8; 32], b: &DensePolynomial<ScalarField>| -> DensePolynomial<ScalarField> {
            let c = ScalarField::from_le_bytes_mod_order(a_bytes);

            b.mul(c)
        },
    )
}

#[no_mangle]
pub unsafe fn r_poly_from_natural(a_ptr: *mut c_char) -> *mut c_char {
    unary(a_ptr, |a: &[u8; 32]| -> DensePolynomial<ScalarField> {
        DensePolynomial {
            coeffs: vec![ScalarField::from_le_bytes_mod_order(a)],
        }
    })
}

#[no_mangle]
pub unsafe fn r_poly_div_shifted_mono(
    a_ptr: *mut c_char,
    b_ptr: *mut c_char,
    c_ptr: *mut c_char,
) -> *mut c_char {
    ternary(
        a_ptr,
        b_ptr,
        c_ptr,
        |cs: &DensePolynomial<ScalarField>,
         m_bytes: &[u8; 32],
         b: &ScalarField|
         -> DensePolynomial<ScalarField> {
            let m = BigUint::from_bytes_le(m_bytes);
            // let exp = (exp_bytes);
            #[cfg(target_pointer_width = "64")]
            let digits = m
                .to_u64_digits()
                .iter()
                .map(|x| *x as usize)
                .collect::<Vec<_>>();
            #[cfg(target_pointer_width = "32")]
            let digits = m
                .to_u32_digits()
                .iter()
                .map(|x| *x as usize)
                .collect::<Vec<_>>();
            match digits.len() {
                0 => cs * (ScalarField::ONE / (ScalarField::ONE + b)),
                1 => {
                    let int_len = cs.coeffs.len();
                    let int_m = digits[0];

                    let mut c = cs.coeffs.clone();
                    let mut res = vec![ScalarField::zero(); int_len];

                    for ix in (int_len - 1)..=int_m {
                        let ci = c[ix];
                        res[ix - int_m] = ci;
                        c[ix - int_m] -= ci * b;
                    }
                    return DensePolynomial { coeffs: res };
                }
                _ => panic!(),
            }
        },
    )
}
