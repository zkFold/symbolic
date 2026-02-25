use core::slice;
use std::{marker::PhantomData};

use crate::{halo2::{PlonkupCircuit, PlonkupSelectorData, PlonkupTableData, PlonkupWitnessData}, utils::{c_char, Wrapper}};
use ark_bls12_381::Fr as ScalarField;
use ark_ff::{AdditiveGroup, Field, PrimeField, UniformRand, Zero};
use halo2_proofs::circuit;
use halo2curves::ff::helpers::sqrt_ratio_generic;
use num_traits::zero;
use subtle::{Choice, CtOption};
// pub use ark_ff_macros;
// pub use num_traits::{One, Zero};
// use zeroize::Zeroize;

// pub mod utils;

// #[macro_use]
// pub mod arithmetic;

// #[macro_use]
// pub mod models;
// pub use self::models::*;

// pub mod field_hashers;

// mod prime;
// // pub use prime::*;

// mod fft_friendly;
// pub use fft_friendly::*;

// mod cyclotomic;
// pub use cyclotomic::*;

// mod sqrt;
// pub use sqrt::*;

trait Storable {
    fn sizeof(a: &Self) -> u64;

    unsafe fn peek_ptr(ptr: *mut c_char) -> Self;

    unsafe fn poke_ptr(ptr: *mut c_char, a: &Self);
}

impl Storable for ScalarField {
    fn sizeof(_: &ScalarField) -> u64 {
        32
    }

    unsafe fn peek_ptr(ptr: *mut c_char) -> ScalarField {
        let buffer = slice::from_raw_parts(ptr as *const u8, 32);
        PrimeField::from_le_bytes_mod_order(buffer)
    }

    unsafe fn poke_ptr(ptr: *mut c_char, a: &ScalarField) {
        todo!()
    }
}

impl<T: Storable> Storable for Vec<T> {

    fn sizeof(a: &Vec<T>) -> u64 {
        8 + a.iter().map(|i| T::sizeof(&i)).sum::<u64>()
    }


    unsafe fn peek_ptr<'a>(ptr: *mut c_char) -> Vec<T> {
        let len_bytes = slice::from_raw_parts(ptr as *const u8, size_of::<u64>());
        let len = u64::from_le_bytes(len_bytes.try_into().unwrap());
        let mut v = Vec::new();
        v.reserve(len as usize);
        let mut off: u64 = 8;
        for _ in 0..len {
            let obj = T::peek_ptr(ptr.wrapping_add(off as usize));
            off += T::sizeof(&obj);
            v.push(obj);
        }
        v
    }

    unsafe fn poke_ptr<'a>(ptr: *mut c_char, a: &Vec<T>) {
        todo!()
    }
}

unsafe fn peek_with_offset<T: Storable>(ptr: *mut c_char) -> (T, *mut c_char) {
    let res = T::peek_ptr(ptr);
    let off = T::sizeof(&res) as usize;
    (res, ptr.wrapping_byte_add(off))
}

impl<T: Storable> Storable for PlonkupWitnessData<T> {
    fn sizeof(a: &Self) -> u64 {
        let PlonkupWitnessData{ w1, w2, w3} = a;
        [w1, w2, w3].iter().map(|x| <Vec<T>>::sizeof(*x)).sum::<u64>()
    }

    unsafe fn peek_ptr(ptr: *mut c_char) -> Self {
        let (w1, ptr) = peek_with_offset(ptr);
        let (w2, ptr) = peek_with_offset(ptr);
        let (w3, _ptr) = peek_with_offset(ptr);

        PlonkupWitnessData{ w1, w2, w3}
    }

    unsafe fn poke_ptr(ptr: *mut c_char, a: &Self) {
        todo!()
    }
}

impl<T: Storable> Storable for PlonkupSelectorData<T> {
    fn sizeof(a: &Self) -> u64 {
        let PlonkupSelectorData{q_mul, q_left, q_right, q_output, q_const, q_lookup} = a;
        [q_mul, q_left, q_right, q_output, q_const, q_lookup].iter().map(|x| <Vec<T>>::sizeof(*x)).sum::<u64>()
    }

    unsafe fn peek_ptr(ptr: *mut c_char) -> Self {
        let (q_mul, ptr) = peek_with_offset(ptr);
        let (q_left, ptr) = peek_with_offset(ptr);
        let (q_right, ptr) = peek_with_offset(ptr);
        let (q_output, ptr) = peek_with_offset(ptr);
        let (q_const, ptr) = peek_with_offset(ptr);
        let (q_lookup, _ptr) = peek_with_offset(ptr);

        PlonkupSelectorData{ q_mul, q_left, q_right, q_output, q_const, q_lookup }
    }

    unsafe fn poke_ptr(ptr: *mut c_char, a: &Self) {
        todo!()
    }
}

impl<T: Storable> Storable for PlonkupTableData<T> {
    fn sizeof(a: &Self) -> u64 {
        let PlonkupTableData{ t1, t2, t3} = a;
        [t1, t2, t3].iter().map(|x| <Vec<T>>::sizeof(*x)).sum::<u64>()
    }

    unsafe fn peek_ptr(ptr: *mut c_char) -> Self {
        let (t1, ptr) = peek_with_offset(ptr);
        let (t2, ptr) = peek_with_offset(ptr);
        let (t3, _ptr) = peek_with_offset(ptr);

        PlonkupTableData { t1, t2, t3 }
    }

    unsafe fn poke_ptr(ptr: *mut c_char, a: &Self) {
        todo!()
    }
}

impl Storable for u64 {
    fn sizeof(_: &Self) -> u64 {
        8
    }

    unsafe fn peek_ptr(ptr: *mut c_char) -> Self {
        let len_bytes = slice::from_raw_parts(ptr as *const u8, 8);
        u64::from_le_bytes(len_bytes.try_into().unwrap())
    }

    unsafe fn poke_ptr(ptr: *mut c_char, a: &Self) {
        let res = u64::to_le_bytes(*a);
        std::ptr::copy(res.as_ptr(), ptr as *mut u8, res.len());

    }
}

impl<T: Storable> Storable for (T, T, T, T) {
    fn sizeof(a: &Self) -> u64 {
        let (a, b, c, d) = a;
        [a, b, c, d].iter().map(|x|T::sizeof(x)).sum()
    }

    unsafe fn peek_ptr(ptr: *mut c_char) -> Self {
        let (a, ptr) = peek_with_offset(ptr);
        let (b, ptr) = peek_with_offset(ptr);
        let (c, ptr) = peek_with_offset(ptr);
        let (d, _) = peek_with_offset(ptr);
        (a, b, c, d)
    }

    unsafe fn poke_ptr(ptr: *mut c_char, a: &Self) {
        todo!()
    }
}

impl<T: Storable + halo2curves::ff::Field> Storable for PlonkupCircuit<T> {
    fn sizeof(a: &Self) -> u64 {
        let PlonkupCircuit{circuit_size, witness, selectors, table, copy_constraints, .. } = a;
        8 + Storable::sizeof(witness) + Storable::sizeof(selectors) + Storable::sizeof(table) + Storable::sizeof(&copy_constraints.iter().map(|(a, b, c, d)| {*a as u64; *b as u64; *c as u64; *d as u64}).collect::<Vec<_>>())
    }

    unsafe fn peek_ptr(ptr: *mut c_char) -> Self {
        let (circuit_size, ptr): (u64, _) = peek_with_offset(ptr);
        let circuit_size = circuit_size as usize;
        let (witness, ptr) = peek_with_offset(ptr);
        let (selectors, ptr) = peek_with_offset(ptr);
        let (table, ptr) = peek_with_offset(ptr);
        let (copy_constraints, _): (Vec<(u64, u64, u64, u64)>, _) = peek_with_offset(ptr);
        let copy_constraints = copy_constraints.iter().map(|(a, b, c, d)| (*a as usize, *b as usize, *c as usize, *d as usize)).collect::<Vec<_>>();
        PlonkupCircuit {circuit_size, witness, selectors, table, copy_constraints, marker: PhantomData }
    }

    unsafe fn poke_ptr(ptr: *mut c_char, a: &Self) {
        todo!()
    }
}

// pub struct ScalarFieldAlias = ScalarField;

impl halo2curves::ff::Field for Wrapper<ScalarField> {
    const ZERO: Self = Wrapper(<ScalarField as AdditiveGroup>::ZERO);

    const ONE: Self = Wrapper(<ScalarField as Field>::ONE);

    fn random(rng: impl rand::RngCore) -> Self {
        Wrapper(        
            loop {
            let mut raw = [0u64; 4];
            for int in raw.iter_mut() {
                *int = rng.next_u64();
            }

            // Mask away the unused most-significant bits.
            raw[3] &= 0xffffffffffffffff >> 1;

            if let Some(scalar) = ScalarField::from_le_bytes_mod_order(&raw).into() {
                return scalar;
            }
        })
    }

    fn square(&self) -> Self {
        <ScalarField as Field>::square(self)
    }

    fn double(&self) -> Self {
        let mut out = *self;
        out += self;
        out
    }

    fn invert(&self) -> CtOption<Self> {
        let r = <ScalarField as Field>::inverse(self);
        match r {
            Some(r) => CtOption::new(r, Choice::from(1)),
            None => CtOption::new(ScalarField::zero(), Choice::from(0)),
        }
        
    }

    fn sqrt_ratio(num: &Self, div: &Self) -> (Choice, Self) {
        sqrt_ratio_generic(num, div)
    }
}

#[no_mangle]
pub unsafe extern "C" fn r_halo2_prove(
    ptr: *mut c_char,
) {
    let a: PlonkupCircuit<ScalarField> = <PlonkupCircuit<ScalarField>>::peek_ptr(ptr);
    println!("Rust Plonkup circuit {:?}", a);
}