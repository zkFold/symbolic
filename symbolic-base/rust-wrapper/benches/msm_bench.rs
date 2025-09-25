use ark_bls12_381::Fr as ScalarField;
use ark_bls12_381::G1Affine;
use ark_ff::UniformRand;

use ark_poly::{univariate::DensePolynomial, DenseUVPolynomial};
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use rust_wrapper::utils::r_g1_free;
use rust_wrapper::utils::r_point_vec_free;
use rust_wrapper::utils::r_scalar_poly_free;
use rust_wrapper::{msm::r_msm, utils::poke};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("msm");
    group.sample_size(10);
    for size in 10..=20 {
        let length = 1 << size;
        let mut rng = &mut ark_std::test_rng();

        let scalars: DensePolynomial<ScalarField> =
            DensePolynomial::<ScalarField>::rand(length, &mut rng);
        let points: Vec<G1Affine> = (0..length).map(|_| G1Affine::rand(rng)).collect();

        group.bench_with_input(BenchmarkId::new("ArkMSM", size), &size, |b, _size| {
            b.iter(|| unsafe {
                let points = poke(points.clone());
                let scalars = poke(scalars.clone());
                let res = r_msm(points, scalars);
                r_g1_free(res);
                r_scalar_poly_free(scalars);
                r_point_vec_free(points);
            })
        });
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
