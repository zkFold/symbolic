use ark_bls12_381::Fr as ScalarField;
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use rust_wrapper::{
    poly::r_poly_div,
    utils::{poke, r_scalar_poly_free},
};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("fft");

    for size in 10..=18 {
        let degree = 1 << size;

        let mut rng = &mut ark_std::test_rng();

        let l: DensePolynomial<ScalarField> =
            DensePolynomial::<ScalarField>::rand(degree, &mut rng);
        let r: DensePolynomial<ScalarField> =
            DensePolynomial::<ScalarField>::rand(degree, &mut rng);

        group.bench_with_input(
            BenchmarkId::new("FFT Division", degree),
            &degree,
            |b, _size| {
                b.iter(|| unsafe {
                    let l = poke(l.clone());
                    let r = poke(r.clone());
                    let res = r_poly_div(l, r);
                    r_scalar_poly_free(res);
                    r_scalar_poly_free(l);
                    r_scalar_poly_free(r);
                })
            },
        );
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
