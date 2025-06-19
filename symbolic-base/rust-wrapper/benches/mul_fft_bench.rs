use ark_bls12_381::Fr as ScalarField;
use ark_ff::{BigInt, BigInteger};
use ark_poly::univariate::DensePolynomial;
use ark_poly::DenseUVPolynomial;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use rust_wrapper::poly::mul_fft;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("fft");

    for size in 10..=14 {
        let degree = 1 << size;

        let mut rng = &mut ark_std::test_rng();

        let l: DensePolynomial<ScalarField> =
            DensePolynomial::<ScalarField>::rand(degree, &mut rng);
        let r: DensePolynomial<ScalarField> =
            DensePolynomial::<ScalarField>::rand(degree, &mut rng);

        let r_bytes_vec: Vec<u8> = r
            .coeffs
            .iter()
            .flat_map(|i| (BigInt::from(*i)).to_bytes_le())
            .collect();

        let l_bytes_vec: Vec<u8> = l
            .coeffs
            .iter()
            .flat_map(|i| (BigInt::from(*i)).to_bytes_le())
            .collect();

        group.bench_with_input(
            BenchmarkId::new("FFT Multiplication", degree),
            &degree,
            |b, _size| {
                b.iter(|| {
                    mul_fft(&l_bytes_vec, &r_bytes_vec);
                })
            },
        );
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
