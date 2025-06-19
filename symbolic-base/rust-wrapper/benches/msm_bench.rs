use ark_bls12_381::G1Affine;
use ark_ff::BigInteger;
use ark_msm::utils::generate_msm_inputs;
use ark_serialize::CanonicalSerialize;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use rust_wrapper::scale::msm;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("msm");
    group.sample_size(10);
    for size in 10..=20 {
        let (point_vec, scalar_vec) = generate_msm_inputs::<G1Affine>(1 << size);

        let points_bytes_vec: Vec<u8> = point_vec
            .iter()
            .flat_map(|a| {
                let mut vec = Vec::new();
                let _ = a.serialize_uncompressed(&mut vec);
                let point_size = 96;
                vec[0..(point_size >> 1)].reverse();
                vec[(point_size >> 1)..point_size].reverse();
                vec
            })
            .collect();

        let scalars_bytes_vec: Vec<u8> = scalar_vec
            .iter()
            .flat_map(BigInteger::to_bytes_le)
            .collect();

        group.bench_with_input(BenchmarkId::new("ArkMSM", size), &size, |b, _size| {
            b.iter(|| {
                msm(&scalars_bytes_vec, &points_bytes_vec);
            })
        });
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
