//! Halo2 PlonkUp Circuit Performance Benchmark
//!
//! This benchmark measures the performance of Halo2 circuit proving
//! with different numbers of PlonkUp constraints to analyze scaling behavior.
//!
//! Run with: `cargo bench --bench halo2_circuit_bench`

use blake2b_simd::{Params, State};
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use halo2_proofs::{
    plonk::{create_proof, keygen_pk, keygen_vk_with_k},
    poly::kzg::{params::ParamsKZG, KZGCommitmentScheme},
    transcript::{CircuitTranscript, Transcript},
};
use halo2curves::bls12381::{Bls12381, Fr};
use halo2curves::ff::FromUniformBytes;
use rand::rngs::OsRng;
use rust_wrapper::halo2::PlonkupCircuit;

/// Hash a byte slice to a field element
fn hash_to_field(data: &[u8]) -> Fr {
    let hash = Params::new()
        .hash_length(32)
        .to_state()
        .update(data)
        .finalize();

    // Convert hash to field element
    Fr::from_uniform_bytes(hash.as_array())
}

/// Generate a test circuit with the specified number of constraints
fn create_test_circuit(num_constraints: usize) -> PlonkupCircuit<Fr> {
    let mut circuit = PlonkupCircuit::<Fr>::new(num_constraints);

    // Fill circuit with multiplication chains where output of one becomes input to next
    for i in 0..num_constraints {
        // Generate field elements from hashes
        let seed_b = format!("witness_b_{}", i);
        let seed_const = format!("witness_const_{}", i);

        let w1 = if i == 0 {
            // First row: generate w1 from hash
            let seed_a = format!("witness_a_{}", i);
            hash_to_field(seed_a.as_bytes())
        } else {
            // Subsequent rows: w1[i] = w3[i-1]
            circuit.witness.w3[i - 1]
        };

        let w2 = hash_to_field(seed_b.as_bytes());
        let constant = hash_to_field(seed_const.as_bytes());
        let w3 = w1 * w2 + constant; // Multiplication result plus constant

        circuit.witness.w1[i] = w1;
        circuit.witness.w2[i] = w2;
        circuit.witness.w3[i] = w3;

        // Set selectors for multiplication gate with constant: qM * w1 * w2 + qO * w3 + qC = 0
        circuit.selectors.q_mul[i] = Fr::from(1); // Enable multiplication
        circuit.selectors.q_output[i] = -Fr::from(1); // Subtract output
        circuit.selectors.q_const[i] = constant; // Constant term

        // Add copy constraint for rows 1 and onwards: w3[i-1] = w1[i]
        if i > 0 {
            circuit.add_copy_constraint(2, i - 1, 0, i);
        }
    }

    circuit
}

/// Calculate appropriate k parameter for given circuit size
/// k determines the number of rows: usable_rows = 2^k - (blinding_factors + 1)
fn calculate_k_parameter(circuit_size: usize) -> u32 {
    let blinding_overhead = 8; // 7 blinding_factors
    let min_total_rows = circuit_size + blinding_overhead;
    let k = (min_total_rows as f64).log2().ceil() as u32;

    // Ensure minimum k=4 (16 usable rows)
    k.max(4)
}

/// Benchmark proving
fn bench_halo2_proving(c: &mut Criterion) {
    let mut group = c.benchmark_group("halo2_proving");
    group.sample_size(10); // Minimum sample size required by Criterion

    // Test realistic circuit sizes: 2^12 to 2^18 constraints
    let constraint_counts = vec![
        1 << 12, // 4,096 constraints
        1 << 13, // 8,192 constraints
        1 << 14, // 16,384 constraints
        1 << 15, // 32,768 constraints
        1 << 16, // 65,536 constraints
    ];

    for &num_constraints in &constraint_counts {
        group.throughput(Throughput::Elements(num_constraints as u64));

        group.bench_with_input(
            BenchmarkId::new("constraints", num_constraints),
            &num_constraints,
            |b, &size| {
                let circuit = create_test_circuit(size);
                let k = calculate_k_parameter(size);

                // Setup phase (not measured)
                let params = ParamsKZG::<Bls12381>::unsafe_setup(k, OsRng);
                let vk =
                    keygen_vk_with_k::<_, KZGCommitmentScheme<Bls12381>, _>(&params, &circuit, k)
                        .expect("Failed to generate verification key");
                let pk = keygen_pk(vk.clone(), &circuit).expect("Failed to generate proving key");

                b.iter(|| {
                    // Create proof
                    let mut transcript = CircuitTranscript::<State>::init();
                    let empty_instances: Vec<&[Fr]> = vec![];
                    create_proof::<Fr, KZGCommitmentScheme<Bls12381>, _, _>(
                        &params,
                        &pk,
                        &[circuit.clone()],
                        &[&empty_instances[..]],
                        OsRng,
                        &mut transcript,
                    )
                    .expect("Failed to create proof");
                    let proof = transcript.finalize();
                    black_box(proof);
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, bench_halo2_proving);

criterion_main!(benches);
