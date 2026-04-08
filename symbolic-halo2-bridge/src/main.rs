mod imported_circuit;
mod ir;

use anyhow::{bail, Context, Result};
use blstrs::{Bls12, G1Projective, Scalar};
use halo2_proofs::plonk::{create_proof, keygen_pk, keygen_vk_with_k, prepare, ProvingKey, VerifyingKey};
use halo2_proofs::poly::commitment::{Guard, PolynomialCommitmentScheme};
use halo2_proofs::poly::gwc_kzg::GwcKZGCommitmentScheme;
use halo2_proofs::poly::kzg::params::{ParamsKZG, ParamsVerifierKZG};
use halo2_proofs::poly::kzg::KZGCommitmentScheme;
use halo2_proofs::transcript::{CircuitTranscript, Transcript};
use imported_circuit::ImportedCircuit;
use ir::ImportedCircuitIr;
use plutus_halo2_verifier_gen::kzg_params::get_or_create_kzg_params;
use plutus_halo2_verifier_gen::plutus_gen::{
    export_proof, export_public_inputs, generate_aiken_verifier, generate_plinth_verifier,
    serialize_proof, CardanoFriendlyBlake2b, ExtractPCS,
};
use rand::prelude::StdRng;
use rand_core::SeedableRng;
use std::env;
use std::fs::{self, File};
use std::path::{Path, PathBuf};

type Params = ParamsKZG<Bls12>;
type ParamsVK = ParamsVerifierKZG<Bls12>;
type CTranscript = CircuitTranscript<CardanoFriendlyBlake2b>;

#[derive(Clone, Copy, Debug)]
enum Flavor {
    Halo2,
    Gwc19,
}

fn main() -> Result<()> {
    env_logger::init_from_env(env_logger::Env::default().filter_or("RUST_LOG", "info"));

    let mut args: Vec<String> = env::args().skip(1).collect();
    let flavor = if args.last().map(|s| s == "gwc_kzg").unwrap_or(false) {
        args.pop();
        Flavor::Gwc19
    } else {
        Flavor::Halo2
    };

    match args.as_slice() {
        [cmd, circuit_cbor] if cmd == "prove" => dispatch_prove(Path::new(circuit_cbor), flavor),
        [cmd, circuit_cbor, proof_hex] if cmd == "verify" => {
            dispatch_verify(Path::new(circuit_cbor), Path::new(proof_hex), flavor)
        }
        [cmd, circuit_cbor] if cmd == "gen-plinth" => {
            dispatch_gen_plinth(Path::new(circuit_cbor), flavor)
        }
        [cmd, circuit_cbor] if cmd == "gen-aiken" => {
            dispatch_gen_aiken(Path::new(circuit_cbor), flavor)
        }
        _ => {
            eprintln!("Usage:");
            eprintln!("  symbolic-halo2-bridge prove <circuit.cbor> [gwc_kzg]");
            eprintln!("  symbolic-halo2-bridge verify <circuit.cbor> <proof.hex> [gwc_kzg]");
            eprintln!("  symbolic-halo2-bridge gen-plinth <circuit.cbor> [gwc_kzg]");
            eprintln!("  symbolic-halo2-bridge gen-aiken <circuit.cbor> [gwc_kzg]");
            bail!("invalid command line")
        }
    }
}

fn dispatch_prove(circuit_cbor: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => prove_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_cbor),
        Flavor::Gwc19 => prove_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_cbor),
    }
}

fn dispatch_verify(circuit_cbor: &Path, proof_hex: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => verify_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_cbor, proof_hex),
        Flavor::Gwc19 => verify_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_cbor, proof_hex),
    }
}

fn dispatch_gen_plinth(circuit_cbor: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => gen_plinth_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_cbor),
        Flavor::Gwc19 => gen_plinth_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_cbor),
    }
}

fn dispatch_gen_aiken(circuit_cbor: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => gen_aiken_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_cbor),
        Flavor::Gwc19 => gen_aiken_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_cbor),
    }
}

fn prove_with_pcs<PCS>(circuit_cbor: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let ir = ImportedCircuitIr::from_path(circuit_cbor)?;
    let circuit = ImportedCircuit::new(ir.clone());

    let seed = [0u8; 32];
    let mut rng: StdRng = SeedableRng::from_seed(seed);

    let k = k_from_ir(&ir)?;
    let params = get_or_create_kzg_params(k, rng.clone())?;
    let vk: VerifyingKey<Scalar, PCS> = keygen_vk_with_k(&params, &circuit, k)?;
    let pk: ProvingKey<Scalar, PCS> = keygen_pk(vk.clone(), &circuit)?;

    let compact_public = ir.compact_public_inputs()?;
    let paths = derive_output_paths(circuit_cbor);

    let proof =
        create_proof_bytes::<PCS>(&params, &pk, circuit.clone(), &compact_public, &mut rng)?;
    verify_proof_bytes::<PCS>(&params, &vk, &compact_public, &proof)?;

    fs::create_dir_all(paths.proof_hex.parent().unwrap_or_else(|| Path::new(".")))?;

    serialize_proof(paths.proof_json.to_string_lossy().to_string(), proof.clone())
        .context("failed to write proof json")?;
    export_proof(paths.proof_hex.to_string_lossy().to_string(), proof.clone())
        .context("failed to write proof hex")?;

    let col0: &[Scalar] = compact_public.as_slice();
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    let mut public_out = File::create(&paths.public_hex)
        .with_context(|| format!("failed to create {}", paths.public_hex.display()))?;
    export_public_inputs(instances, &mut public_out)
        .context("failed to write public inputs")?;

    println!("proof written to {}", paths.proof_hex.display());
    println!("proof json written to {}", paths.proof_json.display());
    println!("public inputs written to {}", paths.public_hex.display());

    Ok(())
}

fn verify_with_pcs<PCS>(circuit_cbor: &Path, proof_hex: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let ir = ImportedCircuitIr::from_path(circuit_cbor)?;
    let circuit = ImportedCircuit::new(ir.clone());

    let seed = [0u8; 32];
    let rng: StdRng = SeedableRng::from_seed(seed);

    let k = k_from_ir(&ir)?;
    let params = get_or_create_kzg_params(k, rng)?;
    let vk: VerifyingKey<Scalar, PCS> = keygen_vk_with_k(&params, &circuit, k)?;

    let compact_public = ir.compact_public_inputs()?;
    let proof_hex_contents = fs::read_to_string(proof_hex)
        .with_context(|| format!("failed to read {}", proof_hex.display()))?;
    let proof_bytes = hex::decode(proof_hex_contents.trim())
        .with_context(|| format!("failed to decode hex proof {}", proof_hex.display()))?;

    verify_proof_bytes::<PCS>(&params, &vk, &compact_public, &proof_bytes)?;
    println!("verification succeeded");
    Ok(())
}

fn gen_plinth_with_pcs<PCS>(circuit_cbor: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let (params, vk, compact_public) = prepare_vk::<PCS>(circuit_cbor)?;

    let col0: &[Scalar] = compact_public.as_slice();
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    generate_plinth_verifier::<PCS>(&params, &vk, instances)
        .context("failed to generate Plinth verifier")?;
    println!("Plinth verifier generated");
    Ok(())
}

fn gen_aiken_with_pcs<PCS>(circuit_cbor: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let (params, vk, compact_public) = prepare_vk::<PCS>(circuit_cbor)?;

    let col0: &[Scalar] = compact_public.as_slice();
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    generate_aiken_verifier::<PCS>(&params, &vk, instances, None)
        .context("failed to generate Aiken verifier")?;
    println!("Aiken verifier generated");
    Ok(())
}

fn prepare_vk<PCS>(circuit_cbor: &Path) -> Result<(Params, VerifyingKey<Scalar, PCS>, Vec<Scalar>)>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let ir = ImportedCircuitIr::from_path(circuit_cbor)?;
    let circuit = ImportedCircuit::new(ir.clone());

    let seed = [0u8; 32];
    let rng: StdRng = SeedableRng::from_seed(seed);

    let k = k_from_ir(&ir)?;
    let params = get_or_create_kzg_params(k, rng)?;
    let vk: VerifyingKey<Scalar, PCS> = keygen_vk_with_k(&params, &circuit, k)?;
    let compact_public = ir.compact_public_inputs()?;

    Ok((params, vk, compact_public))
}

fn create_proof_bytes<PCS>(
    params: &Params,
    pk: &ProvingKey<Scalar, PCS>,
    circuit: ImportedCircuit,
    compact_public: &[Scalar],
    rng: &mut StdRng,
) -> Result<Vec<u8>>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let col0: &[Scalar] = compact_public;
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    let mut transcript = CTranscript::init();
    create_proof(params, pk, &[circuit], instances, rng, &mut transcript)
        .context("proof generation failed")?;
    Ok(transcript.finalize())
}

fn verify_proof_bytes<PCS>(
    params: &Params,
    vk: &VerifyingKey<Scalar, PCS>,
    compact_public: &[Scalar],
    proof: &[u8],
) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let col0: &[Scalar] = compact_public;
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    let mut transcript = CTranscript::init_from_bytes(proof);
    let verifier = prepare::<_, PCS, CTranscript>(vk, instances, &mut transcript)
        .context("prepare verification failed")?;
    verifier
        .verify(&params.verifier_params())
        .map_err(|e| anyhow::anyhow!("{e:?}"))
        .context("verification failed")?;
    Ok(())
}

fn k_from_ir(ir: &ImportedCircuitIr) -> Result<u32> {
    let rows = ir.rows.len();
    let table = ir.lookup_table.len();

    // SimpleFloorPlanner packs the main region and table data into the same
    // overall layout budget, so use a conservative upper bound.
    let total = rows
        .checked_add(table)
        .and_then(|x| x.checked_add(8192))
        .context("row-count overflow while computing k")?;

    let n = total.max(1).next_power_of_two();
    let k = n.ilog2();

    if k > 24 {
        bail!(
            "layout needs k={}, which exceeds the supported limit 24 (rows={}, lookup_table={})",
            k,
            rows,
            table
        );
    }

    println!(
        "Using k={} (rows={}, lookup_table={}, public_inputs={})",
        k,
        rows,
        table,
        ir.public_inputs.len()
    );

    Ok(k)
}

struct OutputPaths {
    proof_hex: PathBuf,
    proof_json: PathBuf,
    public_hex: PathBuf,
}

fn derive_output_paths(circuit_cbor: &Path) -> OutputPaths {
    let dir = circuit_cbor.parent().unwrap_or_else(|| Path::new("."));
    let stem = circuit_cbor
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("symbolic_circuit");

    OutputPaths {
        proof_hex: dir.join(format!("{stem}.proof.hex")),
        proof_json: dir.join(format!("{stem}.proof.json")),
        public_hex: dir.join(format!("{stem}.public_inputs.hex")),
    }
}
