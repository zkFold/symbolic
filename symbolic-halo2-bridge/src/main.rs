mod imported_circuit;
mod ir;

use anyhow::{bail, Context, Result};
use blstrs::{Bls12, G1Projective, Scalar};
use halo2_proofs::plonk::{
    create_proof, k_from_circuit, keygen_pk, keygen_vk, prepare, ProvingKey, VerifyingKey,
};
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
        [cmd, circuit_json] if cmd == "prove" => dispatch_prove(Path::new(circuit_json), flavor),
        [cmd, circuit_json, proof_hex] if cmd == "verify" => {
            dispatch_verify(Path::new(circuit_json), Path::new(proof_hex), flavor)
        }
        [cmd, circuit_json] if cmd == "gen-plinth" => {
            dispatch_gen_plinth(Path::new(circuit_json), flavor)
        }
        [cmd, circuit_json] if cmd == "gen-aiken" => {
            dispatch_gen_aiken(Path::new(circuit_json), flavor)
        }
        _ => {
            eprintln!("Usage:");
            eprintln!("  symbolic-halo2-bridge prove <circuit.json> [gwc_kzg]");
            eprintln!("  symbolic-halo2-bridge verify <circuit.json> <proof.hex> [gwc_kzg]");
            eprintln!("  symbolic-halo2-bridge gen-plinth <circuit.json> [gwc_kzg]");
            eprintln!("  symbolic-halo2-bridge gen-aiken <circuit.json> [gwc_kzg]");
            bail!("invalid command line")
        }
    }
}

fn dispatch_prove(circuit_json: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => prove_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_json),
        Flavor::Gwc19 => prove_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_json),
    }
}

fn dispatch_verify(circuit_json: &Path, proof_hex: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => verify_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_json, proof_hex),
        Flavor::Gwc19 => verify_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_json, proof_hex),
    }
}

fn dispatch_gen_plinth(circuit_json: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => gen_plinth_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_json),
        Flavor::Gwc19 => gen_plinth_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_json),
    }
}

fn dispatch_gen_aiken(circuit_json: &Path, flavor: Flavor) -> Result<()> {
    match flavor {
        Flavor::Halo2 => gen_aiken_with_pcs::<KZGCommitmentScheme<Bls12>>(circuit_json),
        Flavor::Gwc19 => gen_aiken_with_pcs::<GwcKZGCommitmentScheme<Bls12>>(circuit_json),
    }
}

fn prove_with_pcs<PCS>(circuit_json: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let ir = ImportedCircuitIr::from_path(circuit_json)?;
    let circuit = ImportedCircuit::new(ir.clone());

    let seed = [0u8; 32];
    let mut rng: StdRng = SeedableRng::from_seed(seed);

    let k = k_from_circuit(&circuit);
    let params = get_or_create_kzg_params(k, rng.clone())?;
    let vk: VerifyingKey<Scalar, PCS> = keygen_vk(&params, &circuit)?;
    let pk: ProvingKey<Scalar, PCS> = keygen_pk(vk.clone(), &circuit)?;

    let dense_public = ir.dense_public_inputs()?;
    let paths = derive_output_paths(circuit_json);

    let proof = create_proof_bytes::<PCS>(&params, &pk, circuit.clone(), &dense_public, &mut rng)?;
    verify_proof_bytes::<PCS>(&params, &vk, &dense_public, &proof)?;

    fs::create_dir_all(
        paths.proof_hex
            .parent()
            .unwrap_or_else(|| Path::new(".")),
    )?;

    serialize_proof(paths.proof_json.to_string_lossy().to_string(), proof.clone())
        .context("failed to write proof json")?;
    export_proof(paths.proof_hex.to_string_lossy().to_string(), proof.clone())
        .context("failed to write proof hex")?;

    let col0: &[Scalar] = dense_public.as_slice();
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    let mut public_out = File::create(&paths.public_hex)
        .with_context(|| format!("failed to create {}", paths.public_hex.display()))?;
    export_public_inputs(instances, &mut public_out)
        .context("failed to write dense public inputs")?;

    println!("proof written to {}", paths.proof_hex.display());
    println!("proof json written to {}", paths.proof_json.display());
    println!("dense public inputs written to {}", paths.public_hex.display());

    Ok(())
}

fn verify_with_pcs<PCS>(circuit_json: &Path, proof_hex: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let ir = ImportedCircuitIr::from_path(circuit_json)?;
    let circuit = ImportedCircuit::new(ir.clone());

    let seed = [0u8; 32];
    let rng: StdRng = SeedableRng::from_seed(seed);

    let k = k_from_circuit(&circuit);
    let params = get_or_create_kzg_params(k, rng)?;
    let vk: VerifyingKey<Scalar, PCS> = keygen_vk(&params, &circuit)?;

    let dense_public = ir.dense_public_inputs()?;
    let proof_hex_contents = fs::read_to_string(proof_hex)
        .with_context(|| format!("failed to read {}", proof_hex.display()))?;
    let proof_bytes = hex::decode(proof_hex_contents.trim())
        .with_context(|| format!("failed to decode hex proof {}", proof_hex.display()))?;

    verify_proof_bytes::<PCS>(&params, &vk, &dense_public, &proof_bytes)?;
    println!("verification succeeded");
    Ok(())
}

fn gen_plinth_with_pcs<PCS>(circuit_json: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let (params, vk, dense_public) = prepare_vk::<PCS>(circuit_json)?;

    let col0: &[Scalar] = dense_public.as_slice();
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    generate_plinth_verifier::<PCS>(&params, &vk, instances)
        .context("failed to generate Plinth verifier")?;
    println!("Plinth verifier generated");
    Ok(())
}

fn gen_aiken_with_pcs<PCS>(circuit_json: &Path) -> Result<()>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let (params, vk, dense_public) = prepare_vk::<PCS>(circuit_json)?;

    let col0: &[Scalar] = dense_public.as_slice();
    let instance_columns: [&[Scalar]; 1] = [col0];
    let instances: &[&[&[Scalar]]] = &[&instance_columns];

    generate_aiken_verifier::<PCS>(&params, &vk, instances, None)
        .context("failed to generate Aiken verifier")?;
    println!("Aiken verifier generated");
    Ok(())
}

fn prepare_vk<PCS>(circuit_json: &Path) -> Result<(Params, VerifyingKey<Scalar, PCS>, Vec<Scalar>)>
where
    PCS: PolynomialCommitmentScheme<
            Scalar,
            Commitment = G1Projective,
            Parameters = Params,
            VerifierParameters = ParamsVK,
        > + ExtractPCS,
{
    let ir = ImportedCircuitIr::from_path(circuit_json)?;
    let circuit = ImportedCircuit::new(ir.clone());

    let seed = [0u8; 32];
    let rng: StdRng = SeedableRng::from_seed(seed);

    let k = k_from_circuit(&circuit);
    let params = get_or_create_kzg_params(k, rng)?;
    let vk: VerifyingKey<Scalar, PCS> = keygen_vk(&params, &circuit)?;
    let dense_public = ir.dense_public_inputs()?;

    Ok((params, vk, dense_public))
}

fn create_proof_bytes<PCS>(
    params: &Params,
    pk: &ProvingKey<Scalar, PCS>,
    circuit: ImportedCircuit,
    dense_public: &[Scalar],
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
    let col0: &[Scalar] = dense_public;
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
    dense_public: &[Scalar],
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
    let col0: &[Scalar] = dense_public;
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

struct OutputPaths {
    proof_hex: PathBuf,
    proof_json: PathBuf,
    public_hex: PathBuf,
}

fn derive_output_paths(circuit_json: &Path) -> OutputPaths {
    let dir = circuit_json.parent().unwrap_or_else(|| Path::new("."));
    let stem = circuit_json
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("symbolic_circuit");

    OutputPaths {
        proof_hex: dir.join(format!("{stem}.proof.hex")),
        proof_json: dir.join(format!("{stem}.proof.json")),
        public_hex: dir.join(format!("{stem}.public_inputs.hex")),
    }
}
