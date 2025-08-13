//! Halo2 PlonkUp Circuit Implementation
//!
//! This module implements a complete Halo2 circuit for the PlonkUp protocol.
//!
//! **Usage Example:**
//! ```rust
//! use rust_wrapper::halo2::PlonkupCircuit;
//! use halo2_proofs::dev::MockProver;
//! use halo2curves::bls12381::Fr;
//! let circuit = PlonkupCircuit::<Fr>::new(4); // 4 PlonkUp constraints
//! let prover = MockProver::run(5, &circuit, vec![]).unwrap(); // k=5 for Halo2 (32 total rows)
//! prover.assert_satisfied();
//! ```

#![allow(non_snake_case)]

use halo2curves::ff::Field;
use halo2_proofs::{
    circuit::{AssignedCell, Layouter, SimpleFloorPlanner, Value},
    plonk::{
        Advice, Circuit, Column, ConstraintSystem, Error, Fixed,
        Selector, TableColumn,
    },
    poly::Rotation,
};
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

/// Configuration for the PlonkUp circuit
#[derive(Debug, Clone)]
pub struct PlonkupConfig {
    /// Advice columns for witness values (w1, w2, w3)
    pub advice: [Column<Advice>; 3],
    /// Selector columns for gates (qM, qL, qR, qO, qC, qK)
    pub selectors: PlonkupSelectors,
    /// Table columns for lookup arguments (t1, t2, t3)
    pub table: [TableColumn; 3],
}

/// Selector configuration for PlonkUp gates
#[derive(Debug, Clone)]
pub struct PlonkupSelectors {
    /// Multiplication gate selector (qM)
    pub q_mul: Column<Fixed>,
    /// Left input selector (qL)
    pub q_left: Column<Fixed>,
    /// Right input selector (qR)
    pub q_right: Column<Fixed>,
    /// Output selector (qO)
    pub q_output: Column<Fixed>,
    /// Constant selector (qC)
    pub q_const: Column<Fixed>,
    /// Lookup selector (qK)
    pub q_lookup: Selector,
}

/// Main PlonkUp circuit implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlonkupCircuit<F: Field> {
    /// Circuit size (number of constraints in the PlonkUp circuit)
    pub circuit_size: usize,
    /// Witness assignments for advice columns
    pub witness: PlonkupWitnessData<F>,
    /// Selector assignments
    pub selectors: PlonkupSelectorData<F>,
    /// Lookup table data
    pub table: PlonkupTableData<F>,
    /// Copy constraints for equality: (from_col, from_row, to_col, to_row)
    pub copy_constraints: Vec<(usize, usize, usize, usize)>,
    /// Phantom data for the field type
    _marker: PhantomData<F>,
}

/// Witness data for the three advice columns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlonkupWitnessData<F> {
    /// First witness column (w1)
    pub w1: Vec<F>,
    /// Second witness column (w2)
    pub w2: Vec<F>,
    /// Third witness column (w3)
    pub w3: Vec<F>,
}

/// Selector assignments for each gate type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlonkupSelectorData<F> {
    /// Multiplication gate selector values
    pub q_mul: Vec<F>,
    /// Left input selector values
    pub q_left: Vec<F>,
    /// Right input selector values
    pub q_right: Vec<F>,
    /// Output selector values
    pub q_output: Vec<F>,
    /// Constant selector values
    pub q_const: Vec<F>,
    /// Lookup selector values
    pub q_lookup: Vec<F>,
}

/// Lookup table data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlonkupTableData<F> {
    /// First table column (t1)
    pub t1: Vec<F>,
    /// Second table column (t2)
    pub t2: Vec<F>,
    /// Third table column (t3)
    pub t3: Vec<F>,
}

impl<F: Field> PlonkupCircuit<F> {
    /// Create a new PlonkUp circuit with specified number of constraints
    pub fn new(circuit_size: usize) -> Self {
        Self {
            circuit_size,
            witness: PlonkupWitnessData {
                w1: vec![F::ZERO; circuit_size],
                w2: vec![F::ZERO; circuit_size],
                w3: vec![F::ZERO; circuit_size],
            },
            selectors: PlonkupSelectorData {
                q_mul: vec![F::ZERO; circuit_size],
                q_left: vec![F::ZERO; circuit_size],
                q_right: vec![F::ZERO; circuit_size],
                q_output: vec![F::ZERO; circuit_size],
                q_const: vec![F::ZERO; circuit_size],
                q_lookup: vec![F::ZERO; circuit_size],
            },
            table: PlonkupTableData {
                t1: Vec::new(),
                t2: Vec::new(),
                t3: Vec::new(),
            },
            copy_constraints: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Load circuit from JSON string
    pub fn from_json(json_str: &str) -> Result<Self, Box<dyn std::error::Error>>
    where
        F: for<'de> Deserialize<'de>,
    {
        let circuit: PlonkupCircuit<F> = serde_json::from_str(json_str)?;
        Ok(circuit)
    }

    /// Save circuit to JSON string
    pub fn to_json(&self) -> Result<String, Box<dyn std::error::Error>>
    where
        F: Serialize,
    {
        Ok(serde_json::to_string_pretty(self)?)
    }

    /// Load circuit from JSON file
    pub fn from_json_file(path: &str) -> Result<Self, Box<dyn std::error::Error>>
    where
        F: for<'de> Deserialize<'de>,
    {
        let json_str = std::fs::read_to_string(path)?;
        let circuit: PlonkupCircuit<F> = serde_json::from_str(&json_str)?;
        Ok(circuit)
    }

    /// Save circuit to JSON file
    pub fn to_json_file(&self, path: &str) -> Result<(), Box<dyn std::error::Error>>
    where
        F: Serialize,
    {
        let json_str = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json_str)?;
        Ok(())
    }

    /// Add a copy constraint between two cells using (column, row) coordinates
    pub fn add_copy_constraint(&mut self, from_col: usize, from_row: usize, to_col: usize, to_row: usize) {
        self.copy_constraints.push((from_col, from_row, to_col, to_row));
    }

    /// Add multiple copy constraints at once
    pub fn add_copy_constraints(&mut self, constraints: Vec<(usize, usize, usize, usize)>) {
        self.copy_constraints.extend(constraints);
    }

    /// Validate circuit parameters
    pub fn validate_params(&self) -> Result<(), String> {
        let size = self.circuit_size;
        
        // Check witness column lengths
        if self.witness.w1.len() != size {
            return Err(format!("w1 length {} != circuit_size {}", self.witness.w1.len(), size));
        }
        if self.witness.w2.len() != size {
            return Err(format!("w2 length {} != circuit_size {}", self.witness.w2.len(), size));
        }
        if self.witness.w3.len() != size {
            return Err(format!("w3 length {} != circuit_size {}", self.witness.w3.len(), size));
        }

        // Check selector lengths
        if self.selectors.q_mul.len() != size {
            return Err(format!("q_mul length {} != circuit_size {}", self.selectors.q_mul.len(), size));
        }
        if self.selectors.q_left.len() != size {
            return Err(format!("q_left length {} != circuit_size {}", self.selectors.q_left.len(), size));
        }
        if self.selectors.q_right.len() != size {
            return Err(format!("q_right length {} != circuit_size {}", self.selectors.q_right.len(), size));
        }
        if self.selectors.q_output.len() != size {
            return Err(format!("q_output length {} != circuit_size {}", self.selectors.q_output.len(), size));
        }
        if self.selectors.q_const.len() != size {
            return Err(format!("q_const length {} != circuit_size {}", self.selectors.q_const.len(), size));
        }
        if self.selectors.q_lookup.len() != size {
            return Err(format!("q_lookup length {} != circuit_size {}", self.selectors.q_lookup.len(), size));
        }

        Ok(())
    }
}

impl<F: Field> Circuit<F> for PlonkupCircuit<F> {
    type Config = PlonkupConfig;
    type FloorPlanner = SimpleFloorPlanner;

    fn without_witnesses(&self) -> Self {
        // Create a circuit with zero witnesses for key generation
        let zero_witness = PlonkupWitnessData {
            w1: vec![F::ZERO; self.circuit_size],
            w2: vec![F::ZERO; self.circuit_size],
            w3: vec![F::ZERO; self.circuit_size],
        };

        Self {
            circuit_size: self.circuit_size,
            witness: zero_witness,
            selectors: self.selectors.clone(),
            table: self.table.clone(),
            copy_constraints: self.copy_constraints.clone(),
            _marker: PhantomData,
        }
    }

    fn configure(meta: &mut ConstraintSystem<F>) -> Self::Config {
        // Create advice columns for witnesses
        let advice = [
            meta.advice_column(),
            meta.advice_column(),
            meta.advice_column(),
        ];

        // Enable equality constraints on advice columns
        for column in &advice {
            meta.enable_equality(*column);
        }

        // Create selector columns
        let selectors = PlonkupSelectors {
            q_mul: meta.fixed_column(),
            q_left: meta.fixed_column(),
            q_right: meta.fixed_column(),
            q_output: meta.fixed_column(),
            q_const: meta.fixed_column(),
            q_lookup: meta.selector(),
        };

        // Create table columns for lookup
        let table = [
            meta.lookup_table_column(),
            meta.lookup_table_column(),
            meta.lookup_table_column(),
        ];

        // Define the PlonkUp arithmetic gate constraint
        // qM * w1 * w2 + qL * w1 + qR * w2 + qO * w3 + qC = 0
        meta.create_gate("plonkup_arithmetic", |meta| {
            let q_mul = meta.query_fixed(selectors.q_mul, Rotation::cur());
            let q_left = meta.query_fixed(selectors.q_left, Rotation::cur());
            let q_right = meta.query_fixed(selectors.q_right, Rotation::cur());
            let q_output = meta.query_fixed(selectors.q_output, Rotation::cur());
            let q_const = meta.query_fixed(selectors.q_const, Rotation::cur());

            let w1 = meta.query_advice(advice[0], Rotation::cur());
            let w2 = meta.query_advice(advice[1], Rotation::cur());
            let w3 = meta.query_advice(advice[2], Rotation::cur());

            // PlonkUp arithmetic constraint
            vec![
                q_mul * w1.clone() * w2.clone()
                    + q_left * w1
                    + q_right * w2
                    + q_output * w3
                    + q_const,
            ]
        });

        PlonkupConfig {
            advice,
            selectors,
            table,
        }
    }

    fn synthesize(
        &self,
        config: Self::Config,
        mut layouter: impl Layouter<F>,
    ) -> Result<(), Error> {
        // Validate parameters before synthesis
        self.validate_params()
            .map_err(|_e| Error::Synthesis)?;

        // Load lookup table (only if table has data)
        if !self.table.t1.is_empty() {
            layouter.assign_table(
                || "plonkup_table",
                |mut table| {
                    for (i, ((t1, t2), t3)) in self
                        .table
                        .t1
                        .iter()
                        .zip(&self.table.t2)
                        .zip(&self.table.t3)
                        .enumerate()
                    {
                        table.assign_cell(|| format!("t1[{}]", i), config.table[0], i, || Value::known(*t1))?;
                        table.assign_cell(|| format!("t2[{}]", i), config.table[1], i, || Value::known(*t2))?;
                        table.assign_cell(|| format!("t3[{}]", i), config.table[2], i, || Value::known(*t3))?;
                    }
                    Ok(())
                },
            )?;
        }

        // Assign witness values and fixed selector values
        layouter.assign_region(
            || "plonkup_main",
            |mut region| {
                let mut assigned_cells: Vec<Vec<AssignedCell<F, F>>> = vec![Vec::new(); 3];

                for row in 0..self.circuit_size {
                    // Assign witness values
                    let w1 = region.assign_advice(
                        || format!("w1[{}]", row),
                        config.advice[0],
                        row,
                        || Value::known(self.witness.w1[row]),
                    )?;
                    assigned_cells[0].push(w1);

                    let w2 = region.assign_advice(
                        || format!("w2[{}]", row),
                        config.advice[1],
                        row,
                        || Value::known(self.witness.w2[row]),
                    )?;
                    assigned_cells[1].push(w2);

                    let w3 = region.assign_advice(
                        || format!("w3[{}]", row),
                        config.advice[2],
                        row,
                        || Value::known(self.witness.w3[row]),
                    )?;
                    assigned_cells[2].push(w3);

                    // Assign fixed selector values
                    region.assign_fixed(
                        || format!("q_mul[{}]", row),
                        config.selectors.q_mul,
                        row,
                        || Value::known(self.selectors.q_mul[row]),
                    )?;

                    region.assign_fixed(
                        || format!("q_left[{}]", row),
                        config.selectors.q_left,
                        row,
                        || Value::known(self.selectors.q_left[row]),
                    )?;

                    region.assign_fixed(
                        || format!("q_right[{}]", row),
                        config.selectors.q_right,
                        row,
                        || Value::known(self.selectors.q_right[row]),
                    )?;

                    region.assign_fixed(
                        || format!("q_output[{}]", row),
                        config.selectors.q_output,
                        row,
                        || Value::known(self.selectors.q_output[row]),
                    )?;

                    region.assign_fixed(
                        || format!("q_const[{}]", row),
                        config.selectors.q_const,
                        row,
                        || Value::known(self.selectors.q_const[row]),
                    )?;

                    // Enable lookup selector when needed
                    if self.selectors.q_lookup[row] != F::ZERO {
                        config.selectors.q_lookup.enable(&mut region, row)?;
                    }
                }

                // Apply copy constraints
                for &(from_col, from_row, to_col, to_row) in &self.copy_constraints {
                    // Validate constraint indices
                    if from_row >= self.circuit_size || to_row >= self.circuit_size {
                        return Err(Error::Synthesis);
                    }
                    if from_col >= 3 || to_col >= 3 {
                        return Err(Error::Synthesis);
                    }

                    // Enforce equality constraint
                    region.constrain_equal(
                        assigned_cells[from_col][from_row].cell(),
                        assigned_cells[to_col][to_row].cell(),
                    )?;
                }

                Ok(())
            },
        )?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use halo2_proofs::dev::MockProver;
    use halo2curves::bls12381::Fr;

    #[test]
    fn test_empty_circuit() {
        let circuit_size = 0;
        let circuit = PlonkupCircuit::<Fr>::new(circuit_size);

        // Test circuit compilation (without serialization constraints)
        let prover = MockProver::run(5, &circuit.without_witnesses(), vec![]).expect("Failed to run mock prover");
        prover.assert_satisfied();
    }


    #[test]
    fn test_basic_arithmetic() {
        let circuit_size = 1;
        let mut circuit = PlonkupCircuit::<Fr>::new(circuit_size);
        
        // Set up a simple arithmetic constraint: w1 * w2 = w3
        // Row 0: 2 * 3 = 6
        circuit.witness.w1[0] = Fr::from(2);
        circuit.witness.w2[0] = Fr::from(3);
        circuit.witness.w3[0] = Fr::from(6);
        circuit.selectors.q_mul[0] = Fr::from(1);
        circuit.selectors.q_output[0] = -Fr::from(1);
        
        // Test circuit execution
        let prover = MockProver::run(5, &circuit, vec![]).expect("Failed to run mock prover");
        prover.assert_satisfied();
    }

    #[test]
    fn test_validation() {
        let circuit_size = 4;
        let mut circuit = PlonkupCircuit::<Fr>::new(circuit_size);
        
        // Create invalid params (wrong size)
        circuit.witness.w1 = vec![Fr::from(1), Fr::from(2)]; // Size 2 instead of 4
        
        // Should fail validation
        assert!(circuit.validate_params().is_err());
    }

    #[test]
    fn test_copy_constraints() {
        let circuit_size = 2;
        let mut circuit = PlonkupCircuit::<Fr>::new(circuit_size);
        
        // Set up two multiplication gates that share a common value
        // Row 0: w1[0] * w2[0] = w3[0]  =>  2 * 3 = 6
        // Row 1: w1[1] * w2[1] = w3[1]  =>  6 * 4 = 24
        // Copy constraint: w3[0] = w1[1] (output of first gate = input to second gate)
        
        circuit.witness.w1[0] = Fr::from(2);
        circuit.witness.w2[0] = Fr::from(3);
        circuit.witness.w3[0] = Fr::from(6);
        circuit.selectors.q_mul[0] = Fr::from(1);
        circuit.selectors.q_output[0] = -Fr::from(1);
        
        circuit.witness.w1[1] = Fr::from(6);  // This should equal w3[0]
        circuit.witness.w2[1] = Fr::from(4);
        circuit.witness.w3[1] = Fr::from(24);
        circuit.selectors.q_mul[1] = Fr::from(1);
        circuit.selectors.q_output[1] = -Fr::from(1);
        
        // Add copy constraint: w3[0] (column 2, row 0) = w1[1] (column 0, row 1)
        circuit.add_copy_constraint(2, 0, 0, 1);
        
        // Test circuit execution with copy constraints
        let prover = MockProver::run(5, &circuit, vec![]).expect("Failed to run mock prover");
        prover.assert_satisfied();
    }

    #[test]
    fn test_copy_constraint_violation() {
        let circuit_size = 2;
        let mut circuit = PlonkupCircuit::<Fr>::new(circuit_size);
        
        // Set up a copy constraint that should fail
        circuit.witness.w1[0] = Fr::from(2);
        circuit.witness.w1[1] = Fr::from(3); // Different value, should cause constraint violation
        
        // Add copy constraint that should fail
        circuit.add_copy_constraint(0, 0, 0, 1);

        // Test circuit execution - this should fail due to copy constraint violation
        let prover = MockProver::run(5, &circuit, vec![]).expect("Failed to run mock prover");
        
        // This should panic because the copy constraint is violated
        let result = std::panic::catch_unwind(|| {
            prover.assert_satisfied();
        });
        assert!(result.is_err(), "Copy constraint violation should cause assertion failure");
    }
}
