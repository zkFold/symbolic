use anyhow::{anyhow, bail, Context, Result};
use blstrs::Scalar;
use ff::PrimeField;
use ff::Field;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::path::Path;

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImportedCircuitIr {
    pub format_version: u32,
    pub field_tag: String,
    pub rows: Vec<ImportedRowIr>,
    pub lookup_table: Vec<LookupTableRowIr>,
    pub public_inputs: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImportedRowIr {
    pub row_index: u32,
    pub a_cell: AssignedCellIr,
    pub b_cell: AssignedCellIr,
    pub c_cell: AssignedCellIr,
    pub q_m_row: String,
    pub q_l_row: String,
    pub q_r_row: String,
    pub q_o_row: String,
    pub q_c_row: String,
    pub q_lookup: bool,
    pub instance_index: Option<u32>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AssignedCellIr {
    pub cell_value: String,
    pub equality_key: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LookupTableRowIr {
    pub t1: String,
    pub t2: String,
    pub t3: String,
}

impl ImportedCircuitIr {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let file = File::open(path)
            .with_context(|| format!("failed to open IR file: {}", path.display()))?;
        let ir: Self = serde_json::from_reader(file)
            .with_context(|| format!("failed to decode IR file: {}", path.display()))?;
        ir.validate()?;
        Ok(ir)
    }

    pub fn validate(&self) -> Result<()> {
        if self.format_version != 1 {
            bail!(
                "unsupported IR format_version {}; expected 1",
                self.format_version
            );
        }

        match self.field_tag.as_str() {
            "bls12_381_scalar" | "blstrs::Scalar" => {}
            other => bail!("unsupported fieldTag {other:?}; expected bls12_381_scalar"),
        }

        for (expected_ix, row) in self.rows.iter().enumerate() {
            if row.row_index as usize != expected_ix {
                bail!(
                    "row index mismatch: expected {}, got {}",
                    expected_ix,
                    row.row_index
                );
            }
            if let Some(ix) = row.instance_index {
                if ix as usize >= self.public_inputs.len() {
                    bail!(
                        "instance_index {} out of bounds for {} compact public inputs",
                        ix,
                        self.public_inputs.len()
                    );
                }
            }
        }

        Ok(())
    }

    pub fn compact_public_inputs(&self) -> Result<Vec<Scalar>> {
        self.public_inputs
            .iter()
            .map(|s| parse_scalar(s))
            .collect::<Result<Vec<_>>>()
    }

    pub fn dense_public_inputs(&self) -> Result<Vec<Scalar>> {
        let compact = self.compact_public_inputs()?;
        let mut dense = vec![Scalar::ZERO; self.rows.len()];

        for row in &self.rows {
            if let Some(ix) = row.instance_index {
                let compact_ix = ix as usize;
                let row_ix = row.row_index as usize;
                dense[row_ix] = *compact
                    .get(compact_ix)
                    .ok_or_else(|| anyhow!("missing compact public input at index {}", compact_ix))?;
            }
        }

        Ok(dense)
    }
}

pub fn parse_scalar(value: &str) -> Result<Scalar> {
    Scalar::from_str_vartime(value)
        .ok_or_else(|| anyhow!("failed to parse Scalar from decimal string {value:?}"))
}
