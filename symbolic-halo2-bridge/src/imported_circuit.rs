use ff::Field;
use crate::ir::{parse_scalar, ImportedCircuitIr};
use anyhow::{anyhow, Result};
use blstrs::Scalar;
use halo2_proofs::circuit::{Cell, Layouter, SimpleFloorPlanner, Value};
use halo2_proofs::plonk::{
    Advice, Circuit, Column, ConstraintSystem, Error, Fixed, Instance, TableColumn,
};
use halo2_proofs::poly::Rotation;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct ImportedConfig {
    pub adv_a: Column<Advice>,
    pub adv_b: Column<Advice>,
    pub adv_c: Column<Advice>,

    pub inst: Column<Instance>,

    pub q_m: Column<Fixed>,
    pub q_l: Column<Fixed>,
    pub q_r: Column<Fixed>,
    pub q_o: Column<Fixed>,
    pub q_c: Column<Fixed>,
    pub q_pub: Column<Fixed>,
    pub q_lookup: Column<Fixed>,

    pub table_1: TableColumn,
    pub table_2: TableColumn,
    pub table_3: TableColumn,
}

#[derive(Clone, Debug)]
pub struct ImportedCircuit {
    pub ir: Arc<ImportedCircuitIr>,
}

impl ImportedCircuit {
    pub fn new(ir: ImportedCircuitIr) -> Self {
        Self { ir: Arc::new(ir) }
    }
}

impl Circuit<Scalar> for ImportedCircuit {
    type Config = ImportedConfig;
    type FloorPlanner = SimpleFloorPlanner;

    fn without_witnesses(&self) -> Self {
        self.clone()
    }

    fn configure(meta: &mut ConstraintSystem<Scalar>) -> Self::Config {
        let adv_a = meta.advice_column();
        let adv_b = meta.advice_column();
        let adv_c = meta.advice_column();

        meta.enable_equality(adv_a);
        meta.enable_equality(adv_b);
        meta.enable_equality(adv_c);

        let inst = meta.instance_column();

        let q_m = meta.fixed_column();
        let q_l = meta.fixed_column();
        let q_r = meta.fixed_column();
        let q_o = meta.fixed_column();
        let q_c = meta.fixed_column();
        let q_pub = meta.fixed_column();
        let q_lookup = meta.fixed_column();

        let table_1 = meta.lookup_table_column();
        let table_2 = meta.lookup_table_column();
        let table_3 = meta.lookup_table_column();

        meta.create_gate("symbolic_main_gate", |meta| {
            let a = meta.query_advice(adv_a, Rotation::cur());
            let b = meta.query_advice(adv_b, Rotation::cur());
            let c = meta.query_advice(adv_c, Rotation::cur());

            let q_m_expr = meta.query_fixed(q_m, Rotation::cur());
            let q_l_expr = meta.query_fixed(q_l, Rotation::cur());
            let q_r_expr = meta.query_fixed(q_r, Rotation::cur());
            let q_o_expr = meta.query_fixed(q_o, Rotation::cur());
            let q_c_expr = meta.query_fixed(q_c, Rotation::cur());
            let q_pub_expr = meta.query_fixed(q_pub, Rotation::cur());

            let pi = meta.query_instance(inst, Rotation::cur());

            vec![
                q_m_expr * a.clone() * b.clone()
                    + q_l_expr * a
                    + q_r_expr * b
                    + q_o_expr * c
                    + q_c_expr
                    - q_pub_expr * pi,
            ]
        });

        meta.lookup("symbolic_lookup", |meta| {
            let q = meta.query_fixed(q_lookup, Rotation::cur());
            let a = meta.query_advice(adv_a, Rotation::cur());
            let b = meta.query_advice(adv_b, Rotation::cur());
            let c = meta.query_advice(adv_c, Rotation::cur());

            vec![(q.clone() * a, table_1), (q.clone() * b, table_2), (q * c, table_3)]
        });

        ImportedConfig {
            adv_a,
            adv_b,
            adv_c,
            inst,
            q_m,
            q_l,
            q_r,
            q_o,
            q_c,
            q_pub,
            q_lookup,
            table_1,
            table_2,
            table_3,
        }
    }

    fn synthesize(
        &self,
        config: Self::Config,
        mut layouter: impl Layouter<Scalar>,
    ) -> Result<(), Error> {
        layouter.assign_table(
            || "symbolic_lookup_table",
            |mut table| {
                for (offset, row) in self.ir.lookup_table.iter().enumerate() {
                    let t1 = parse_scalar(&row.t1).map_err(to_halo2_err)?;
                    let t2 = parse_scalar(&row.t2).map_err(to_halo2_err)?;
                    let t3 = parse_scalar(&row.t3).map_err(to_halo2_err)?;

                    table.assign_cell(|| "t1", config.table_1, offset, || Value::known(t1))?;
                    table.assign_cell(|| "t2", config.table_2, offset, || Value::known(t2))?;
                    table.assign_cell(|| "t3", config.table_3, offset, || Value::known(t3))?;
                }
                Ok(())
            },
        )?;

        layouter.assign_region(
            || "symbolic_rows",
            |mut region| {
                let mut equality_cells: HashMap<String, Cell> = HashMap::new();

                for (offset, row) in self.ir.rows.iter().enumerate() {
                    let a_val = parse_scalar(&row.a_cell.cell_value).map_err(to_halo2_err)?;
                    let b_val = parse_scalar(&row.b_cell.cell_value).map_err(to_halo2_err)?;
                    let c_val = parse_scalar(&row.c_cell.cell_value).map_err(to_halo2_err)?;

                    let q_m = parse_scalar(&row.q_m_row).map_err(to_halo2_err)?;
                    let q_l = parse_scalar(&row.q_l_row).map_err(to_halo2_err)?;
                    let q_r = parse_scalar(&row.q_r_row).map_err(to_halo2_err)?;
                    let q_o = parse_scalar(&row.q_o_row).map_err(to_halo2_err)?;
                    let q_c = parse_scalar(&row.q_c_row).map_err(to_halo2_err)?;
                    let q_pub = if row.instance_index.is_some() {
                        Scalar::ONE
                    } else {
                        Scalar::ZERO
                    };
                    let q_lookup = if row.q_lookup {
                        Scalar::ONE
                    } else {
                        Scalar::ZERO
                    };

                    region.assign_fixed(|| "q_m", config.q_m, offset, || Value::known(q_m))?;
                    region.assign_fixed(|| "q_l", config.q_l, offset, || Value::known(q_l))?;
                    region.assign_fixed(|| "q_r", config.q_r, offset, || Value::known(q_r))?;
                    region.assign_fixed(|| "q_o", config.q_o, offset, || Value::known(q_o))?;
                    region.assign_fixed(|| "q_c", config.q_c, offset, || Value::known(q_c))?;
                    region.assign_fixed(|| "q_pub", config.q_pub, offset, || Value::known(q_pub))?;
                    region.assign_fixed(
                        || "q_lookup",
                        config.q_lookup,
                        offset,
                        || Value::known(q_lookup),
                    )?;

                    let a_assigned =
                        region.assign_advice(|| "a", config.adv_a, offset, || Value::known(a_val))?;
                    let b_assigned =
                        region.assign_advice(|| "b", config.adv_b, offset, || Value::known(b_val))?;
                    let c_assigned =
                        region.assign_advice(|| "c", config.adv_c, offset, || Value::known(c_val))?;

                    maybe_constrain_equal(
                        &mut region,
                        &mut equality_cells,
                        row.a_cell.equality_key.as_deref(),
                        a_assigned.cell(),
                    )?;
                    maybe_constrain_equal(
                        &mut region,
                        &mut equality_cells,
                        row.b_cell.equality_key.as_deref(),
                        b_assigned.cell(),
                    )?;
                    maybe_constrain_equal(
                        &mut region,
                        &mut equality_cells,
                        row.c_cell.equality_key.as_deref(),
                        c_assigned.cell(),
                    )?;
                }

                Ok(())
            },
        )?;

        Ok(())
    }
}

fn maybe_constrain_equal(
    region: &mut halo2_proofs::circuit::Region<'_, Scalar>,
    equality_cells: &mut HashMap<String, Cell>,
    key: Option<&str>,
    cell: Cell,
) -> Result<(), Error> {
    if let Some(key) = key {
        if let Some(existing) = equality_cells.get(key) {
            region.constrain_equal(*existing, cell)?;
        } else {
            equality_cells.insert(key.to_owned(), cell);
        }
    }
    Ok(())
}

fn to_halo2_err(err: anyhow::Error) -> Error {
    log::error!("{err:#}");
    Error::Synthesis
}
