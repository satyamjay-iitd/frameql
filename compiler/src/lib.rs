use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use petgraph::algo::{condensation, toposort};

use crate::{
    compile::{
        Arrangement, CompilerState, create_arrangements, normalize_arrangement,
        prog_dependency_graph,
    },
    expr::{ECtx, Expr, expr_collect_ctx},
    prog::FrameQLProgram,
    rule::{Rule, RuleRHS},
    r#type::Type,
    var::Var,
};

mod compile;
mod expr;
mod func;
mod prog;
mod relation;
mod rule;
mod r#type;
mod var;

pub type Statics = HashMap<(Expr, Type), i32>;
pub type ModuleName = Vec<String>;
pub type Crate = HashSet<ModuleName>;

fn collect_statics(prog: FrameQLProgram) -> Statics {
    let statics = Rc::new(RefCell::new(HashMap::new()));
    // Getting error here now saying that Clone is implemented for &HashMap<> but not for &mut HashMap<>
    prog.prog_expr_map(|ctx, expr| {
        if prog.expr_is_static(ctx, &expr) {
            prog.add_static(&expr, ctx, &mut statics.borrow_mut());
        }
        expr
    });
    Rc::try_unwrap(statics)
        .expect("multiple Rc pointers left")
        .into_inner()
}

pub fn compile_rules(
    prog: &FrameQLProgram,
    statics: &Statics,
    specname: &str,
) -> (Vec<ProgNode>, CompilerState) {
    // 1. Compute dependency graph
    let depgraph = prog_dependency_graph(prog);

    // // 2. Compute SCCs (condensation + topological sort)
    let condensation = condensation(depgraph, true);

    let sccs = toposort(&condensation, None).expect("Cyclic dependency in program");

    // // 3. Initialize arrangements map
    let arrs: HashMap<String, Vec<Arrangement>> = prog
        .relations
        .keys()
        .map(|rel| (rel.clone(), Vec::new()))
        .collect();

    // // 4. Initialize compiler state
    let mut state = CompilerState { arrangements: arrs };

    // // 5. First pass: compute arrangements
    create_arrangements(prog, &mut state);

    // // 6. Second pass: allocate delayed relation ids
    // alloc_delayed_rel_ids(prog, &mut state);

    // // 7. Third pass: compile SCCs
    // let mut prog_nodes = Vec::new();
    // for (i, scc) in sccs.iter().enumerate() {
    //     let nodes = compile_scc(prog, statics, &depgraph, i, scc, &mut state);
    //     prog_nodes.extend(nodes);
    // }

    (prog_nodes, state)
}

impl FrameQLProgram {
    pub fn rhs_input_arrangement(
        &self,
        rule: &Rule,
        rhs_idx: usize,
        rhs: &RuleRHS,
    ) -> Option<(Vec<(Expr, ECtx)>, Vec<Var>)> {
        match rhs {
            RuleRHS::Literal(atom) => {
                let ctx = ECtx::RuleRAtom {
                    rule: rule.clone(),
                    idx: rhs_idx,
                };
                let (_, vmap) = normalize_arrangement(self, &ctx, &atom.val);

                // collect (Expr, ECtx) pairs
                let keys: Vec<(Expr, ECtx)> = vmap.into_iter().map(|(_, e, c)| (e, c)).collect();

                // vars visible before join ∩ vars visible after join
                let before = self.rhs_vars_after(rule, rhs_idx - 1);
                let after = self.rhs_vars_after(rule, rhs_idx);
                let values: Vec<Var> = before.into_iter().filter(|v| after.contains(v)).collect();

                Some((keys, values))
            }

            RuleRHS::GroupBy {
                project, group_by, ..
            } => {
                let grp_ctx = ECtx::RuleRGroupBy {
                    rule: rule.clone(),
                    idx: rhs_idx,
                };
                let proj_ctx = ECtx::RuleRProject {
                    rule: rule.clone(),
                    idx: rhs_idx,
                };

                let keys: Vec<(Expr, ECtx)> = expr_collect_ctx(
                    |ctx2, e2| match e2 {
                        Expr::EVar(v) => vec![(v.clone(), ctx2.clone())],
                        _ => vec![],
                    },
                    |mut a, mut b| {
                        a.append(&mut b);
                        a
                    },
                    &grp_ctx,
                    group_by,
                )
                .into_iter()
                .map(|(v, ctx)| (Expr::EVar(v.clone()), ctx))
                .collect();

                let values: Vec<Var> = self.expr_free_vars(&proj_ctx, project);

                Some((keys, values))
            }

            _ => None,
        }
    }

    pub fn rule_rhs_term_vars(&self, rule: &Rule, idx: usize) -> Vec<Var> {
        match &rule.body[idx] {
            RuleRHS::Literal(atom) => self.expr_free_vars(
                &ECtx::RuleRAtom {
                    rule: rule.clone(),
                    idx,
                },
                &atom.val,
            ),
            RuleRHS::Cond(expr) => self.expr_free_vars(
                &ECtx::RuleRCond {
                    rule: rule.clone(),
                    idx,
                },
                expr,
            ),
            RuleRHS::FlatMap { map, .. } => self.expr_free_vars(
                &ECtx::RuleRFlatMap {
                    rule: rule.clone(),
                    idx,
                },
                map,
            ),
            RuleRHS::GroupBy {
                group_by, project, ..
            } => {
                let mut set: HashSet<Var> = self
                    .expr_vars(
                        &ECtx::RuleRGroupBy {
                            rule: rule.clone(),
                            idx,
                        },
                        group_by,
                    )
                    .into_iter()
                    .collect();
                set.extend(self.expr_free_vars(
                    &ECtx::RuleRProject {
                        rule: rule.clone(),
                        idx,
                    },
                    project,
                ));
                set.into_iter().collect()
            }
        }
    }

    pub fn rule_lhs_vars(&self, rule: &Rule) -> Vec<Var> {
        let mut vars: HashSet<Var> = HashSet::new();

        for (i, lhs) in rule.head.iter().enumerate() {
            // variables in atom
            let atom_vars = self.expr_free_vars(
                &ECtx::RuleLAtom {
                    rule: rule.clone(),
                    head_idx: i,
                },
                &lhs.0.val,
            );
            vars.extend(atom_vars);
        }

        vars.into_iter().collect()
    }

    pub fn rhs_vars_after(&self, rule: &Rule, i: usize) -> Vec<Var> {
        let vars_after: Vec<Vec<Var>> = (i + 1..rule.body.len())
            .map(|j| self.rule_rhs_term_vars(rule, j))
            .collect();

        let lhs_vars = self.rule_lhs_vars(rule);
        let rhs_vars_next = self.rule_rhs_vars(rule, i + 1);

        rhs_vars_next
            .into_iter()
            .filter(|f| lhs_vars.contains(f) || vars_after.iter().any(|vset| vset.contains(f)))
            .collect()
    }
}
