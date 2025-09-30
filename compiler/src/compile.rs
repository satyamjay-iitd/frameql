use std::collections::HashMap;

use frameql_ast::{Identifier, RelSemantics};
use petgraph::{Directed, Graph};

use crate::{
    expr::{ECtx, Expr, expr_fold_ctx},
    prog::FrameQLProgram,
    relation::Relation,
    rule::{Rule, RuleRHS},
};

pub struct CompilerState {
    pub arrangements: HashMap<String, Vec<Arrangement>>,
}

#[derive(Clone)]
pub enum Arrangement {
    Map(Expr),
    Set(Expr, bool),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DepGraphNode {
    Rel(Identifier),
}

pub type DepGraph = Graph<DepGraphNode, (), Directed>;

pub fn prog_dependency_graph(prog: &FrameQLProgram) -> DepGraph {
    let mut g: DepGraph = Graph::new();

    // --- Add relation nodes
    let mut rel_indices = HashMap::new();
    for rel in prog.relations.values() {
        let idx = g.add_node(DepGraphNode::Rel(rel.name.clone()));
        rel_indices.insert(rel.name.clone(), idx);
    }

    // --- Rule edges: RHS → LHS
    for rule in &prog.rules {
        for lhs in &rule.head {
            for rhs in &rule.body {
                if let RuleRHS::Literal(atom) = rhs {
                    let rhs_idx = rel_indices[&atom.relation];
                    let lhs_idx = rel_indices[&lhs.0.relation];
                    g.add_edge(rhs_idx, lhs_idx, ());
                }
            }
        }
    }

    g
}

pub fn create_arrangements(prog: &FrameQLProgram, state: &mut CompilerState) {
    for rel in prog.relations.values() {
        create_rel_arrangements(&prog, rel, state);
    }
}

pub fn create_rel_arrangements(prog: &FrameQLProgram, rel: &Relation, state: &mut CompilerState) {
    for rule in prog.get_rules(&rel.name) {
        create_rule_arrangements(prog, &rule, state);
    }
}

pub fn create_rule_arrangements(prog: &FrameQLProgram, rule: &Rule, state: &mut CompilerState) {
    if let Some(arr) = prog.rule_arrange_fst_literal(rule) {
        let first_rhs: &RuleRHS = &rule.body[0];
        let rel_name: &Identifier = first_rhs.var();

        add_join_arrangement(prog, rel_name, arr, &mut state.arrangements);
    }

    for idx in 1..rule.body.len() {
        create_rule_arrangement(prog, rule, idx, state);
    }
}

pub fn create_rule_arrangement(
    prog: &FrameQLProgram,
    rule: &Rule,
    idx: usize,
    state: &mut CompilerState,
) {
    let rhs = &rule.body[idx];
    let ctx = ECtx::RuleRAtom {
        idx,
        rule: rule.clone(),
    };

    match rhs {
        RuleRHS::Literal(atom) => {
            let rel = prog.get_relation(&atom.relation);
            let (arr, _) = normalize_arrangement(prog, &ctx, &atom.val);

            // If the literal does not introduce new variables, it's a semijoin.
            let is_semi = prog.rule_rhs_new_vars(rule, idx).is_empty();

            if is_semi {
                add_semijoin_arrangement(prog, &rel.name, arr, &mut state.arrangements);
            } else {
                add_join_arrangement(prog, &rel.name, arr, &mut state.arrangements);
            }
        }

        RuleRHS::Cond { .. } | RuleRHS::GroupBy { .. } | RuleRHS::FlatMap { .. } => {}
    }
}

pub type VarMap = Vec<(String, Expr, ECtx)>;

struct RenameState {
    counter: usize,
    vmap: VarMap,
}

pub fn normalize_arrangement(prog: &FrameQLProgram, patctx: &ECtx, pat: &Expr) -> (Expr, VarMap) {
    // First normalize pattern structure
    let pat_prime = expr_fold_ctx(&mut |ctx, e| normalize_pattern(prog, ctx, e), patctx, pat);

    // Initialize state
    let mut st = RenameState {
        counter: 0,
        vmap: Vec::new(),
    };

    let renamed = rename(&mut st, patctx.clone(), &pat_prime);

    (renamed, st.vmap)
}

fn rename(st: &mut RenameState, ctx: ECtx, expr: &Expr) -> Expr {
    match &expr {
        Expr::EStruct { fields, name } => {
            let new_fields: Vec<(Identifier, Expr)> = fields
                .iter()
                .enumerate()
                .map(|(i, (name, subexpr))| {
                    let new_e = rename(
                        st,
                        ECtx::Struct {
                            par_expr: expr.clone(),
                            par: Box::new(ctx.clone()),
                            arg: (i, name.clone()),
                        },
                        subexpr,
                    );
                    (name.clone(), new_e)
                })
                .collect();
            Expr::EStruct {
                name: name.clone(),
                fields: new_fields,
            }
        }
        Expr::ETuple(fields) => {
            let new_fields: Vec<Expr> = fields
                .iter()
                .enumerate()
                .map(|(i, subexpr)| {
                    rename(
                        st,
                        ECtx::Tuple {
                            par_expr: expr.clone(),
                            par: Box::new(ctx.clone()),
                            idx: i,
                        },
                        subexpr,
                    )
                })
                .collect();
            Expr::ETuple(new_fields)
        }
        Expr::ETyped { expr: inner, spec } => {
            let new_e = rename(
                st,
                ECtx::Typed {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                inner,
            );
            Expr::ETyped {
                expr: Box::new(new_e),
                spec: spec.clone(),
            }
        }
        Expr::ERef(pattern) => {
            let new_p = rename(
                st,
                ECtx::Ref {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                pattern,
            );
            Expr::ERef(Box::new(new_p))
        }

        // constants — leave unchanged
        Expr::EBool(_) | Expr::EInt(_) | Expr::EFloat(_) | Expr::EString(_) => expr.clone(),

        // Constant expression detected
        _ if expr.is_const() => expr.clone(),

        // Otherwise introduce a new variable
        _ => {
            let vid = st.counter;
            let vname = format!("_{}", vid);
            st.counter += 1;
            st.vmap.push((vname.clone(), expr.clone(), ctx));
            Expr::EVar(Identifier(vname))
        }
    }
}

fn normalize_pattern(prog: &FrameQLProgram, ctx: &ECtx, e: Expr) -> Expr {
    match e {
        // Replace new variables with placeholders
        Expr::EVar(expr_var) if prog.lookup_var(ctx, &expr_var).is_none() => Expr::EPHolder,

        // Replace structs with unique constructors populated entirely by placeholders
        Expr::EStruct { name, fields }
            if prog.cons_is_unique(name.as_str())
                && fields.iter().all(|(_, v)| *v == Expr::EPHolder) =>
        {
            Expr::EPHolder
        }

        // Replace tuples made entirely of placeholders
        Expr::ETuple(expr_tuple_fields)
            if expr_tuple_fields.iter().all(|v| *v == Expr::EPHolder) =>
        {
            Expr::EPHolder
        }

        // A binding just unwraps to its pattern
        Expr::EBinding { pattern, .. } => *pattern.clone(),

        // A reference pointing to placeholder is itself a placeholder
        Expr::ERef(expr_pattern) if *expr_pattern == Expr::EPHolder => Expr::EPHolder,

        // Default: wrap back into Expr
        other => other.clone(),
    }
}

pub fn add_join_arrangement(
    prog: &FrameQLProgram,
    relname: &Identifier,
    pat: Expr,
    arrangements: &mut HashMap<String, Vec<Arrangement>>,
) {
    // streams don’t need arrangements
    if prog
        .get_relation(&Identifier(relname.to_string()))
        .semantics
        == RelSemantics::Stream
    {
        return;
    }

    let arrs = arrangements
        .entry(relname.to_string())
        .or_insert_with(Vec::new);

    // find existing arrangement with same pattern and not distinct
    let existing_idx = arrs.iter().position(|a: &Arrangement| match a {
        Arrangement::Map(expr) => *expr == pat,
        Arrangement::Set(expr, is_distinct) => *expr == pat && !is_distinct,
    });

    // create join arrangement
    let join_arr = Arrangement::Map(pat);

    // replace or append
    if let Some(idx) = existing_idx {
        arrs[idx] = join_arr;
    } else {
        arrs.push(join_arr);
    }
}

pub fn add_semijoin_arrangement(
    prog: &FrameQLProgram,
    relname: &Identifier,
    pat: Expr,
    arrangements: &mut HashMap<String, Vec<Arrangement>>,
) {
    // streams don’t need arrangements
    if prog
        .get_relation(&Identifier(relname.to_string()))
        .semantics
        == RelSemantics::Stream
    {
        return;
    }

    let arrs = arrangements
        .entry(relname.to_string())
        .or_insert_with(Vec::new);

    if let Some(idx) = arrs.iter().position(|a: &Arrangement| match a {
        Arrangement::Map(expr) => *expr == pat,
        Arrangement::Set(expr, is_distinct) => *expr == pat && !is_distinct,
    }) {
        // update existing arrangement with new usage position
        let updated = arrs[idx].clone();
        arrs[idx] = updated;
    } else {
        // insert a new semijoin arrangement
        arrs.push(Arrangement::Set(pat, false));
    }
}
