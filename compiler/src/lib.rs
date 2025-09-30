use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use frameql_ast::Identifier;
use petgraph::dot::{Config, Dot};
use petgraph::{
    algo::{condensation, toposort},
    graph::NodeIndex,
};

use crate::{
    compile::{
        Arrangement, CompilerState, DepGraphNode, create_arrangements, normalize_arrangement,
        prog_dependency_graph,
    },
    expr::{ECtx, Expr, expr_collect_ctx},
    prog::FrameQLProgram,
    relation::RelRole,
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

#[derive(Debug)]
pub struct ProgRel {
    name: String,
}

#[derive(Debug)]
pub struct RecProgRel {
    p_rel: ProgRel,
    is_distinct: bool,
}

#[derive(Debug)]
pub enum ProgNode {
    SCCNode(Vec<RecProgRel>),
    RelNode(ProgRel),
}

pub fn compile_prog(prog: FrameQLProgram) -> Vec<ProgNode> {
    let statics = collect_statics(&prog);
    compile_rules(&prog, &statics).0
}

fn collect_statics(prog: &FrameQLProgram) -> Statics {
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

pub fn compile_rules(prog: &FrameQLProgram, statics: &Statics) -> (Vec<ProgNode>, CompilerState) {
    // 1. Compute dependency graph
    let depgraph: petgraph::Graph<DepGraphNode, ()> = prog_dependency_graph(prog);

    let dot = Dot::with_config(&depgraph, &[Config::EdgeNoLabel]);
    println!("{:?}", dot);

    // 2. Compute SCCs (condensation + topological sort)
    let condensation: petgraph::Graph<Vec<DepGraphNode>, ()> = condensation(depgraph.clone(), true);

    let sccs: Vec<NodeIndex> = toposort(&condensation, None).expect("Cyclic dependency in program");

    // 3. Initialize arrangements map
    let arrs: HashMap<String, Vec<Arrangement>> = prog
        .relations
        .keys()
        .map(|rel| (rel.clone(), Vec::new()))
        .collect();

    // 4. Initialize compiler state
    let mut state = CompilerState { arrangements: arrs };

    // 5. First pass: compute arrangements
    create_arrangements(prog, &mut state);

    // 6. Second pass: allocate delayed relation ids
    // alloc_delayed_rel_ids(prog, &mut state);

    // 7. Third pass: compile SCCs
    let mut prog_nodes = Vec::new();
    for scc in sccs.iter() {
        let node = compile_scc(
            prog,
            statics,
            &depgraph,
            condensation.node_weight(*scc).unwrap(),
            &mut state,
        );
        prog_nodes.push(node);
    }

    (prog_nodes, state)
}

fn compile_scc(
    prog: &FrameQLProgram,
    statics: &HashMap<(Expr, Type), i32>,
    depgraph: &petgraph::Graph<DepGraphNode, ()>,
    scc_nodes: &Vec<DepGraphNode>,
    state: &mut CompilerState,
) -> ProgNode {
    let mut is_recursive = false;
    for e_idx in depgraph.edge_indices() {
        let (n1, n2): (NodeIndex, NodeIndex) = depgraph.edge_endpoints(e_idx).unwrap();
        let n1 = depgraph.node_weight(n1).unwrap();
        let n2 = depgraph.node_weight(n2).unwrap();
        if scc_nodes.contains(n1) && scc_nodes.contains(n2) {
            is_recursive = true;
            break;
        }
    }
    if is_recursive {
        // Extract relation names from all nodes in SCC
        let relnames: Vec<Identifier> = scc_nodes
            .iter()
            .map(|n| match n {
                DepGraphNode::Rel(identifier) => identifier.clone(),
            })
            .collect();

        return compile_scc_node(prog, statics, &relnames, state);
    }

    let depnode = &scc_nodes[0];

    match depnode {
        DepGraphNode::Rel(rel) => compile_rel_node(prog, statics, rel, state),
    }
}

fn compile_scc_node(
    prog: &FrameQLProgram,
    statics: &HashMap<(Expr, Type), i32>,
    rel: &Vec<Identifier>,
    state: &mut CompilerState,
) -> ProgNode {
    ProgNode::SCCNode(
        rel.iter()
            .map(|r| RecProgRel {
                p_rel: compile_relation(prog, statics, r, state),
                is_distinct: false,
            })
            .collect(),
    )
}

fn compile_rel_node(
    prog: &FrameQLProgram,
    statics: &HashMap<(Expr, Type), i32>,
    rel: &Identifier,
    state: &mut CompilerState,
) -> ProgNode {
    ProgNode::RelNode(compile_relation(prog, statics, rel, state))
}

fn compile_relation(
    prog: &FrameQLProgram,
    statics: &HashMap<(Expr, Type), i32>,
    rel: &Identifier,
    state: &mut CompilerState,
) -> ProgRel {
    let rel = prog.get_relation(rel);

    // helper closures
    let rule_name = |i: usize| -> String { format!("__Rule_{}_{}", rel.name, i) };

    let fact_name = |i: usize| -> String { format!("__Fact_{}_{}", rel.name, i) };

    let arng_name = |i: usize| -> String { format!("__Arng_{}_{}", rel.name, i) };

    let all_rules = prog.get_rules(&rel.name);
    let (facts, rules): (Vec<_>, Vec<_>) =
        all_rules.into_iter().partition(|rule| rule.body.is_empty());

    let _rules_prime: Vec<String> = rules
        .into_iter()
        .enumerate()
        .map(|(i, rl)| {
            // compile the rule
            let compiled = compile_rule(prog, &rl, 0, true, &statics);

            // generate the static variable declaration
            let code = format!(
                "pub static {}: ::once_cell::sync::Lazy<program::Rule> = \
             ::once_cell::sync::Lazy::new(|| {{\n{}\n}});",
                rule_name(i),
                &compiled
            );

            code
        })
        .collect();

    let _facts_prime: Vec<(String, String)> = facts
        .into_iter()
        .enumerate()
        .map(|(i, fact)| {
            // compile the fact (String or AST)
            let fact_body = compile_fact(prog, &fact, &statics);

            // second element: identifier reference with .clone()
            let fact_ref = format!("{}.clone()", &fact_name(i));

            // third element: static declaration string
            let decl = format!(
                "pub static {}: ::once_cell::sync::Lazy<(program::RelId, \
             ::differential_datalog::ddval::DDValue)> = \
             ::once_cell::sync::Lazy::new(|| {{\n{}\n}});",
                fact_name(i),
                &fact_body
            );

            (fact_ref, decl)
        })
        .collect();

    // 1. Callback setting
    let _cb = if rel.role == RelRole::Output {
        "change_cb: ::core::option::Option::Some(::std::sync::Arc::clone(&__update_cb)),"
            .to_string()
    } else {
        "change_cb: ::core::option::Option::None,".to_string()
    };

    // 2. Get arrangements from compiler state
    let arrangements = state
        .arrangements
        .get(rel.name.as_str())
        .cloned()
        .unwrap_or_default();

    // 3. Compile arrangements
    let mut compiled_arrangements = Vec::new();

    for (i, arng) in arrangements.into_iter().enumerate() {
        let arng_code = mk_arrangement(prog, &rel, &arng, &statics);

        let arng_static = format!(
            "pub static {}: ::once_cell::sync::Lazy<program::Arrangement> = \
         ::once_cell::sync::Lazy::new(|| {{\n{}\n}});",
            arng_name(i),
            &arng_code
        );

        compiled_arrangements.push(arng_static);
    }

    ProgRel {
        name: rel.name.to_string(),
    }
}

fn mk_arrangement(
    _prog: &FrameQLProgram,
    _rel: &relation::Relation,
    _arng: &Arrangement,
    _statics: &&HashMap<(Expr, Type), i32>,
) -> String {
    "".to_string()
}

fn compile_fact(
    _prog: &FrameQLProgram,
    _fact: &Rule,
    _statics: &&HashMap<(Expr, Type), i32>,
) -> String {
    "".to_string()
}

fn compile_rule(
    _prog: &FrameQLProgram,
    _rl: &Rule,
    _arg_1: i32,
    _arg_2: bool,
    _statics: &&HashMap<(Expr, Type), i32>,
) -> String {
    "".to_string()
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
