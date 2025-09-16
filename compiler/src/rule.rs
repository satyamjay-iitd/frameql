use frameql_ast::Identifier;

use crate::expr::{ECtx, Expr, expr_fold_ctx};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Rule {
    pub head: Vec<RuleLHS>,
    pub body: Vec<RuleRHS>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Atom {
    pub relation: Identifier,
    pub val: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RuleLHS(pub Atom);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RuleRHS {
    Literal(Atom),
    Cond(Expr),
    GroupBy {
        ident: Identifier,
        group_by: Expr,
        project: Expr,
    },
    FlatMap {
        vars: Expr,
        map: Expr,
    },
}

impl RuleRHS {
    pub(crate) fn var(&self) -> &Identifier {
        match self {
            RuleRHS::GroupBy { ident, .. } => ident,
            _ => panic!("RuleRHS::var called on non-GroupBy variant"),
        }
    }
}

pub(crate) fn rule_expr_map<F>(rule: Rule, f: F) -> Rule
where
    F: FnMut(&ECtx, Expr) -> Expr + Clone,
{
    // Map over LHS with index
    let lhs: Vec<RuleLHS> = rule
        .head
        .iter()
        .enumerate()
        .map(|(i, lhs)| {
            let RuleLHS(atom) = lhs;

            // Apply f to atom
            let new_atom = atom_expr_map_ctx(
                f.clone(),
                &ECtx::RuleLAtom {
                    head_idx: i,
                    rule: rule.clone(),
                },
                atom,
            );

            RuleLHS(new_atom)
        })
        .collect();
    // Map over RHS with index
    let rhs: Vec<RuleRHS> = rule
        .body
        .iter()
        .enumerate()
        .map(|(i, rhs)| rhs_expr_map_ctx(f.clone(), &rule, i, rhs))
        .collect();

    // Return updated Rule
    Rule {
        head: lhs,
        body: rhs,
    }
}

fn rhs_expr_map_ctx<F>(mut f: F, rule: &Rule, idx: usize, rhs: &RuleRHS) -> RuleRHS
where
    F: FnMut(&ECtx, Expr) -> Expr + Clone,
{
    match rhs {
        RuleRHS::Literal(atom) => RuleRHS::Literal(atom_expr_map_ctx(
            f,
            &ECtx::RuleLAtom {
                rule: rule.clone(),
                head_idx: idx,
            },
            atom,
        )),
        RuleRHS::Cond(expr) => RuleRHS::Cond(expr_fold_ctx(
            &mut f,
            &ECtx::RuleRCond {
                rule: rule.clone(),
                idx,
            },
            expr,
        )),
        RuleRHS::GroupBy {
            ident,
            group_by,
            project,
        } => RuleRHS::GroupBy {
            ident: ident.clone(),
            group_by: expr_fold_ctx(
                &mut f,
                &ECtx::RuleRGroupBy {
                    rule: rule.clone(),
                    idx,
                },
                group_by,
            ),
            project: expr_fold_ctx(
                &mut f,
                &ECtx::RuleRProject {
                    rule: rule.clone(),
                    idx,
                },
                project,
            ),
        },
        RuleRHS::FlatMap { vars, map } => RuleRHS::FlatMap {
            vars: expr_fold_ctx(
                &mut f,
                &ECtx::RuleRFlatMap {
                    rule: rule.clone(),
                    idx: idx,
                },
                vars,
            ),
            map: expr_fold_ctx(
                &mut f,
                &ECtx::RuleRFlatMapVars {
                    rule: rule.clone(),
                    idx: idx,
                },
                map,
            ),
        },
    }
}

fn atom_expr_map_ctx<F>(mut f: F, ctx: &ECtx, atom: &Atom) -> Atom
where
    F: FnMut(&ECtx, Expr) -> Expr + Clone,
{
    Atom {
        relation: atom.relation.clone(),
        val: expr_fold_ctx(&mut f, ctx, &atom.val),
    }
}
