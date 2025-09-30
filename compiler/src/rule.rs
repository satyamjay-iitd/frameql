use frameql_ast::Identifier;

use crate::expr::{ECtx, Expr, expr_fold_ctx};

use frameql_ast::RuleDecl as ASTRule;

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

impl From<ASTRule> for Rule {
    fn from(value: ASTRule) -> Self {
        Rule {
            head: value.head.into_iter().map(|head| head.into()).collect(),
            body: value.body.into_iter().map(|body| body.into()).collect(),
        }
    }
}

impl From<frameql_ast::Atom> for RuleLHS {
    fn from(value: frameql_ast::Atom) -> Self {
        RuleLHS(value.into())
    }
}
impl From<frameql_ast::Atom> for Atom {
    fn from(value: frameql_ast::Atom) -> Self {
        match value {
            frameql_ast::Atom::Positional(atom) => Atom {
                relation: atom.rel.clone(),
                val: Expr::EStruct {
                    name: atom.rel,
                    fields: atom
                        .args
                        .into_iter()
                        .map(|x| (Identifier("".to_string()), x.into()))
                        .collect(),
                },
            },
            frameql_ast::Atom::Named(atom) => Atom {
                relation: atom.rel.clone(),
                val: Expr::EStruct {
                    name: atom.rel,
                    fields: atom.args.into_iter().map(|(i, x)| (i, x.into())).collect(),
                },
            },
            frameql_ast::Atom::Indexed(atom) => Atom {
                relation: atom.rel,
                val: atom.index.into(),
            },
        }
    }
}
impl From<frameql_ast::RhsClause> for RuleRHS {
    fn from(value: frameql_ast::RhsClause) -> Self {
        match value {
            frameql_ast::RhsClause::Atom(atom) => RuleRHS::Literal(atom.into()),
            frameql_ast::RhsClause::Expr(expr) => RuleRHS::Cond(expr.into()),
            frameql_ast::RhsClause::Equality(expr, expr1) => RuleRHS::Cond(Expr::EBinOp {
                op: frameql_ast::BinaryOp::Eq,
                left: Box::new(expr.into()),
                right: Box::new(expr1.into()),
            }),
            frameql_ast::RhsClause::GroupBy { var, value, key } => RuleRHS::GroupBy {
                ident: var,
                project: value.into(),
                group_by: key.into(),
            },
            frameql_ast::RhsClause::Not(_) => todo!(),
        }
    }
}
