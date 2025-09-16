use frameql_ast::{FnArg, Identifier};

use crate::{
    expr::{ECtx, Expr},
    func::Function,
    relation::Relation,
    rule::Rule,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Var {
    /// Variable declared in an expression ('var v').
    ExprVar { var_ctx: ECtx, var_expr: Expr },
    /// Variable declared in a @-binding.
    BindingVar { var_ctx: ECtx, var_expr: Expr },
    /// Function argument.
    ArgVar {
        var_func: Function,
        var_arg_index: usize,
        var_name: Identifier,
    },
    /// Closure argument.
    ClosureArgVar {
        var_ctx: ECtx,
        var_expr: Expr,
        var_arg_index: usize,
    },
    /// Primary key variable.
    KeyVar { var_rel: Relation },
    /// Variable returned by group_by.
    GroupVar { var_rule: Rule, var_rhs_idx: usize },
}

impl Var {
    pub fn name(&self) -> Identifier {
        match self {
            // ExprVar: check if the node is a declaration or a plain variable
            Var::ExprVar { var_expr, .. } => match var_expr {
                Expr::EVarDecl(expr_v_name) => expr_v_name.clone(),
                Expr::EVar(ident) => ident.clone(),
                e => panic!("ExprVar.name: unexpected expression {:?}", e),
            },

            // BindingVar: only valid for EBinding
            Var::BindingVar { var_expr, .. } => match var_expr {
                Expr::EBinding { var, .. } => var.clone(),
                e => panic!("BindingVar.name: unexpected expression {:?}", e),
            },

            // ArgVar: the name is stored directly
            Var::ArgVar { var_name, .. } => var_name.clone(),

            // ClosureArgVar: lookup argument i from exprClosureArgs
            Var::ClosureArgVar {
                var_expr,
                var_arg_index,
                ..
            } => {
                if let Expr::EClosure { args, .. } = var_expr {
                    args[*var_arg_index].name.clone()
                } else {
                    panic!("ClosureArgVar.name: unexpected expression {:?}", var_expr);
                }
            }
            Var::KeyVar { var_rel } => {
                let pk = var_rel
                    .primary_key
                    .clone()
                    .expect("KeyVar.name: relation has no primary key");
                pk.0.clone()
            }
            Var::GroupVar {
                var_rule,
                var_rhs_idx,
            } => {
                let rhs = &var_rule.body[*var_rhs_idx];
                rhs.var().clone()
            }
        }
    }
}

pub(crate) fn arg2v(f: &Function, a: &FnArg) -> Var {
    let i = f
        .args()
        .iter()
        .position(|arg| arg.name == a.name)
        .expect("FuncArg not found in function args");

    Var::ArgVar {
        var_func: f.clone(),
        var_arg_index: i,
        var_name: a.name.clone(),
    }
}
