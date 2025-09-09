use frameql_ast::{Datalog, Expr, Function, Identifier, Relation, RuleDecl};

use crate::expr::ECtx;

mod expr;

fn progExprMapCtxM<F>(mut prog: Datalog, f: F) -> Datalog
where
    F: FnMut(&ECtx, &Expr) -> Expr + Clone,
{
    let rels = prog
        .relations
        .drain(..)
        .map(|r| (r.0, relExprMapCtxM(r.1, f.clone())))
        .collect();

    let funcs = prog
        .functions
        .drain(..)
        .map(|(attr, func)| {
            (
                attr,
                match func {
                    Function::FnDef(fn_def) => todo!(),
                    Function::ExternFn(extern_fn) => Function::ExternFn(extern_fn),
                },
            )
            // func.1.func_def = match func.1.func_def {
            //     None => None,
            //     Some(e) => {
            //         let new_e = expr_fold_ctx(&mut f, &ECtx::CtxFunc(func.clone()), e);
            //         Some(new_e)
            //     }
            // };
            // func
        })
        .collect();
    // let funcs = prog.functions.iter().map(|func| todo!()).collect();

    let rules = prog
        .rules
        .drain(..)
        .map(|r| (r.0, ruleExprMapCtxM(r.1, f.clone())))
        .collect();

    Datalog {
        relations: rels,
        functions: funcs,
        rules,
        ..prog.clone()
    }
}

fn relExprMapCtxM<F>(prog: Relation, f: F) -> Relation
where
    F: FnMut(&ECtx, &Expr) -> Expr,
{
    todo!()
}

fn ruleExprMapCtxM<F>(prog: RuleDecl, f: F) -> RuleDecl
where
    F: FnMut(&ECtx, &Expr) -> Expr,
{
    todo!()
}
