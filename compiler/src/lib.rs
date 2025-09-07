use frameql_ast::{Datalog, Expr, Function, Identifier, Relation, Rule};
/// Expression context: where an expression occurs, what variables are in scope,
/// and what type is expected.
#[derive(Debug, Clone, PartialEq)]
pub enum ECtx {
    /// Top-level context. Expressions cannot appear here.
    CtxTop,

    /// Function definition: `function f(...) = {X}`
    CtxFunc { ctx_func: Function },

    /// Argument to an atom in the left-hand side of a rule:
    /// `Rel1[X] :- ...`
    CtxRuleLAtom { ctx_rule: Rule, ctx_head_idx: usize },

    /// Location component of a rule head: `Rel1() @X :- ...`
    CtxRuleLLocation { ctx_rule: Rule, ctx_head_idx: usize },

    /// Argument to a right-hand-side atom
    CtxRuleRAtom { ctx_rule: Rule, ctx_atom_idx: usize },

    /// Filter or assignment expression in the RHS of a rule
    CtxRuleRCond { ctx_rule: Rule, ctx_idx: usize },

    /// Right-hand side of a FlatMap clause in RHS of a rule
    CtxRuleRFlatMap { ctx_rule: Rule, ctx_idx: usize },

    /// Left-hand side of a FlatMap clause in RHS of a rule
    CtxRuleRFlatMapVars { ctx_rule: Rule, ctx_idx: usize },

    /// Inspect clause in RHS of a rule
    CtxRuleRInspect { ctx_rule: Rule, ctx_idx: usize },

    /// Projection expression in a group_by clause in RHS of a rule
    CtxRuleRProject { ctx_rule: Rule, ctx_idx: usize },

    /// Group-by expression in RHS of a rule
    CtxRuleRGroupBy { ctx_rule: Rule, ctx_idx: usize },

    /// Key expression
    CtxKey { ctx_relation: Relation },

    /// Argument of a function call
    CtxApplyArg {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
        ctx_idx: usize,
    },

    /// Function or closure being invoked
    CtxApplyFunc {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Field expression: `X.f`
    CtxField {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Tuple field expression: `X.N`
    CtxTupField {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Argument passed to a type constructor: `Cons(X, y, z)`
    CtxStruct {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
        ctx_arg: (usize, Identifier),
    },

    /// Argument passed to a tuple expression: `(X, y, z)`
    CtxTuple {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
        ctx_idx: usize,
    },

    /// Bit slice: `X[h:l]`
    CtxSlice {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Argument of a match expression: `match (X) {...}`
    CtxMatchExpr {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Match pattern: `match (...) {X: e1, ...}`
    CtxMatchPat {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
        ctx_idx: usize,
    },

    /// Value returned by a match clause: `match (...) {p1: X, ...}`
    CtxMatchVal {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
        ctx_idx: usize,
    },

    /// First expression in a sequence `X; y`
    CtxSeq1 {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Second expression in a sequence `y; X`
    CtxSeq2 {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// `if (X) ... else ...`
    CtxITEIf {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// `if (cond) X else ...`
    CtxITEThen {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// `if (cond) ... else X`
    CtxITEElse {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// `for (X in ..)`
    CtxForVars {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// `for (.. in e)`
    CtxForIter {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// `for (.. in ..) e`
    CtxForBody {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Left-hand side of an assignment: `X = y`
    CtxSetL {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Right-hand side of an assignment: `y = X`
    CtxSetR {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Argument of a `return` statement
    CtxReturn {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// First operand of a binary operator: `X op y`
    CtxBinOpL {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Second operand of a binary operator: `y op X`
    CtxBinOpR {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Operand of a unary operator: `op X`
    CtxUnOp {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Pattern of a @-expression `v@pat`
    CtxBinding {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Argument of a typed expression `X: t`
    CtxTyped {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Argument of a type cast expression `X as t`
    CtxAs {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Argument of a &-pattern `&e`
    CtxRef {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Argument of a ?-expression `e?`
    CtxTry {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },

    /// Expression inside a closure
    CtxClosure {
        ctx_par_expr: Expr,
        ctx_par: Box<ECtx>,
    },
}

fn progExprMapCtxM<F>(prog: Datalog, f: F) -> Datalog
where
    F: FnMut(&ECtx, &Expr) -> Expr,
{
    let rels = prog
        .relations
        .iter()
        .map(|r| (r.0, relExprMapCtxM(r.1, f)))
        .collect();

    let funcs = prog
        .functions
        .into_iter()
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
        .iter()
        .map(|r| (r.0, ruleExprMapCtxM(r.1, f)))
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

fn ruleExprMapCtxM<F>(prog: Rule, f: F) -> Rule
where
    F: FnMut(&ECtx, &Expr) -> Expr,
{
    todo!()
}
