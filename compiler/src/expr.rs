use frameql_ast::{BinaryOp, Expr as ASTExpr, FnArg, Identifier, Term as ASTTerm, UnaryOp};
use ordered_float::OrderedFloat;

use crate::{
    func::Function, prog::FrameQLProgram, relation::Relation, rule::Rule, r#type::Type, var::Var,
};

// #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
// pub enum BOp {
//     Eq,
//     Neq,
//     Lt,
//     Gt,
//     Lte,
//     Gte,
//     And,
//     Or,
//     Impl,
//     Plus,
//     Minus,
//     Mod,
//     Times,
//     Div,
//     ShiftR,
//     ShiftL,
//     BAnd,
//     BOr,
//     BXor,
//     Concat,
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Expr {
    EVar(Identifier),
    EApply {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    EField {
        struct_: Box<Expr>,
        field: Identifier,
    },
    ETupField {
        tuple: Box<Expr>,
        field: usize,
    },
    EBool(bool),
    EInt(i64),
    EFloat(OrderedFloat<f64>),
    EString(String),
    EStruct {
        name: Identifier,
        fields: Vec<(Identifier, Expr)>,
    },
    ETuple(Vec<Expr>),
    // ESlice {
    //     array: Box<Expr>,
    //     high: u64,
    //     low: u64,
    // },
    EMatch {
        clause: Box<Expr>,
        body: Vec<(Expr, Expr)>,
    },
    EVarDecl(Identifier),
    ESeq {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    EITE {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    EFor {
        loop_var: Box<Expr>,
        iter_: Box<Expr>,
        body: Box<Expr>,
    },
    ESet {
        lval: Box<Expr>,
        rval: Box<Expr>,
    },
    EBreak,
    EContinue,
    EReturn(Box<Expr>),
    EBinOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    EUnOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    EBinding {
        var: Identifier,
        pattern: Box<Expr>,
    },
    ETyped {
        expr: Box<Expr>,
        spec: Type,
    },
    EAs {
        expr: Box<Expr>,
        spec: Type,
    },
    ERef(Box<Expr>),
    ETry(Box<Expr>),
    EClosure {
        args: Vec<FnArg>,
        ret_type: Option<Type>,
        body: Box<Expr>,
    },
    EFunc {
        name: Vec<Identifier>,
    },
    EPHolder,
}

impl Expr {
    fn vars(&self, prog: &FrameQLProgram, ctx: &ECtx) -> Vec<Var> {
        // The collector function: given a context and subexpression, return vars
        let collect = |ctx_prime: &ECtx, e_prime: &Expr| -> Vec<Var> {
            match e_prime {
                Expr::EVar(name) => match prog.lookup_var(ctx_prime, name) {
                    Some(var) => vec![var],
                    None => vec![Var::ExprVar {
                        var_ctx: ctx_prime.clone(),
                        var_expr: Expr::EVar(name.clone()).into(),
                    }],
                },
                _ => vec![],
            }
        };

        let mut vars = expr_collect_ctx(
            collect,
            |mut a, mut b| {
                a.append(&mut b);
                a
            },
            ctx,
            self,
        );

        vars.sort();
        vars.dedup();
        vars
    }

    pub(crate) fn expr_closure_args(&self) -> Vec<FnArg> {
        match self {
            Expr::EClosure { args, .. } => args.clone(),
            _ => panic!("Shouldn't happen"),
        }
    }

    pub(crate) fn is_const(&self) -> bool {
        false
        // self.vars(None, None).is_empty()
    }

    pub fn field_expr_var(&self) -> Option<&Identifier> {
        match self {
            Expr::EVar(v) => Some(v),
            Expr::ETyped { expr, .. } => expr.field_expr_var(),
            Expr::EField { struct_, .. } => struct_.field_expr_var(),
            Expr::ETupField { tuple, .. } => tuple.field_expr_var(),
            _ => None,
        }
    }
}

pub trait ExprVisitor<T> {
    #[allow(unused)]
    fn visit_expr(&mut self, e: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_var(&mut self, n: &Identifier) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_apply(&mut self, func: &Expr, args: Vec<Expr>) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_field(&mut self, struct_: &Expr, field: &Identifier) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_tup_field(&mut self, tuple: &Expr, field: usize) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_bool(&mut self, val: &bool) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_int(&mut self, val: &u64) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_string(&mut self, val: &str) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_float(&mut self, val: &OrderedFloat<f64>) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_struc(&mut self, name: &Identifier, fields: &Vec<(Identifier, Expr)>) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_tuple(&mut self, tuple: &Vec<Expr>) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_slice(&mut self, array: &Expr, high: u64, low: u64) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_match(&mut self, clause: &Expr, body: &Vec<(Expr, Expr)>) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_var_decl(&mut self, ident: &Identifier) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_seq(&mut self, left: &Expr, right: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_ite(&mut self, cond: &Expr, then: &Expr, else_: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_for(&mut self, loop_var: &Expr, iter: &Expr, body: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_set(&mut self, lval: &Expr, rval: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_break(&mut self) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_continue(&mut self) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_return(&mut self, val: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_bin_op(&mut self, op: BinaryOp, left: &Expr, right: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_un_op(&mut self, op: UnaryOp, expr: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_binding(&mut self, var: &Identifier, pattern: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_typed(&mut self, expr: &Expr, spec: &Type) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_as(&mut self, expr: &Expr, spec: &Type) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_ref(&mut self, expr: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_try(&mut self, expr: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_closure(&mut self, args: &Vec<FnArg>, ret_type: Option<Type>, body: &Expr) -> T {
        unimplemented!();
    }
    #[allow(unused)]
    fn visit_fn(&mut self, name: &Vec<Identifier>) -> T {
        unimplemented!();
    }
}

/// Expression context: where an expression occurs, what variables are in scope,
/// and what type is expected.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ECtx {
    /// Top-level context. Expressions cannot appear here.
    Top,

    /// Function definition: `function f(...) = {X}`
    Func(Function),

    /// Argument to an atom in the left-hand side of a rule:
    /// `Rel1[X] :- ...`
    RuleLAtom {
        rule: Rule,
        head_idx: usize,
    },

    /// Location component of a rule head: `Rel1() @X :- ...`
    RuleLLocation {
        rule: Rule,
        head_idx: usize,
    },

    /// Argument to a right-hand-side atom
    RuleRAtom {
        rule: Rule,
        idx: usize,
    },

    /// Filter or assignment expression in the RHS of a rule
    RuleRCond {
        rule: Rule,
        idx: usize,
    },

    /// Right-hand side of a FlatMap clause in RHS of a rule
    RuleRFlatMap {
        rule: Rule,
        idx: usize,
    },

    /// Left-hand side of a FlatMap clause in RHS of a rule
    RuleRFlatMapVars {
        rule: Rule,
        idx: usize,
    },

    /// Projection expression in a group_by clause in RHS of a rule
    RuleRProject {
        rule: Rule,
        idx: usize,
    },

    /// Group-by expression in RHS of a rule
    RuleRGroupBy {
        rule: Rule,
        idx: usize,
    },

    /// Key expression
    Key {
        relation: Relation,
    },

    /// Argument of a function call
    ApplyArg {
        par_expr: Expr,
        par: Box<ECtx>,
        idx: usize,
    },

    /// Function or closure being invoked
    ApplyFunc {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Field expression: `X.f`
    Field {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Tuple field expression: `X.N`
    TupField {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Argument passed to a type constructor: `Cons(X, y, z)`
    Struct {
        par_expr: Expr,
        par: Box<ECtx>,
        arg: (usize, Identifier),
    },

    /// Argument passed to a tuple expression: `(X, y, z)`
    Tuple {
        par_expr: Expr,
        par: Box<ECtx>,
        idx: usize,
    },

    /// Bit slice: `CtxTupFied[h:l]`
    // Slice {
    //     par_expr: Expr,
    //     par: Box<ECtx>,
    // },

    /// Argument of a match expression: `match (X) {...}`
    MatchExpr {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Match pattern: `match (...) {X: e1, ...}`
    MatchPat {
        par_expr: Expr,
        par: Box<ECtx>,
        idx: usize,
    },

    /// Value returned by a match clause: `match (...) {p1: X, ...}`
    MatchVal {
        par_expr: Expr,
        par: Box<ECtx>,
        idx: usize,
    },

    /// First expression in a sequence `X; y`
    Seq1 {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Second expression in a sequence `y; X`
    Seq2 {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// `if (X) ... else ...`
    ITEIf {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// `if (cond) X else ...`
    ITEThen {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// `if (cond) ... else X`
    ITEElse {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// `for (X in ..)`
    ForVars {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// `for (.. in e)`
    ForIter {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// `for (.. in ..) e`
    ForBody {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Left-hand side of an assignment: `X = y`
    SetL {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Right-hand side of an assignment: `y = X`
    SetR {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Argument of a `return` statement
    Return {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// First operand of a binary operator: `X op y`
    BinOpL {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Second operand of a binary operator: `y op X`
    BinOpR {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Operand of a unary operator: `op X`
    UnOp {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Pattern of a @-expression `v@pat`
    Binding {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Argument of a typed expression `X: t`
    Typed {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Argument of a type cast expression `X as t`
    As {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Argument of a &-pattern `&e`
    Ref {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Argument of a ?-expression `e?`
    Try {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    /// Expression inside a closure
    Closure {
        par_expr: Expr,
        par: Box<ECtx>,
    },

    Undefined,
}

impl ECtx {
    pub(crate) fn parent(&self) -> ECtx {
        match self {
            ECtx::RuleLAtom { .. }
            | ECtx::RuleLLocation { .. }
            | ECtx::RuleRAtom { .. }
            | ECtx::RuleRCond { .. }
            | ECtx::RuleRFlatMap { .. }
            | ECtx::RuleRFlatMapVars { .. }
            | ECtx::RuleRProject { .. }
            | ECtx::RuleRGroupBy { .. }
            | ECtx::Key { .. }
            | ECtx::Top
            | ECtx::Undefined
            | ECtx::Func(_) => ECtx::Top,
            ECtx::ApplyArg { par, .. }
            | ECtx::ApplyFunc { par, .. }
            | ECtx::Field { par, .. }
            | ECtx::TupField { par, .. }
            | ECtx::Struct { par, .. }
            | ECtx::Tuple { par, .. }
            // | ECtx::Slice { par, .. }
            | ECtx::MatchExpr { par, .. }
            | ECtx::MatchPat { par, .. }
            | ECtx::MatchVal { par, .. }
            | ECtx::Seq1 { par, .. }
            | ECtx::Seq2 { par, .. }
            | ECtx::ITEIf { par, .. }
            | ECtx::ITEThen { par, .. }
            | ECtx::ITEElse { par, .. }
            | ECtx::ForVars { par, .. }
            | ECtx::ForIter { par, .. }
            | ECtx::ForBody { par, .. }
            | ECtx::SetL { par, .. }
            | ECtx::SetR { par, .. }
            | ECtx::Return { par, .. }
            | ECtx::BinOpL { par, .. }
            | ECtx::BinOpR { par, .. }
            | ECtx::UnOp { par, .. }
            | ECtx::Binding { par, .. }
            | ECtx::Typed { par, .. }
            | ECtx::As { par, .. }
            | ECtx::Ref { par, .. }
            | ECtx::Try { par, .. }
            | ECtx::Closure { par, .. } => *par.clone(),
        }
    }

    pub(crate) fn in_rule_rhs(&self) -> bool {
        match self {
            ECtx::RuleRAtom { .. } => true,
            ECtx::Struct { par, .. }
            | ECtx::Tuple { par, .. }
            | ECtx::Typed { par, .. }
            | ECtx::Binding { par, .. }
            | ECtx::Ref { par, .. } => par.in_rule_rhs(),
            _ => false,
        }
    }

    pub(crate) fn expect_type(&self) -> Type {
        match self {
            ECtx::Typed { par_expr, .. } => match par_expr {
                Expr::ETyped { spec, .. } => spec.clone(),
                _ => panic!("Unknown type in context, {:?}", self),
            },
            _ => panic!("Unknown type in context, {:?}", self),
        }
    }
}

pub fn expr_fold<F>(mut f: F, expr: &Expr) -> Expr
where
    F: FnMut(&Expr) -> Expr,
{
    expr_fold_ctx(&mut |_, node| f(&node), &ECtx::Undefined, expr)
}
/// Depth-first fold of an expression with *syntactic context*.
/// - `f` is called *after* recursively folding children,
///   receiving the current `ctx` and a node whose children are already of type `B`.
pub(crate) fn expr_fold_ctx<F>(f: &mut F, ctx: &ECtx, expr: &Expr) -> Expr
where
    F: FnMut(&ECtx, Expr) -> Expr,
{
    match expr {
        Expr::EVar(identifier) => f(&ctx, Expr::EVar(identifier.clone())),
        Expr::EApply { func, args } => {
            let ectx = ECtx::ApplyFunc {
                par_expr: expr.clone(),
                par: Box::new(ctx.clone()),
            };
            let func = expr_fold_ctx(f, &ectx, func);

            // fold args with indices
            let mut new_args = Vec::with_capacity(args.len());
            for (i, a) in args.into_iter().enumerate() {
                let folded = expr_fold_ctx(
                    f,
                    &ECtx::ApplyArg {
                        par_expr: expr.clone(),
                        par: Box::new(ctx.clone()),
                        idx: i,
                    },
                    a,
                );
                new_args.push(folded);
            }

            f(
                ctx,
                Expr::EApply {
                    func: Box::new(func),
                    args: new_args,
                },
            )
        }
        Expr::EField { struct_, field } => {
            // fold the inner struct expression
            let struct_ = expr_fold_ctx(
                f,
                &ECtx::Field {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                struct_,
            );

            f(
                ctx,
                Expr::EField {
                    struct_: Box::new(struct_),
                    field: field.clone(),
                },
            )
        }
        Expr::ETupField { tuple, field } => {
            // fold the inner tuple expr
            let tuple = expr_fold_ctx(
                f,
                &ECtx::TupField {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                tuple,
            );

            // rebuild and apply f
            f(
                ctx,
                Expr::ETupField {
                    tuple: Box::new(tuple),
                    field: *field,
                },
            )
        }
        Expr::EBool(val) => f(ctx, Expr::EBool(*val)),
        Expr::EInt(val) => f(ctx, Expr::EInt(*val)),
        Expr::EFloat(val) => f(ctx, Expr::EFloat(*val)),
        Expr::EString(val) => f(ctx, Expr::EString(val.clone())),
        Expr::EStruct { name, fields } => {
            let mut new_fields = Vec::with_capacity(fields.len());

            for (i, (fname, fl)) in fields.into_iter().enumerate() {
                let fl_folded = expr_fold_ctx(
                    f,
                    // &ECtx::CtxStruct{expr.clone(), ctx.clone(), (i, fname.clone())},
                    &ECtx::Struct {
                        par_expr: expr.clone(),
                        par: Box::new(ctx.clone()),
                        arg: (i, fname.clone()),
                    },
                    fl,
                );
                new_fields.push((fname.clone(), fl_folded));
            }

            f(
                ctx,
                Expr::EStruct {
                    name: name.clone(),
                    fields: new_fields,
                },
            )
        }
        Expr::ETuple(exprs) => {
            let mut new_exprs = Vec::with_capacity(exprs.len());

            for (i, e) in exprs.into_iter().enumerate() {
                let folded = expr_fold_ctx(
                    f,
                    &ECtx::Tuple {
                        par_expr: expr.clone(),
                        par: Box::new(ctx.clone()),
                        idx: i,
                    },
                    e,
                );
                new_exprs.push(folded);
            }

            f(ctx, Expr::ETuple(new_exprs))
        }
        // Expr::ESlice { array, high, low } => {
        //     let array = expr_fold_ctx(
        //         f,
        //         &ECtx::Slice {
        //             par_expr: expr.clone(),
        //             par: Box::new(ctx.clone()),
        //         },
        //         array,
        //     );

        //     f(
        //         ctx,
        //         Expr::ESlice {
        //             array: Box::new(array),
        //             high: *high,
        //             low: *low,
        //         },
        //     )
        // }
        Expr::EMatch { clause, body } => {
            // Fold the match expression itself
            let clause =
                    // expr_fold_ctx_m(f, ECtx::CtxMatchExpr{expr.clone(), ctx.clone()}, *clause).await;
                    expr_fold_ctx(f, &ECtx::MatchExpr{ par_expr: expr.clone(), par: Box::new(ctx.clone()) }, clause);

            // Fold each (pattern, value) pair
            let mut new_body = Vec::with_capacity(body.len());
            for (i, (pat, val)) in body.into_iter().enumerate() {
                let pat_folded = expr_fold_ctx(
                    f,
                    &ECtx::MatchPat {
                        par_expr: expr.clone(),
                        par: Box::new(ctx.clone()),
                        idx: i,
                    },
                    pat,
                );

                let val_folded = expr_fold_ctx(
                    f,
                    &ECtx::MatchVal {
                        par_expr: expr.clone(),
                        par: Box::new(ctx.clone()),
                        idx: i,
                    },
                    val,
                );

                new_body.push((pat_folded, val_folded));
            }

            f(
                ctx,
                Expr::EMatch {
                    clause: Box::new(clause),
                    body: new_body,
                },
            )
        }
        Expr::EVarDecl(identifier) => f(ctx, Expr::EVarDecl(identifier.clone())),
        Expr::ESeq { left, right } => {
            let left = expr_fold_ctx(
                f,
                &ECtx::Seq1 {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                left,
            );

            let right = expr_fold_ctx(
                f,
                &ECtx::Seq2 {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                right,
            );

            f(
                ctx,
                Expr::ESeq {
                    left: Box::new(left),
                    right: Box::new(right),
                },
            )
        }
        Expr::EITE { cond, then, else_ } => {
            let cond = expr_fold_ctx(
                f,
                &ECtx::ITEIf {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                cond,
            );

            let then_branch = expr_fold_ctx(
                f,
                &ECtx::ITEThen {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                then,
            );

            let else_branch = expr_fold_ctx(
                f,
                &ECtx::ITEElse {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                else_,
            );

            f(
                ctx,
                Expr::EITE {
                    cond: Box::new(cond),
                    then: Box::new(then_branch),
                    else_: Box::new(else_branch),
                },
            )
        }
        Expr::EFor {
            loop_var,
            iter_,
            body,
        } => {
            let loop_var = expr_fold_ctx(
                f,
                &ECtx::ForVars {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                loop_var,
            );

            let iter_ = expr_fold_ctx(
                f,
                &ECtx::ForIter {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                iter_,
            );

            let body = expr_fold_ctx(
                f,
                &ECtx::ForBody {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                body,
            );

            f(
                ctx,
                Expr::EFor {
                    loop_var: Box::new(loop_var),
                    iter_: Box::new(iter_),
                    body: Box::new(body),
                },
            )
        }
        Expr::ESet { lval, rval } => {
            // fold RHS first
            let rval = expr_fold_ctx(
                f,
                &ECtx::SetR {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                rval,
            );

            // then LHS
            let lval = expr_fold_ctx(
                f,
                &ECtx::SetL {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                lval,
            );

            f(
                ctx,
                Expr::ESet {
                    lval: Box::new(lval),
                    rval: Box::new(rval),
                },
            )
        }
        Expr::EBreak => f(ctx, Expr::EBreak),
        Expr::EContinue => f(ctx, Expr::EContinue),
        Expr::EReturn(expr) => {
            let val = expr_fold_ctx(
                f,
                &ECtx::Return {
                    par_expr: *expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                expr,
            );

            f(ctx, Expr::EReturn(Box::new(val)))
        }
        Expr::EBinOp { op, left, right } => {
            let left = expr_fold_ctx(
                f,
                &ECtx::BinOpL {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                left,
            );

            let right = expr_fold_ctx(
                f,
                &ECtx::BinOpR {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                right,
            );

            f(
                ctx,
                Expr::EBinOp {
                    op: *op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            )
        }
        Expr::EUnOp { op, expr } => {
            let expr = expr_fold_ctx(
                f,
                &ECtx::UnOp {
                    par_expr: *expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                expr,
            );

            f(
                ctx,
                Expr::EUnOp {
                    op: *op,
                    expr: Box::new(expr),
                },
            )
        }
        Expr::EBinding { var, pattern } => {
            let pattern = expr_fold_ctx(
                f,
                &ECtx::Binding {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                pattern,
            );

            f(
                ctx,
                Expr::EBinding {
                    var: var.clone(),
                    pattern: Box::new(pattern),
                },
            )
        }
        Expr::ETyped { expr, spec } => {
            let expr = expr_fold_ctx(
                f,
                &ECtx::Typed {
                    par_expr: *expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                expr,
            );

            f(
                ctx,
                Expr::ETyped {
                    expr: Box::new(expr),
                    spec: spec.clone(),
                },
            )
        }
        Expr::EAs { expr, spec } => {
            let expr = expr_fold_ctx(
                f,
                &ECtx::As {
                    par_expr: *expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                expr,
            );

            f(
                ctx,
                Expr::EAs {
                    expr: Box::new(expr),
                    spec: spec.clone(),
                },
            )
        }
        Expr::ERef(expr) => {
            let expr = expr_fold_ctx(
                f,
                &ECtx::Ref {
                    par_expr: *expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                expr,
            );

            f(ctx, Expr::ERef(Box::new(expr)))
        }
        Expr::ETry(expr) => {
            let expr = expr_fold_ctx(
                f,
                &ECtx::Try {
                    par_expr: *expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                expr,
            );

            f(ctx, Expr::ETry(Box::new(expr)))
        }
        Expr::EClosure {
            args,
            ret_type,
            body,
        } => {
            let body = expr_fold_ctx(
                f,
                &ECtx::Closure {
                    par_expr: expr.clone(),
                    par: Box::new(ctx.clone()),
                },
                body,
            );

            f(
                ctx,
                Expr::EClosure {
                    args: args.clone(),
                    ret_type: ret_type.clone(),
                    body: Box::new(body),
                },
            )
        }
        Expr::EFunc { name } => f(ctx, Expr::EFunc { name: name.clone() }),
        Expr::EPHolder => f(ctx, Expr::EPHolder),
    }
}

pub fn expr_collect_ctx<F, Op, B>(mut f: F, op: Op, ctx: &ECtx, e: &Expr) -> B
where
    F: FnMut(&ECtx, &Expr) -> B,
    Op: FnMut(B, B) -> B + Copy,
    B: Clone,
{
    // Recursive helper
    fn go<F, Op, B>(f: &mut F, mut op: Op, ctx: &ECtx, e: &Expr) -> B
    where
        F: FnMut(&ECtx, &Expr) -> B,
        Op: FnMut(B, B) -> B + Copy,
        B: Clone,
    {
        let x_prime = f(ctx, e);

        match e {
            Expr::EVar(_) => x_prime,
            Expr::EApply { func, args } => {
                let mut acc = op.clone()(x_prime.clone(), go(f, op, ctx, func));
                for arg in args {
                    acc = op.clone()(acc, go(f, op, ctx, arg));
                }
                acc
            }
            Expr::EField { struct_, .. } => op.clone()(x_prime, go(f, op, ctx, struct_)),
            Expr::ETupField { tuple, .. } => op.clone()(x_prime, go(f, op, ctx, tuple)),
            Expr::EBool(_)
            | Expr::EInt(_)
            | Expr::EFloat(_)
            | Expr::EString(_)
            | Expr::EBreak
            | Expr::EContinue => x_prime,
            Expr::EStruct { fields, .. } => fields
                .iter()
                .fold(x_prime, |a, (_, sub)| op.clone()(a, go(f, op, ctx, sub))),
            Expr::ETuple(elems) => elems
                .iter()
                .fold(x_prime, |a, sub| op.clone()(a, go(f, op, ctx, sub))),
            // Expr::ESlice { array, .. } => op.clone()(x_prime, go(f, op, ctx, array)),
            Expr::EMatch { clause, body } => {
                let acc = op.clone()(x_prime.clone(), go(f, op, ctx, clause));
                body.iter().fold(acc, |a, (p, v)| {
                    let a = op.clone()(a, go(f, op, ctx, p));
                    op.clone()(a, go(f, op, ctx, v))
                })
            }
            Expr::EVarDecl(_) => x_prime,
            Expr::ESeq { left, right }
            | Expr::ESet {
                lval: left,
                rval: right,
            }
            | Expr::EBinOp { left, right, .. } => {
                let l_val = go(f, op, ctx, left);
                let r_val = go(f, op, ctx, right);
                op.clone()(op(x_prime, l_val), r_val)
            }
            Expr::EITE { cond, then, else_ } => {
                let i_val = go(f, op, ctx, cond);
                let t_val = go(f, op, ctx, then);
                let el_val = go(f, op, ctx, else_);
                op.clone()(op.clone()(op(x_prime, i_val), t_val), el_val)
            }
            Expr::EFor {
                loop_var,
                iter_,
                body,
            } => {
                let v_val = go(f, op, ctx, loop_var);
                let i_val = go(f, op, ctx, iter_);
                let b_val = go(f, op, ctx, body);
                op.clone()(op.clone()(op(x_prime, v_val), i_val), b_val)
            }
            Expr::EReturn(expr)
            | Expr::EUnOp { expr, .. }
            | Expr::ETyped { expr, .. }
            | Expr::EAs { expr, .. }
            | Expr::ERef(expr)
            | Expr::ETry(expr)
            | Expr::EClosure { body: expr, .. } => {
                let v_val = go(f, op, ctx, expr);
                op(x_prime, v_val)
            }
            Expr::EBinding { pattern, .. } => {
                let pat_val = go(f, op, ctx, pattern);
                op(x_prime, pat_val)
            }
            Expr::EFunc { .. } => x_prime,
            Expr::EPHolder => x_prime,
        }
    }

    go(&mut f, op, ctx, e)
}

impl From<ASTTerm> for Expr {
    fn from(value: ASTTerm) -> Self {
        match value {
            ASTTerm::Int(v) => Expr::EInt(v),
            ASTTerm::Bool(v) => Expr::EBool(v),
            ASTTerm::String(v) => Expr::EString(v),
            ASTTerm::Float(v) => Expr::EFloat(v),
            ASTTerm::Vec(_) => todo!(),
            ASTTerm::Tuple(_) => todo!(),
            ASTTerm::Map(_) => todo!(),
            ASTTerm::ConsTerm { .. } => todo!(),
            ASTTerm::Var(identifier) => Expr::EVar(identifier),
            ASTTerm::Match { scrutinee, clauses } => Expr::EMatch {
                clause: Box::new((*scrutinee).into()),
                body: clauses
                    .into_iter()
                    .map(|(pat, body)| (pat.into(), body.into()))
                    .collect(),
            },
            ASTTerm::IfThenElse {
                cond,
                then_term,
                else_term,
            } => Expr::EITE {
                cond: Box::new((*cond).into()),
                then: Box::new((*then_term).into()),
                else_: match else_term {
                    Some(else_) => Box::new((*else_).into()),
                    None => Box::new(Expr::EPHolder),
                },
            },
            ASTTerm::For { .. } => todo!(),
            ASTTerm::Continue => Expr::EContinue,
            ASTTerm::Break => Expr::EBreak,
            ASTTerm::Return(expr) => Expr::EReturn(Box::new((*expr.unwrap()).into())),
            ASTTerm::VarDecl(identifier) => Expr::EVarDecl(identifier),
            ASTTerm::Lambda { .. } => todo!(),
            ASTTerm::Wildcard => todo!(),
        }
    }
}

impl From<ASTExpr> for Expr {
    fn from(value: ASTExpr) -> Self {
        match value {
            ASTExpr::Term(term) => term.into(),
            ASTExpr::Unary { op, rhs } => Expr::EUnOp {
                op: op,
                expr: Box::new((*rhs).into()),
            },
            ASTExpr::Binary { lhs, op, rhs } => Expr::EBinOp {
                op,
                left: Box::new((*lhs).into()),
                right: Box::new((*rhs).into()),
            },
            ASTExpr::Slice { .. } => todo!(),
            ASTExpr::TypeAnnotation { parent, ty } => Expr::ETyped {
                expr: Box::new((*parent).into()),
                spec: ty.into(),
            },
            ASTExpr::FCall { func, args } => Expr::EApply {
                func: Box::new((*func).into()),
                args: args.into_iter().map(|a| a.into()).collect(),
            },
            ASTExpr::DotFCall { .. } => todo!(),
            ASTExpr::Field { base, field } => Expr::EField {
                struct_: Box::new((*base).into()),
                field,
            },
            ASTExpr::TupleIndex { base, index } => Expr::ETupField {
                tuple: Box::new((*base).into()),
                field: index as usize,
            },
            ASTExpr::Cast { base, ty } => Expr::EAs {
                expr: Box::new((*base).into()),
                spec: ty.into(),
            },
            ASTExpr::Try(expr) => Expr::ETry(Box::new((*expr).into())),
        }
    }
}
