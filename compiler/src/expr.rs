use frameql_ast::{Expr, Function, Identifier, Relation, RuleDecl, Term};

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
    CtxRuleLAtom {
        ctx_rule: RuleDecl,
        ctx_head_idx: usize,
    },

    /// Location component of a rule head: `Rel1() @X :- ...`
    CtxRuleLLocation {
        ctx_rule: RuleDecl,
        ctx_head_idx: usize,
    },

    /// Argument to a right-hand-side atom
    CtxRuleRAtom {
        ctx_rule: RuleDecl,
        ctx_atom_idx: usize,
    },

    /// Filter or assignment expression in the RHS of a rule
    CtxRuleRCond { ctx_rule: RuleDecl, ctx_idx: usize },

    /// Right-hand side of a FlatMap clause in RHS of a rule
    CtxRuleRFlatMap { ctx_rule: RuleDecl, ctx_idx: usize },

    /// Left-hand side of a FlatMap clause in RHS of a rule
    CtxRuleRFlatMapVars { ctx_rule: RuleDecl, ctx_idx: usize },

    /// Inspect clause in RHS of a rule
    CtxRuleRInspect { ctx_rule: RuleDecl, ctx_idx: usize },

    /// Projection expression in a group_by clause in RHS of a rule
    CtxRuleRProject { ctx_rule: RuleDecl, ctx_idx: usize },

    /// Group-by expression in RHS of a rule
    CtxRuleRGroupBy { ctx_rule: RuleDecl, ctx_idx: usize },

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

/// Depth-first fold of an expression with *syntactic context*.
/// Mirrors the Haskell type:
///   exprFoldCtxM :: (ECtx -> ExprNode b -> m b) -> ECtx -> Expr -> m b
///
/// - `f` is called *after* recursively folding children,
///   receiving the current `ctx` and a node whose children are already of type `B`.
fn expr_fold_ctx_m_node<F>(
    f: &mut F,
    ctx: &ECtx,
    n: &Expr, // ENode = ExprNode<Expr>
) -> Expr
where
    F: FnMut(&ECtx, Expr) -> Expr + Clone,
{
    match n {
        Expr::Term(Term::Int(val)) => f(ctx, Expr::Term(Term::Int(*val))),
        Expr::Term(Term::Bool(val)) => f(ctx, Expr::Term(Term::Bool(*val))),
        Expr::Term(Term::String(val)) => f(ctx, Expr::Term(Term::String(val.clone()))),
        Expr::Term(Term::Float(val)) => f(ctx, Expr::Term(Term::Float(*val))),
        Expr::Term(Term::Vec(val)) => f(ctx, Expr::Term(Term::Vec(val.clone()))),
        Expr::Term(Term::Map(val)) => f(ctx, Expr::Term(Term::Map(val.clone()))),
        Expr::Term(Term::Var(ident)) => f(ctx, Expr::Term(Term::Var(ident.clone()))),
        Expr::Term(Term::Tuple(ident)) => todo!(),
        Expr::Term(Term::Match { scrutinee, clauses }) => {
            let parent = n.clone();
            let m_ctx = ECtx::CtxMatchExpr {
                ctx_par_expr: parent.clone(),
                ctx_par: Box::new(ctx.clone()),
            };
            let m_b = expr_fold_ctx_m_node(f, &m_ctx, scrutinee);

            let mut cs_b = Vec::with_capacity(clauses.len());
            for (i, (pattern, e2)) in clauses.iter().enumerate() {
                let pat_ctx = ECtx::CtxMatchPat {
                    ctx_par_expr: parent.clone(),
                    ctx_par: Box::new(ctx.clone()),
                    ctx_idx: i,
                };
                let val_ctx = ECtx::CtxMatchVal {
                    ctx_par_expr: parent.clone(),
                    ctx_par: Box::new(ctx.clone()),
                    ctx_idx: i,
                };

                let e1b = expr_fold_ctx_m_node(f, &pat_ctx, &pattern);
                let e2b = expr_fold_ctx_m_node(f, &val_ctx, e2);
                cs_b.push((e1b, e2b));
            }

            f(
                ctx,
                Expr::Term(Term::Match {
                    scrutinee: Box::new(m_b),
                    clauses: cs_b,
                }),
            )
        }
        Expr::Term(Term::IfThenElse {
            cond,
            then_term,
            else_term,
        }) => todo!(),
        Expr::Term(Term::IfThenElse {
            cond,
            then_term,
            else_term,
        }) => todo!(),
        Expr::Term(Term::For {
            pattern,
            iter,
            body,
        }) => {
            todo!()
        }
        Expr::Term(Term::Continue) => {
            todo!()
        }
        Expr::Term(Term::Break) => {
            todo!()
        }
        Expr::Term(Term::Return(ret)) => {
            todo!()
        }
        Expr::Term(Term::VarDecl(ide)) => {
            todo!()
        }
        Expr::Term(Term::Lambda {
            params,
            return_ty,
            body,
        }) => {
            todo!()
        }
        Expr::Term(Term::Wildcard) => {
            todo!()
        }
        Expr::Term(Term::ConsTerm {
            name,
            args,
            named_args,
        }) => {
            todo!()
        }
        Expr::Unary { op, rhs } => todo!(),
        Expr::Binary { lhs, op, rhs } => todo!(),
        Expr::Slice { parent, index } => todo!(),
        Expr::TypeAnnotation { parent, ty } => todo!(),
        Expr::FCall { func, args } => todo!(),
        Expr::DotFCall { parent, name, args } => todo!(),
        Expr::Field { base, field } => todo!(),
        Expr::TupleIndex { base, index } => todo!(),
        Expr::Cast { base, ty } => todo!(),
        Expr::Try(expr) => todo!(),
    }
    //     Term(Term::Var(ident)) => f(ctx, Expr::Term(Term::Var(ident.clone()))),

    //     EApply {
    //         expr_pos,
    //         expr_func,
    //         expr_args,
    //     } => {
    //         // Parent node for context
    //         let parent = n.clone();
    //         let func_ctx = ECtx::CtxApplyFunc {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let func_b = expr_fold_ctx_m(f, &func_ctx, expr_func)?;

    //         let mut args_b = Vec::with_capacity(expr_args.len());
    //         for (i, a) in expr_args.iter().enumerate() {
    //             let arg_ctx = ECtx::CtxApplyArg {
    //                 ctx_par_expr: parent.clone(),
    //                 ctx_par: Box::new(ctx.clone()),
    //                 ctx_idx: i,
    //             };
    //             args_b.push(expr_fold_ctx_m(f, &arg_ctx, a)?);
    //         }

    //         f(
    //             ctx,
    //             EApply {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_func: func_b,
    //                 expr_args: args_b,
    //             },
    //         )
    //     }

    //     EField {
    //         expr_pos,
    //         expr_struct,
    //         expr_field,
    //     } => {
    //         let parent = n.clone();
    //         let s_ctx = ECtx::CtxField {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let s_b = expr_fold_ctx_m(f, &s_ctx, expr_struct)?;
    //         f(
    //             ctx,
    //             EField {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_struct: s_b,
    //                 expr_field: expr_field.clone(),
    //             },
    //         )
    //     }

    //     ETupField {
    //         expr_pos,
    //         expr_tuple,
    //         expr_tup_field,
    //     } => {
    //         let parent = n.clone();
    //         let s_ctx = ECtx::CtxTupField {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let s_b = expr_fold_ctx_m(f, &s_ctx, expr_tuple)?;
    //         f(
    //             ctx,
    //             ETupField {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_tuple: s_b,
    //                 expr_tup_field: *expr_tup_field,
    //             },
    //         )
    //     }

    //     EBool {
    //         expr_pos,
    //         expr_bval,
    //     } => f(
    //         ctx,
    //         EBool {
    //             expr_pos: expr_pos.clone(),
    //             expr_bval: *expr_bval,
    //         },
    //     ),

    //     EInt {
    //         expr_pos,
    //         expr_ival,
    //     } => f(
    //         ctx,
    //         EInt {
    //             expr_pos: expr_pos.clone(),
    //             expr_ival: *expr_ival,
    //         },
    //     ),

    //     EDouble {
    //         expr_pos,
    //         expr_dval,
    //     } => f(
    //         ctx,
    //         EDouble {
    //             expr_pos: expr_pos.clone(),
    //             expr_dval: *expr_dval,
    //         },
    //     ),

    //     EFloat {
    //         expr_pos,
    //         expr_fval,
    //     } => f(
    //         ctx,
    //         EFloat {
    //             expr_pos: expr_pos.clone(),
    //             expr_fval: *expr_fval,
    //         },
    //     ),

    //     EString {
    //         expr_pos,
    //         expr_string,
    //     } => f(
    //         ctx,
    //         EString {
    //             expr_pos: expr_pos.clone(),
    //             expr_string: expr_string.clone(),
    //         },
    //     ),

    //     EBit {
    //         expr_pos,
    //         expr_width,
    //         expr_ival,
    //     } => f(
    //         ctx,
    //         EBit {
    //             expr_pos: expr_pos.clone(),
    //             expr_width: *expr_width,
    //             expr_ival: *expr_ival,
    //         },
    //     ),

    //     ESigned {
    //         expr_pos,
    //         expr_width,
    //         expr_ival,
    //     } => f(
    //         ctx,
    //         ESigned {
    //             expr_pos: expr_pos.clone(),
    //             expr_width: *expr_width,
    //             expr_ival: *expr_ival,
    //         },
    //     ),

    //     EStruct {
    //         expr_pos,
    //         expr_constructor,
    //         expr_struct_fields,
    //     } => {
    //         let parent = n.clone();
    //         let mut fs_b = Vec::with_capacity(expr_struct_fields.len());
    //         for (i, (fname, fl)) in expr_struct_fields.iter().enumerate() {
    //             let field_ctx = ECtx::CtxStruct {
    //                 ctx_par_expr: parent.clone(),
    //                 ctx_par: Box::new(ctx.clone()),
    //                 ctx_arg: (i, fname.clone()),
    //             };
    //             let fl_b = expr_fold_ctx_m(f, &field_ctx, fl)?;
    //             fs_b.push((fname.clone(), fl_b));
    //         }
    //         f(
    //             ctx,
    //             EStruct {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_constructor: expr_constructor.clone(),
    //                 expr_struct_fields: fs_b,
    //             },
    //         )
    //     }

    //     ETuple {
    //         expr_pos,
    //         expr_tuple_fields,
    //     } => {
    //         let parent = n.clone();
    //         let mut fs_b = Vec::with_capacity(expr_tuple_fields.len());
    //         for (i, fl) in expr_tuple_fields.iter().enumerate() {
    //             let tup_ctx = ECtx::CtxTuple {
    //                 ctx_par_expr: parent.clone(),
    //                 ctx_par: Box::new(ctx.clone()),
    //                 ctx_idx: i,
    //             };
    //             fs_b.push(expr_fold_ctx_m(f, &tup_ctx, fl)?);
    //         }
    //         f(
    //             ctx,
    //             ETuple {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_tuple_fields: fs_b,
    //             },
    //         )
    //     }

    //     ESlice {
    //         expr_pos,
    //         expr_op,
    //         expr_h,
    //         expr_l,
    //     } => {
    //         let parent = n.clone();
    //         let v_ctx = ECtx::CtxSlice {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let v_b = expr_fold_ctx_m(f, &v_ctx, expr_op)?;
    //         f(
    //             ctx,
    //             ESlice {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_op: v_b,
    //                 expr_h: *expr_h,
    //                 expr_l: *expr_l,
    //             },
    //         )
    //     }

    //     EMatch {
    //         expr_pos,
    //         expr_match_expr,
    //         expr_cases,
    //     } => {
    //         let parent = n.clone();
    //         let m_ctx = ECtx::CtxMatchExpr {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let m_b = expr_fold_ctx_m(f, &m_ctx, expr_match_expr)?;

    //         let mut cs_b = Vec::with_capacity(expr_cases.len());
    //         for (i, (e1, e2)) in expr_cases.iter().enumerate() {
    //             let pat_ctx = ECtx::CtxMatchPat {
    //                 ctx_par_expr: parent.clone(),
    //                 ctx_par: Box::new(ctx.clone()),
    //                 ctx_idx: i,
    //             };
    //             let val_ctx = ECtx::CtxMatchVal {
    //                 ctx_par_expr: parent.clone(),
    //                 ctx_par: Box::new(ctx.clone()),
    //                 ctx_idx: i,
    //             };
    //             let e1b = expr_fold_ctx_m(f, &pat_ctx, e1)?;
    //             let e2b = expr_fold_ctx_m(f, &val_ctx, e2)?;
    //             cs_b.push((e1b, e2b));
    //         }

    //         f(
    //             ctx,
    //             EMatch {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_match_expr: m_b,
    //                 expr_cases: cs_b,
    //             },
    //         )
    //     }

    //     EVarDecl {
    //         expr_pos,
    //         expr_vname,
    //     } => f(
    //         ctx,
    //         EVarDecl {
    //             expr_pos: expr_pos.clone(),
    //             expr_vname: expr_vname.clone(),
    //         },
    //     ),

    //     ESeq {
    //         expr_pos,
    //         expr_left,
    //         expr_right,
    //     } => {
    //         let parent = n.clone();
    //         let l_ctx = ECtx::CtxSeq1 {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let r_ctx = ECtx::CtxSeq2 {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let l_b = expr_fold_ctx_m(f, &l_ctx, expr_left)?;
    //         let r_b = expr_fold_ctx_m(f, &r_ctx, expr_right)?;
    //         f(
    //             ctx,
    //             ESeq {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_left: l_b,
    //                 expr_right: r_b,
    //             },
    //         )
    //     }

    //     EITE {
    //         expr_pos,
    //         expr_cond,
    //         expr_then,
    //         expr_else,
    //     } => {
    //         let parent = n.clone();
    //         let if_ctx = ECtx::CtxITEIf {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let then_ctx = ECtx::CtxITEThen {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let else_ctx = ECtx::CtxITEElse {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let c_b = expr_fold_ctx_m(f, &if_ctx, expr_cond)?;
    //         let t_b = expr_fold_ctx_m(f, &then_ctx, expr_then)?;
    //         let e_b = expr_fold_ctx_m(f, &else_ctx, expr_else)?;
    //         f(
    //             ctx,
    //             EITE {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_cond: c_b,
    //                 expr_then: t_b,
    //                 expr_else: e_b,
    //             },
    //         )
    //     }

    //     EFor {
    //         expr_pos,
    //         expr_loop_vars,
    //         expr_iter,
    //         expr_body,
    //     } => {
    //         let parent = n.clone();
    //         let vars_ctx = ECtx::CtxForVars {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let iter_ctx = ECtx::CtxForIter {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let body_ctx = ECtx::CtxForBody {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let v_b = expr_fold_ctx_m(f, &vars_ctx, expr_loop_vars)?;
    //         let i_b = expr_fold_ctx_m(f, &iter_ctx, expr_iter)?;
    //         let b_b = expr_fold_ctx_m(f, &body_ctx, expr_body)?;
    //         f(
    //             ctx,
    //             EFor {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_loop_vars: v_b,
    //                 expr_iter: i_b,
    //                 expr_body: b_b,
    //             },
    //         )
    //     }

    //     ESet {
    //         expr_pos,
    //         expr_lval,
    //         expr_rval,
    //     } => {
    //         // Haskell note: fold RHS first (helps with typing checks)
    //         let parent = n.clone();
    //         let r_ctx = ECtx::CtxSetR {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let l_ctx = ECtx::CtxSetL {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let r_b = expr_fold_ctx_m(f, &r_ctx, expr_rval)?;
    //         let l_b = expr_fold_ctx_m(f, &l_ctx, expr_lval)?;
    //         f(
    //             ctx,
    //             ESet {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_lval: l_b,
    //                 expr_rval: r_b,
    //             },
    //         )
    //     }

    //     EContinue { expr_pos } => f(
    //         ctx,
    //         EContinue {
    //             expr_pos: expr_pos.clone(),
    //         },
    //     ),
    //     EBreak { expr_pos } => f(
    //         ctx,
    //         EBreak {
    //             expr_pos: expr_pos.clone(),
    //         },
    //     ),

    //     EReturn {
    //         expr_pos,
    //         expr_ret_val,
    //     } => {
    //         let parent = n.clone();
    //         let ret_ctx = ECtx::CtxReturn {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let v_b = expr_fold_ctx_m(f, &ret_ctx, expr_ret_val)?;
    //         f(
    //             ctx,
    //             EReturn {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_ret_val: v_b,
    //             },
    //         )
    //     }

    //     EBinOp {
    //         expr_pos,
    //         expr_bop,
    //         expr_left,
    //         expr_right,
    //     } => {
    //         let parent = n.clone();
    //         let l_ctx = ECtx::CtxBinOpL {
    //             ctx_par_expr: parent.clone(),
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let r_ctx = ECtx::CtxBinOpR {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let l_b = expr_fold_ctx_m(f, &l_ctx, expr_left)?;
    //         let r_b = expr_fold_ctx_m(f, &r_ctx, expr_right)?;
    //         f(
    //             ctx,
    //             EBinOp {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_bop: expr_bop.clone(),
    //                 expr_left: l_b,
    //                 expr_right: r_b,
    //             },
    //         )
    //     }

    //     EUnOp {
    //         expr_pos,
    //         expr_uop,
    //         expr_op,
    //     } => {
    //         let parent = n.clone();
    //         let x_ctx = ECtx::CtxUnOp {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let x_b = expr_fold_ctx_m(f, &x_ctx, expr_op)?;
    //         f(
    //             ctx,
    //             EUnOp {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_uop: expr_uop.clone(),
    //                 expr_op: x_b,
    //             },
    //         )
    //     }

    //     EPHolder { expr_pos } => f(
    //         ctx,
    //         EPHolder {
    //             expr_pos: expr_pos.clone(),
    //         },
    //     ),

    //     EBinding {
    //         expr_pos,
    //         expr_var,
    //         expr_pattern,
    //     } => {
    //         let parent = n.clone();
    //         let b_ctx = ECtx::CtxBinding {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let x_b = expr_fold_ctx_m(f, &b_ctx, expr_pattern)?;
    //         f(
    //             ctx,
    //             EBinding {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_var: expr_var.clone(),
    //                 expr_pattern: x_b,
    //             },
    //         )
    //     }

    //     ETyped {
    //         expr_pos,
    //         expr_expr,
    //         expr_tspec,
    //     } => {
    //         let parent = n.clone();
    //         let t_ctx = ECtx::CtxTyped {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let x_b = expr_fold_ctx_m(f, &t_ctx, expr_expr)?;
    //         f(
    //             ctx,
    //             ETyped {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_expr: x_b,
    //                 expr_tspec: expr_tspec.clone(),
    //             },
    //         )
    //     }

    //     EAs {
    //         expr_pos,
    //         expr_expr,
    //         expr_tspec,
    //     } => {
    //         let parent = n.clone();
    //         let a_ctx = ECtx::CtxAs {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let x_b = expr_fold_ctx_m(f, &a_ctx, expr_expr)?;
    //         f(
    //             ctx,
    //             EAs {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_expr: x_b,
    //                 expr_tspec: expr_tspec.clone(),
    //             },
    //         )
    //     }

    //     ERef {
    //         expr_pos,
    //         expr_pattern,
    //     } => {
    //         let parent = n.clone();
    //         let r_ctx = ECtx::CtxRef {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let x_b = expr_fold_ctx_m(f, &r_ctx, expr_pattern)?;
    //         f(
    //             ctx,
    //             ERef {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_pattern: x_b,
    //             },
    //         )
    //     }

    //     ETry {
    //         expr_pos,
    //         expr_expr,
    //     } => {
    //         let parent = n.clone();
    //         let t_ctx = ECtx::CtxTry {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let x_b = expr_fold_ctx_m(f, &t_ctx, expr_expr)?;
    //         f(
    //             ctx,
    //             ETry {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_expr: x_b,
    //             },
    //         )
    //     }

    //     EClosure {
    //         expr_pos,
    //         expr_closure_args,
    //         expr_closure_type,
    //         expr_expr,
    //     } => {
    //         let parent = n.clone();
    //         let c_ctx = ECtx::CtxClosure {
    //             ctx_par_expr: parent,
    //             ctx_par: Box::new(ctx.clone()),
    //         };
    //         let x_b = expr_fold_ctx_m(f, &c_ctx, expr_expr)?;
    //         f(
    //             ctx,
    //             EClosure {
    //                 expr_pos: expr_pos.clone(),
    //                 expr_closure_args: expr_closure_args.clone(),
    //                 expr_closure_type: expr_closure_type.clone(),
    //                 expr_expr: x_b,
    //             },
    //         )
    //     }

    //     EFunc {
    //         expr_pos,
    //         expr_func_name,
    //     } => f(
    //         ctx,
    //         EFunc {
    //             expr_pos: expr_pos.clone(),
    //             expr_func_name: expr_func_name.clone(),
    //         },
    //     ),
    // }
}
