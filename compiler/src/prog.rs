use std::collections::{HashMap, HashSet};

use frameql_ast::{Identifier, Import, TypeVarName};

use crate::{
    Statics,
    expr::{BOp, ECtx, Expr, ExprVisitor, UOp, expr_collect_ctx, expr_fold, expr_fold_ctx},
    func::{FnDef, Function},
    relation::{Relation, rel_expr_map},
    rule::{Rule, RuleRHS, rule_expr_map},
    r#type::{Constructor, Field, Type, TypeDef, struct_fields},
    var::{Var, arg2v},
};

#[derive(Debug, Clone)]
pub struct FrameQLProgram {
    pub imports: Vec<Import>,
    pub typedefs: HashMap<String, TypeDef>,
    // There can be multiple functions with the same name.
    // The key in the map is the function name; value is a Vec of functions
    pub functions: HashMap<String, Vec<Function>>,
    pub relations: HashMap<String, Relation>,
    pub rules: Vec<Rule>,
    // maps module to source code
    pub sources: HashMap<String, String>,
}

impl FrameQLProgram {
    /// Variables visible in the `i`-th conjunct in the right-hand side of a rule.
    /// All conjuncts before `i` must be validated before calling this function.
    pub fn rule_rhs_vars(&self, rl: &Rule, i: usize) -> Vec<Var> {
        if i == 0 {
            Vec::new()
        } else {
            self.rule_rhs_vars_after(rl, i - 1)
        }
    }

    /// Variables visible *after* the `i`-th conjunct.
    pub fn rule_rhs_vars_after(&self, rl: &Rule, i: usize) -> Vec<Var> {
        if i < 0 {
            return Vec::new();
        }

        // assume you have `rule_rhs(&self) -> &Vec<Rule>` or similar
        let rhs_item = &rl.body[i as usize];

        let mut vs = self.rule_rhs_vars_after(rl, i - 1);

        match rhs_item {
            RuleRHS::Literal(atom) => {
                let mut new_vars = self.expr_var_decls(
                    &ECtx::RuleRAtom {
                        rule: rl.clone(),
                        idx: i as usize,
                    },
                    &atom.val,
                );
                vs.append(&mut new_vars);
            }
            RuleRHS::Cond(expr) => {
                if let Expr::ESet { lval, .. } = expr {
                    let mut new_vars = self.expr_var_decls(
                        &ECtx::SetL {
                            par_expr: expr.clone(),
                            par: Box::new(ECtx::RuleRCond {
                                rule: rl.clone(),
                                idx: i as usize,
                            }),
                        },
                        lval,
                    );
                    vs.append(&mut new_vars);
                }
                // plain condition introduces no vars
            }
            RuleRHS::FlatMap { vars, .. } => {
                let mut new_vars = self.expr_var_decls(
                    &ECtx::RuleRFlatMapVars {
                        rule: rl.clone(),
                        idx: i as usize,
                    },
                    vars,
                );
                vs.append(&mut new_vars);
            }
            RuleRHS::GroupBy { group_by, .. } => {
                let ctx = ECtx::RuleRGroupBy {
                    rule: rl.clone(),
                    idx: i as usize,
                };
                let mut gvars = self.expr_vars(&ctx, group_by);
                let avar = Var::GroupVar {
                    var_rule: rl.clone(),
                    var_rhs_idx: i as usize,
                };

                vs.clear(); // group_by hides all previous variables
                vs.push(avar);
                vs.append(&mut gvars);
                vs.sort();
                vs.dedup();
            }
        }
        vs
    }

    pub fn rule_vars(&self, rl: &Rule) -> Vec<Var> {
        let rhs_len = rl.body.len();
        self.rule_rhs_vars(rl, rhs_len)
    }

    /// Enumerate all variables that occur in the expression.
    pub fn expr_vars(&self, ctx: &ECtx, e: &Expr) -> Vec<Var> {
        let mut vars = expr_collect_ctx(
            |ctx_prime: &ECtx, e_prime: &Expr| -> Vec<Var> {
                match e_prime {
                    Expr::EVar(v) => {
                        if let Some(var) = self.lookup_var(ctx_prime, v) {
                            vec![var]
                        } else {
                            vec![Var::ExprVar {
                                var_ctx: ctx_prime.clone(),
                                var_expr: Expr::EVar(v.clone()),
                            }]
                        }
                    }
                    _ => Vec::new(),
                }
            },
            |mut acc: Vec<Var>, mut xs: Vec<Var>| {
                acc.append(&mut xs);
                acc
            },
            ctx,
            e,
        );

        // remove duplicates like Haskell's `nub`
        vars.sort();
        vars.dedup();
        vars
    }

    pub fn expr_type(&self, ctx: &ECtx, e: &Expr) -> Type {
        use Expr::*;

        match e {
            // Variables: look up in visible vars or implicit declaration inside rule RHS
            Expr::EVar(name) => {
                let vs = self.ctx_all_vars(ctx);
                if let Some(var) = vs.into_iter().find(|v| v.name() == *name) {
                    self.var_type(&var)
                } else if ctx.in_rule_rhs() {
                    // implicit var declaration
                    let var = Var::ExprVar {
                        var_ctx: ctx.clone(),
                        var_expr: e.clone(),
                    };
                    self.var_type(&var)
                } else {
                    panic!("expr_node_type: unknown variable {}", name,);
                }
            }

            // Application: result type is the return type of the function's type
            EApply { func, args: _ } => {
                let fn_type: Type = self.typ_prime(ctx, func);
                match fn_type {
                    Type::TFunction { ret_type, .. } => *ret_type,
                    _ => panic!("Unexpected type"),
                }
            }

            EFunc { name } => {
                if name.len() == 1 {
                    let f = &name[0];
                    if self.func_is_polymorphic(f) {
                        ctx.expect_type()
                    } else {
                        let func: Function = self.lookup_fn(f).expect("Function should exist");
                        Type::TFunction {
                            args: func.args().iter().map(|a| a.ty.clone().into()).collect(),
                            ret_type: Box::new(func.ret_type().into()),
                        }
                    }
                } else {
                    panic!(
                        "expr_node_type_prime called with unresolved function name: {:?}",
                        e
                    )
                }
            }

            // Field access: dereference struct type and find field type
            EField {
                struct_: sub_expr,
                field,
            } => {
                let t = self.typ_deref_prime(ctx, sub_expr);
                // assume structFields(t) -> Vec<Field> with name and type
                let Type::TStruct(_) = t.clone() else {
                    panic!("Expected struct type, got {:?}", t);
                };
                let fields = struct_fields(&t);
                let fld = fields
                    .into_iter()
                    .find(|f| f.name == *field)
                    .expect("field not found in struct type");
                fld.ty.clone().into()
            }

            ETupField { tuple, field } => {
                let t = self.typ_deref_prime(ctx, tuple);
                match t {
                    Type::TTuple(ref args) => args[*field as usize].clone(),
                    _ => panic!("ETupField on non-tuple type"),
                }
            }

            // literals
            EBool { .. } => Type::TBool,
            EInt { .. } => Type::TInt,
            EString { .. } => Type::TString,
            EFloat { .. } => Type::TFloat,

            // Struct: type is taken from context expected type (Haskell: ctxExpectType ctx)
            EStruct { .. } => ctx.expect_type(),

            ETuple(elems) => {
                // Haskell: tTuple fs  — build a tuple type from each element's type
                let types: Vec<Type> = elems.iter().map(|e| self.typ_prime(ctx, e)).collect();
                Type::TTuple(types)
            }

            ESlice { .. } => {
                todo!()
            }

            EMatch { clause: _, body } => {
                // Haskell: snd $ head cs
                // cases: Vec<(Expr, Type)> in Haskell; in Rust assume Vec<(Expr, Type)>
                if body.is_empty() {
                    panic!("EMatch has no cases");
                }
                let ty = self.expr_type(&ctx, &body[0].0);
                ty.clone()
            }

            EVarDecl { .. } => ctx.expect_type(),

            ESeq { left: _, right } => {
                // Haskell: expression type = e2
                self.typ_prime(ctx, right)
            }

            EITE { cond: _, then, .. } => self.typ_prime(ctx, then),

            EFor { .. } => Type::TTuple(vec![]),

            ESet { .. } => Type::TTuple(vec![]),

            EContinue { .. } => ctx.expect_type(),
            EBreak { .. } => ctx.expect_type(),
            EReturn(..) => ctx.expect_type(),

            EBinOp { op, left, .. } => match op {
                BOp::Eq
                | BOp::Neq
                | BOp::Lt
                | BOp::Gt
                | BOp::Lte
                | BOp::Gte
                | BOp::And
                | BOp::Or
                | BOp::Impl
                | BOp::Plus
                | BOp::Minus
                | BOp::Mod
                | BOp::Times
                | BOp::Div
                | BOp::ShiftR
                | BOp::ShiftL
                | BOp::BAnd
                | BOp::BOr
                | BOp::BXor => self.typ_prime(ctx, left),
                BOp::Concat => Type::TString,
            },

            EUnOp { op: UOp::Not, .. } => Type::TBool,
            EUnOp {
                op: UOp::BNeg,
                expr,
            } => self.typ_prime(ctx, expr),
            EUnOp {
                op: UOp::UMinus,
                expr,
            } => self.typ_prime(ctx, expr),

            EBinding { pattern, .. } => self.typ_prime(ctx, pattern),

            ETyped { spec, .. } => spec.clone(),

            EAs { spec, .. } => spec.clone(),

            ERef(..) => ctx.expect_type(),

            ETry { .. } => {
                panic!("expr_node_type: ?-expressions should be eliminated during type inference.")
            }

            EClosure { args, ret_type, .. } => {
                // Haskell: tFunction (map (fromJust . ceargType) args) (fromJust r)
                let arg_types: Vec<Type> = args.iter().map(|a| a.ty.clone().into()).collect();
                let r = ret_type.clone().expect("closure return type missing");
                Type::TFunction {
                    args: arg_types,
                    ret_type: Box::new(r),
                }
            }

            // add other cases / variants as required
            _ => panic!("expr_node_type: unhandled expression variant"),
        }
    }

    pub fn cons_type(&self, c: &str) -> &TypeDef {
        self.typedefs
            .values()
            .find(|td| match &td.type_ {
                Some(Type::TStruct(args)) => args.iter().any(|ctor| ctor.name.as_str() == c),
                _ => false,
            })
            .expect("Constructor not found in any typedef")
    }

    /// True iff `c` is the unique constructor of its type.
    pub fn cons_is_unique(&self, c: &str) -> bool {
        let td = self.cons_type(c);
        match &td.type_ {
            Some(Type::TStruct(args)) => args.len() == 1,
            _ => false,
        }
    }

    pub(crate) fn expr_is_polymorphic(&self, _ctx: &ECtx, _expr: &Expr) -> bool {
        false
    }

    pub(crate) fn expr_is_pure(&self, ctx: &ECtx, expr: &Expr) -> bool {
        true
    }

    /// Variables declared inside an expression, visible in the code that follows the expression.
    pub fn expr_var_decls(&self, ctx: &ECtx, e: &Expr) -> Vec<Var> {
        struct Visitor<'a> {
            prog: &'a FrameQLProgram,
            ctx: &'a ECtx,
        }
        impl<'a> ExprVisitor<Vec<Var>> for Visitor<'a> {
            fn visit_expr(&mut self, e: &Expr) -> Vec<Var> {
                match e {
                    Expr::EStruct { name, fields } => self.visit_struc(name, fields),
                    Expr::EVar(ident) => self.visit_var(ident),
                    Expr::ETuple(tuple) => self.visit_tuple(tuple),
                    Expr::EVarDecl(ident) => self.visit_var_decl(ident),
                    Expr::ESet { lval, rval } => self.visit_set(lval, rval),
                    Expr::EBinding { var, pattern } => self.visit_binding(var, pattern),
                    Expr::ETyped { expr, spec } => self.visit_typed(expr, spec),
                    Expr::ERef(expr) => self.visit_ref(expr),
                    _ => vec![],
                }
            }
            fn visit_struc(
                &mut self,
                _name: &Identifier,
                fields: &Vec<(Identifier, Expr)>,
            ) -> Vec<Var> {
                fields
                    .iter()
                    .flat_map(|(_ident, expr)| self.visit_expr(expr))
                    .collect()
            }

            fn visit_tuple(&mut self, tuple: &Vec<Expr>) -> Vec<Var> {
                tuple
                    .iter()
                    .flat_map(|expr| self.visit_expr(expr))
                    .collect()
            }

            fn visit_set(&mut self, lval: &Expr, _rval: &Expr) -> Vec<Var> {
                // Only collect declared vars from the left-hand side
                self.visit_expr(lval)
            }

            fn visit_binding(&mut self, var: &Identifier, pattern: &Expr) -> Vec<Var> {
                // Current expr (binding) introduces a BindingVar, plus whatever is declared in the pattern
                let mut vars = vec![Var::BindingVar {
                    var_ctx: self.ctx.clone(),
                    var_expr: Expr::EBinding {
                        var: var.clone(),
                        pattern: Box::new(pattern.clone()),
                    },
                }];
                vars.extend(self.visit_expr(pattern));
                vars
            }

            fn visit_typed(&mut self, expr: &Expr, _spec: &Type) -> Vec<Var> {
                // Just recurse into the expression
                self.visit_expr(expr)
            }

            fn visit_ref(&mut self, expr: &Expr) -> Vec<Var> {
                // Just recurse into the referenced expression
                self.visit_expr(expr)
            }

            fn visit_var_decl(&mut self, ident: &Identifier) -> Vec<Var> {
                vec![Var::ExprVar {
                    var_ctx: self.ctx.clone(),
                    var_expr: Expr::EVarDecl(ident.clone()),
                }]
            }

            fn visit_var(&mut self, ident: &Identifier) -> Vec<Var> {
                match self.prog.lookup_var(self.ctx, ident) {
                    None => vec![Var::ExprVar {
                        var_ctx: self.ctx.clone(),
                        var_expr: Expr::EVar(ident.clone()),
                    }],
                    Some(var) => vec![var],
                }
            }
        }

        let mut visitor = Visitor {
            ctx: ctx,
            prog: self,
        };
        visitor.visit_expr(e)
    }

    pub(crate) fn lookup_var(&self, ctx: &ECtx, n: &Identifier) -> Option<Var> {
        self.ctx_all_vars(ctx).into_iter().find(|v| v.name() == *n)
    }
    fn ctx_vars(&self, ctx: &ECtx) -> (Vec<Var>, Vec<Var>) {
        self.ctx_vars_with_flag(ctx, true)
    }

    pub(crate) fn ctx_all_vars(&self, ctx: &ECtx) -> Vec<Var> {
        let (lvs, rvs) = self.ctx_vars(ctx);
        let mut all_vars = lvs;
        all_vars.extend(rvs);
        all_vars
    }

    pub(crate) fn ctx_vars_with_flag(&self, ctx: &ECtx, with_types: bool) -> (Vec<Var>, Vec<Var>) {
        let parent = ctx.parent();
        let (plvars, prvars) = if ECtx::Top != parent {
            self.ctx_vars_with_flag(&parent, with_types)
        } else {
            (vec![], vec![])
        };

        match ctx {
            ECtx::Top => (vec![], vec![]),
            ECtx::Func(f) => {
                let mut_vars = f.mutable_args().into_iter().map(|a| arg2v(f, &a)).collect();
                let immut_vars = f
                    .immutable_args()
                    .into_iter()
                    .map(|a| arg2v(f, &a))
                    .collect();
                (mut_vars, immut_vars)
            }
            ECtx::RuleLAtom { rule, .. } | ECtx::RuleLLocation { rule, .. } => {
                (vec![], self.rule_vars(rule))
            }
            ECtx::RuleRAtom { rule, idx }
            | ECtx::RuleRCond { rule, idx }
            | ECtx::RuleRFlatMap { rule, idx }
            | ECtx::RuleRFlatMapVars { rule, idx }
            | ECtx::RuleRProject { rule, idx }
            | ECtx::RuleRGroupBy { rule, idx } => (vec![], self.rule_rhs_vars(rule, *idx)),
            ECtx::Key { relation: rel } => (
                vec![],
                vec![Var::KeyVar {
                    var_rel: rel.clone(),
                }],
            ),
            ECtx::Closure {
                par_expr: e,
                par: pctx,
            } => {
                // Closure arguments
                if with_types {
                    let closure_args = e.expr_closure_args();
                    let t_func = self.expr_type(pctx, e);
                    let mut closure_mut_vars = vec![];
                    let mut closure_imm_vars = vec![];
                    for (i, arg) in closure_args.iter().enumerate() {
                        let var = Var::ClosureArgVar {
                            var_ctx: *pctx.clone(),
                            var_expr: e.clone(),
                            var_arg_index: i,
                        };
                        if arg.is_mut {
                            closure_mut_vars.push(var);
                        } else {
                            closure_imm_vars.push(var);
                        }
                    }
                    (
                        closure_mut_vars,
                        closure_imm_vars
                            .into_iter()
                            .chain(plvars.into_iter())
                            .chain(prvars.into_iter())
                            .collect(),
                    )
                } else {
                    let closure_vars = (0..e.expr_closure_args().len())
                        .map(|i| Var::ClosureArgVar {
                            var_ctx: *pctx.clone(),
                            var_expr: e.clone(),
                            var_arg_index: i,
                        })
                        .collect::<Vec<_>>();
                    (
                        vec![],
                        closure_vars
                            .into_iter()
                            .chain(plvars.into_iter())
                            .chain(prvars.into_iter())
                            .collect(),
                    )
                }
            }
            // TODO: Add the remaining cases like CtxApplyArg, CtxSeq1, CtxForBody, etc.
            _ => (plvars, prvars),
        }
    }
    pub fn expr_free_vars(&self, ctx: &ECtx, e: &Expr) -> Vec<Var> {
        let visible_vars: HashSet<_> = self.ctx_all_vars(ctx).into_iter().collect();
        let used_vars: HashSet<_> = self.expr_vars(ctx, e).into_iter().collect();

        visible_vars.intersection(&used_vars).cloned().collect()
    }
    pub(crate) fn expr_is_static(&self, ctx: &ECtx, e: &Expr) -> bool {
        match e {
            Expr::EApply { .. } => {
                self.expr_free_vars(ctx, e).is_empty()
                    && !self.expr_is_polymorphic(ctx, e)
                    && self.expr_is_pure(ctx, e)
            }
            Expr::EString(_) => true,
            _ => false,
        }
    }
    fn check_static_expr(&self, ctx: &ECtx, node: &Expr, statics: &mut Statics) -> Expr {
        if self.expr_is_static(ctx, &node) {
            self.add_static(&node, ctx, statics);
        }
        node.clone()
    }

    pub fn add_static(&self, e: &Expr, ctx: &ECtx, statics: &mut Statics) {
        let key = (e.clone(), self.expr_type(ctx, e));
        let len = statics.len();

        statics.entry(key).or_insert_with(|| len as i32);
    }

    pub fn var_type(&self, var: &Var) -> Type {
        match var {
            // ExprVar inside a declaration or variable reference
            Var::ExprVar { var_ctx, var_expr } => match var_expr {
                Expr::EVarDecl(..) | Expr::EVar(..) => var_ctx.expect_type(),
                _ => panic!("var_type: unexpected ExprVar {:?}", var),
            },

            // Binding variable: @-binding
            Var::BindingVar { var_ctx, var_expr } => match var_expr {
                Expr::EBinding { pattern, .. } => self.expr_type(
                    &ECtx::Binding {
                        par_expr: var_expr.clone(),
                        par: Box::new(var_ctx.clone()),
                    },
                    pattern,
                ),
                _ => panic!("var_type: unexpected BindingVar {:?}", var),
            },

            // Function argument
            Var::ArgVar {
                var_func, var_name, ..
            } => {
                let arg: &frameql_ast::FnArg = var_func
                    .args()
                    .iter()
                    .find(|a| a.name == *var_name)
                    .expect("var_type: ArgVar not found in function args");
                arg.ty.clone().into()
            }

            // Closure argument
            Var::ClosureArgVar {
                var_ctx,
                var_expr,
                var_arg_index,
            } => {
                if let Type::TFunction { args, .. } = self.typ_prime(var_ctx, &var_expr) {
                    args.get(*var_arg_index)
                        .cloned()
                        .unwrap_or_else(|| panic!("var_type: invalid closure arg index {:?}", var))
                } else {
                    panic!("var_type: closure arg is not a function {:?}", var)
                }
            }

            // Primary key
            Var::KeyVar { var_rel } => var_rel.type_.clone(),

            // Group-by
            Var::GroupVar {
                var_rule,
                var_rhs_idx,
            } => {
                let ktype = self.rule_group_by_key_type(var_rule, *var_rhs_idx);
                let vtype = self.rule_group_by_val_type(var_rule, *var_rhs_idx);
                Type::TOpaque {
                    name: Identifier("frameql_std::Group".to_string()),
                    args: vec![ktype, vtype],
                }
            }
        }
    }

    pub fn typ_prime(&self, ctx: &ECtx, expr: &Expr) -> Type {
        let t = self.expr_type(ctx, expr);
        self._typ_prime(t)
    }

    fn _typ_prime(&self, t: Type) -> Type {
        match t {
            Type::TUser(name, args) => {
                let tdef = self.get_type(&name);

                match &tdef.type_ {
                    Some(ty) => {
                        let subst_map = tdef
                            .params
                            .iter()
                            .cloned()
                            .zip(args.into_iter())
                            .collect::<std::collections::HashMap<TypeVarName, Type>>();

                        let substituted = type_subst_type_args(&subst_map, &ty.clone().into());
                        self._typ_prime(substituted)
                    }
                    None => Type::TOpaque { name, args },
                }
            }
            other => other,
        }
    }
    fn typ_deref_prime(&self, ctx: &ECtx, e: &Expr) -> Type {
        self._typ_deref_prime(self.expr_type(ctx, e))
    }
    fn _typ_deref_prime(&self, t: Type) -> Type {
        match t {
            Type::TUser(ref n, ref as_) => {
                let tdef: TypeDef = self.get_type(n);
                match tdef.type_ {
                    Some(ty) => {
                        let subst: std::collections::HashMap<TypeVarName, Type> =
                            tdef.params.iter().cloned().zip(as_.clone()).collect();
                        let new_t = type_subst_type_args(&subst, &ty.into());
                        self._typ_deref_prime(new_t)
                    }
                    None => todo!(),
                }
            }

            Type::TOpaque { ref args, .. } if args.len() == 1 => t,

            _ => t,
        }
    }

    pub fn get_type(&self, name: &Identifier) -> TypeDef {
        self.lookup_type(name.as_str())
            .unwrap_or_else(|| panic!("get_type: unknown type {}", name))
    }

    pub fn lookup_type(&self, name: &str) -> Option<TypeDef> {
        self.typedefs.get(name).cloned()
    }

    fn func_is_polymorphic(&self, f: &Identifier) -> bool {
        todo!()
    }

    fn lookup_fn(&self, name: &Identifier) -> Option<Function> {
        let fns = self.functions.get(name.as_str())?;
        assert_eq!(fns.len(), 1);
        Some(fns[0].clone())
    }

    fn rule_group_by_key_type(&self, rule: &Rule, idx: usize) -> Type {
        let ctx = ECtx::RuleRGroupBy {
            rule: rule.clone(),
            idx,
        };
        let expr = match &rule.body[idx] {
            RuleRHS::GroupBy { group_by, .. } => group_by,
            _ => panic!("Unexpected Rule type"),
        };
        self.expr_type(&ctx, expr)
    }
    fn rule_group_by_val_type(&self, rule: &Rule, idx: usize) -> Type {
        let ctx = ECtx::RuleRProject {
            rule: rule.clone(),
            idx,
        };
        let expr = match &rule.body[idx] {
            RuleRHS::GroupBy { project, .. } => project,
            _ => panic!("Unexpected Rule type"),
        };
        self.expr_type(&ctx, expr)
    }

    pub fn prog_expr_map<F>(&self, f: F) -> Self
    where
        F: FnMut(&ECtx, Expr) -> Expr + Clone,
    {
        let rels = self
            .relations
            .iter()
            .map(|r| (r.0.clone(), rel_expr_map(r.1.clone(), f.clone())))
            .collect();

        let mut new_funcs: HashMap<String, Vec<Function>> = HashMap::new();

        for (name, funcs) in self.functions.iter() {
            let updated_funcs: Vec<Function> = funcs
                .into_iter()
                .map(|func| match func {
                    Function::FnDef(fn_def) => Function::FnDef(FnDef {
                        body: expr_fold_ctx(
                            &mut f.clone(),
                            &ECtx::Func(func.clone()),
                            &fn_def.body,
                        ),
                        ..fn_def.clone()
                    }),
                    x @ Function::ExternFn(_) => x.clone(),
                })
                .collect();
            new_funcs.insert(name.clone(), updated_funcs);
        }

        let rules = self
            .rules
            .iter()
            .map(|r| rule_expr_map(r.clone(), f.clone()))
            .collect();

        FrameQLProgram {
            relations: rels,
            functions: new_funcs,
            rules,
            ..self.clone()
        }
    }

    pub(crate) fn get_rules(&self, rel: &Relation) -> Vec<Rule> {
        todo!()
    }

    pub(crate) fn get_relation(&self, relation: &Identifier) -> Relation {
        self.relations.get(relation.as_str()).unwrap().clone()
    }

    pub(crate) fn rule_rhs_new_vars(&self, rule: &Rule, idx: usize) -> Vec<Var> {
        let mut vars_i = self.rule_rhs_vars_prime(rule, idx);
        let vars_prev = if idx == 0 {
            vec![]
        } else {
            self.rule_rhs_vars_prime(rule, idx - 1)
        };

        // set difference: vars_i \ vars_prev
        vars_i.retain(|v| !vars_prev.contains(v));
        vars_i
    }

    pub fn rule_rhs_vars_prime(&self, rule: &Rule, idx: usize) -> Vec<Var> {
        if idx == usize::MAX {
            return vec![];
        }

        let rhs = &rule.body;
        let this_rhs = &rhs[idx];

        let vs = self.rule_rhs_vars(rule, idx);

        match this_rhs {
            RuleRHS::Literal(atom) => {
                let mut vars = self.expr_var_decls(
                    &ECtx::RuleRAtom {
                        rule: rule.clone(),
                        idx,
                    },
                    &atom.val,
                );
                vars.extend(vs);
                vars
            }
            RuleRHS::Cond(e @ Expr::ESet { lval, .. }) => {
                let mut vars = self.expr_var_decls(
                    &ECtx::SetL {
                        par_expr: e.clone(),
                        par: Box::new(ECtx::RuleRCond {
                            rule: rule.clone(),
                            idx,
                        }),
                    },
                    &*lval,
                );
                vars.extend(vs);
                vars
            }
            RuleRHS::Cond { .. } => vs,
            RuleRHS::FlatMap { vars, .. } => {
                let mut vars = self.expr_var_decls(
                    &ECtx::RuleRFlatMapVars {
                        rule: rule.clone(),
                        idx,
                    },
                    vars,
                );
                vars.extend(vs);
                vars
            }
            RuleRHS::GroupBy { group_by, .. } => {
                let ctx = ECtx::RuleRGroupBy {
                    rule: rule.clone(),
                    idx,
                };
                let mut gvars = self.expr_vars(&ctx, &group_by);
                gvars.push(Var::GroupVar {
                    var_rule: rule.clone(),
                    var_rhs_idx: idx,
                });
                gvars.sort();
                gvars.dedup();
                gvars
            }
        }
    }

    pub(crate) fn rule_arrange_fst_literal(&self, rule: &Rule) -> Option<Expr> {
        if rule.body.is_empty() {
            return None;
        }

        // indices of consecutive conditions starting at position 1
        let mut conds = Vec::new();
        for i in 1..rule.body.len() {
            if let RuleRHS::Cond(_) = &rule.body[i] {
                conds.push(i);
            } else {
                break;
            }
        }

        // index of the next operator
        let rhs_idx = conds.len() + 1;
        if rhs_idx >= rule.body.len() {
            return None;
        }

        let rhs = &rule.body[rhs_idx];

        // expected arrangement of input by the operator
        let arrange_input_by: Option<(Vec<(Expr, ECtx)>, Vec<Var>)> =
            self.rhs_input_arrangement(rule, rhs_idx, rhs);

        // check if all skipped conditions are pure filters
        let all_filters = conds.iter().all(|&i| match &rule.body[i] {
            RuleRHS::Cond(Expr::ESet { .. }) => false,
            RuleRHS::Cond(_) => true,
            _ => false,
        });

        if all_filters {
            if let Some((pattern, _vars)) = arrange_input_by {
                let first_atom = match &rule.body[0] {
                    RuleRHS::Literal(atom) => atom,
                    _ => return None,
                };
                return self.arrange_input(first_atom, pattern);
            }
        }

        None
    }

    fn arrange_input(
        &self,
        first_atom: &crate::rule::Atom,
        arrange_input_by: Vec<(Expr, ECtx)>,
    ) -> Option<Expr> {
        let mut acc = first_atom.val.clone();

        for (i, arrange_by) in arrange_input_by.iter().enumerate() {
            match self.subst(&acc, arrange_by, i) {
                Some(new_expr) => acc = new_expr,
                None => return None,
            }
        }

        // build vars: ["_0", "_1", ...]
        let vars: Vec<String> = (0..arrange_input_by.len())
            .map(|i| format!("_{}", i))
            .collect();

        Some(normalize(vars, acc))
    }

    fn subst(&self, acc: &Expr, arrange_by: &(Expr, ECtx), i: usize) -> Option<Expr> {
        if expr_is_var_or_field(&arrange_by.0) {
            let mut found = false;
            // exprFoldM with a state flag
            let e_prime = expr_fold(
                |x: &Expr| {
                    let (new_x, hit) = self.subst_prime(x, arrange_by, i);
                    if hit {
                        found = true;
                    }
                    new_x
                },
                acc,
            );
            if found { Some(e_prime) } else { None }
        } else {
            None
        }
    }

    pub fn subst_var(&self, ab: &(Expr, ECtx), i: usize) -> Expr {
        let fresh = Expr::EVar(Identifier(format!("_{}", i))); // eVar $ "_" ++ show i
        self.subst_var_prime(ab, fresh)
    }

    fn subst_prime(&self, x: &Expr, arrange_by: &(Expr, ECtx), i: usize) -> (Expr, bool) {
        match x {
            // Binding expression
            Expr::EBinding { var, .. } => {
                if let Some(field_var) = arrange_by.0.field_expr_var() {
                    if var == field_var {
                        // matched — substitute
                        return (self.subst_var(arrange_by, i), true);
                    }
                }
                (x.clone(), false)
            }

            // Variable expression
            Expr::EVar(v) => {
                if let Some(field_var) = arrange_by.0.field_expr_var() {
                    if v == field_var {
                        return (self.subst_var(arrange_by, i), true);
                    }
                }
                (x.clone(), false)
            }

            // All the “just return” cases
            Expr::ERef(..)
            | Expr::EStruct { .. }
            | Expr::ETuple(..)
            | Expr::EBool(..)
            | Expr::EInt(..)
            | Expr::EFloat(..)
            | Expr::EString(..)
            | Expr::EPHolder
            | Expr::ETyped { .. } => (x.clone(), false),

            // Constants are preserved
            e if e.is_const() => (x.clone(), false),

            // Catch-all: panic like the Haskell code
            other => panic!(
                "Unexpected expression {:?} in arrangeInput::subst_prime",
                other
            ),
        }
    }
    fn subst_var_prime(&self, ab: &(Expr, ECtx), replacement: Expr) -> Expr {
        match (&ab.0, &ab.1) {
            // Base case: direct variable
            (Expr::EVar(_), _) => replacement,

            // Typed wrapper: recurse through inner expr
            (par @ Expr::ETyped { expr, .. }, ctx) => {
                let ctx_prime = ECtx::Typed {
                    par_expr: par.clone(),
                    par: Box::new(ctx.clone()),
                };
                self.subst_var_prime(&(*expr.clone(), ctx_prime), replacement)
            }

            // Field access
            (par @ Expr::EField { struct_, field }, ctx) => {
                let ctx_prime = ECtx::Field {
                    par_expr: par.clone(),
                    par: Box::new(ctx.clone()),
                };
                let etype = self.expr_type(&ctx_prime, struct_);
                if let Type::TStruct(args) = &etype {
                    let cons = &args[0]; // assumes one constructor
                    let mut fields = Vec::new();
                    for arg in &cons.fields {
                        let field_expr = if arg.name == *field {
                            replacement.clone()
                        } else {
                            Expr::EPHolder
                        };
                        fields.push((arg.name.clone(), field_expr));
                    }
                    let estruct = Expr::ETyped {
                        expr: Box::new(Expr::EStruct {
                            name: cons.name.clone(),
                            fields,
                        }),
                        spec: etype.clone(),
                    };
                    // Expr::EStruct{name: cons.name.clone(), fields, self.typ_deref_prime(&etype)};
                    // add back references
                    (0..self.nref(&etype)).fold(estruct, |acc, _| Expr::ERef(Box::new(acc)))
                } else {
                    panic!("Expected struct type in subst_var_prime");
                }
            }

            // Tuple field
            (par @ Expr::ETupField { tuple, field }, ctx) => {
                let ctx_prime = ECtx::TupField {
                    par_expr: par.clone(),
                    par: Box::new(ctx.clone()),
                };
                let etype = self.expr_type(&ctx_prime, tuple);
                if let Type::TTuple(args) = &etype {
                    let mut elems = Vec::new();
                    for (j, _) in args.iter().enumerate() {
                        if j == *field {
                            elems.push(replacement.clone());
                        } else {
                            elems.push(Expr::EPHolder);
                        }
                    }
                    let etup = Expr::ETuple(elems);
                    (0..self.nref(&etype)).fold(etup, |acc, _| Expr::ERef(Box::new(acc)))
                } else {
                    panic!("Expected tuple type in subst_var_prime");
                }
            }

            // Unexpected
            (other, _) => {
                panic!(
                    "Unexpected expression {:?} in arrangeInput::subst_var_prime",
                    other
                );
            }
        }
    }

    fn nref(&self, _t: &Type) -> usize {
        0
        // match t {
        //     // TOpaque with a single type parameter, and recognized as a shared ref
        //     Type::TOpaque { args, .. } if args.len() == 1 && self.is_shared_ref(t) => {
        //         1 + self.nref(&args[0])
        //     }
        //     Type::TStruct { .. } => 0,
        //     Type::TTuple { .. } => 0,
        //     _ => panic!(
        //         "Unexpected type {:?} in Compile.arrangeInput.substVar'.nref",
        //         t
        //     ),
        // }
    }
}

fn expr_is_var_or_field(expr: &Expr) -> bool {
    struct Visitor<'a> {
        prog: &'a FrameQLProgram,
        ctx: &'a ECtx,
    }
    impl<'a> ExprVisitor<bool> for Visitor<'a> {
        fn visit_expr(&mut self, e: &Expr) -> bool {
            match e {
                Expr::EVar(ident) => self.visit_var(ident),
                Expr::EField { struct_, field } => self.visit_field(struct_, field),
                Expr::ETupField { tuple, field } => self.visit_tup_field(tuple, *field),
                Expr::EStruct { name, fields } => self.visit_struc(name, fields),
                Expr::ETuple(tuple) => self.visit_tuple(tuple),
                Expr::EVarDecl(ident) => self.visit_var_decl(ident),
                Expr::ESet { lval, rval } => self.visit_set(lval, rval),
                Expr::EBinding { var, pattern } => self.visit_binding(var, pattern),
                Expr::ETyped { expr, spec } => self.visit_typed(expr, spec),
                Expr::ERef(expr) => self.visit_ref(expr),
                _ => false,
            }
        }
        fn visit_struc(&mut self, _name: &Identifier, fields: &Vec<(Identifier, Expr)>) -> bool {}

        fn visit_tuple(&mut self, tuple: &Vec<Expr>) -> bool {}

        fn visit_set(&mut self, lval: &Expr, _rval: &Expr) -> bool {}

        fn visit_binding(&mut self, var: &Identifier, pattern: &Expr) -> bool {}

        fn visit_typed(&mut self, expr: &Expr, _spec: &Type) -> bool {}

        fn visit_ref(&mut self, expr: &Expr) -> bool {}

        fn visit_var_decl(&mut self, ident: &Identifier) -> bool {}

        fn visit_var(&mut self, ident: &Identifier) -> bool {}
    }
}

fn normalize(vars: Vec<String>, acc: Expr) -> Expr {
    todo!()
}

pub fn type_subst_type_args(
    subst: &std::collections::HashMap<TypeVarName, Type>,
    t: &Type,
) -> Type {
    match t {
        Type::TUser(name, args) => Type::TUser(
            name.clone(),
            args.iter()
                .map(|a| type_subst_type_args(subst, a))
                .collect(),
        ),
        Type::TOpaque { name, args } => Type::TOpaque {
            name: name.clone(),
            args: args
                .iter()
                .map(|a| type_subst_type_args(subst, a))
                .collect(),
        },
        Type::TStruct(args) => Type::TStruct(
            args.iter()
                .map(|c| cons_subst_type_args(subst, c))
                .collect(),
        ),
        Type::TTuple(elems) => Type::TTuple(
            elems
                .iter()
                .map(|e| type_subst_type_args(subst, e))
                .collect(),
        ),
        Type::TVar(name) => subst
            .get(&TypeVarName(name.clone()))
            .unwrap_or_else(|| panic!("type_subst_type_args: missing binding for {}", name))
            .clone(),
        Type::TFunction { args, ret_type } => Type::TFunction {
            args: args
                .iter()
                .map(|t| type_subst_type_args(subst, &t))
                .collect(),
            ret_type: Box::new(type_subst_type_args(subst, ret_type)),
        },
        other => other.clone(),
    }
}

pub fn cons_subst_type_args(subst: &HashMap<TypeVarName, Type>, cons: &Constructor) -> Constructor {
    let new_args = cons
        .fields
        .iter()
        .map(|field| Field {
            ty: type_subst_type_args(subst, &field.ty),
            ..field.clone()
        })
        .collect();

    Constructor {
        fields: new_args,
        ..cons.clone()
    }
}
