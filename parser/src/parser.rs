use frameql_ast::{
    AnnotatedDecl, Atom, AtomPositional, Attribute, BinaryOp, Constructor, Datalog, Decl, Expr,
    ExternFn, Field, FnArg, FnDef, ForPattern, FuncParam, Function, FunctionType, Identifier,
    Import, IndexedAtom, IoQualifier, NamedAtom, Pattern, PrimaryKey, RelSemantics, Relation,
    RelationKind, RhsClause, RuleDecl, SimpleTypeSpec, Term, TypeAlias, TypeSpec, TypeVarName,
    Typedef, UnaryOp,
};
use pest::iterators::Pair;

///////////////////////////////////////////////////////////////////////
///                            IDENTIFIER
///////////////////////////////////////////////////////////////////////

fn parse_identifier(pair: Pair<Rule>) -> Identifier {
    match pair.as_rule() {
        Rule::uc_identifier => Identifier(pair.as_str().to_string()),
        Rule::lc_identifier => Identifier(pair.as_str().to_string()),
        Rule::uc_scoped_identifier => Identifier(pair.as_str().to_string()),
        Rule::lc_scoped_identifier => Identifier(pair.as_str().to_string()),
        Rule::identifier => parse_identifier(pair.into_inner().next().unwrap()),
        Rule::rel_name => parse_identifier(pair.into_inner().next().unwrap()),
        Rule::var_name => parse_identifier(pair.into_inner().next().unwrap()),
        Rule::scoped_identifier => parse_identifier(pair.into_inner().next().unwrap()),
        Rule::type_name => parse_identifier(pair.into_inner().next().unwrap()),
        _ => unreachable!("unexpected rule: {:?}", pair.as_rule()),
    }
}

///////////////////////////////////////////////////////////////////////
///                            IMPORT
///////////////////////////////////////////////////////////////////////
fn parse_import(pair: Pair<Rule>) -> Import {
    assert_eq!(pair.as_rule(), Rule::import_decl);

    let mut module_path = Vec::new();
    let mut alias = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::module_path => {
                module_path = inner
                    .into_inner()
                    .filter(|p| p.as_rule() == Rule::identifier)
                    .map(parse_identifier)
                    .collect();
            }
            Rule::module_alias => {
                alias = Some(parse_identifier(inner.into_inner().next().unwrap()));
            }
            _ => {}
        }
    }

    Import { module_path, alias }
}

///////////////////////////////////////////////////////////////////////
///                            TYPE
///////////////////////////////////////////////////////////////////////
fn parse_typedef(pair: Pair<Rule>) -> Typedef {
    assert_eq!(pair.as_rule(), Rule::typedef);

    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();

    match first.as_rule() {
        Rule::typedef_definition => {
            // let name = parse_identifier(first.into_inner().next().unwrap());
            let mut name = None;
            let mut type_params = Vec::new();
            let mut def = None;

            for p in first.into_inner() {
                match p.as_rule() {
                    Rule::type_name => {
                        name = Some(parse_identifier(p.into_inner().next().unwrap()));
                    }
                    Rule::type_params => {
                        type_params = parse_typevar_names(p);
                    }
                    Rule::type_spec => {
                        def = Some(parse_typespec(p));
                    }
                    _ => {}
                }
            }
            for p in inner {
                match p.as_rule() {
                    Rule::type_params => {
                        type_params = parse_typevar_names(p);
                    }
                    Rule::type_spec => {
                        def = Some(parse_typespec(p));
                    }
                    _ => {}
                }
            }

            Typedef::Regular {
                name: name.expect("typedef must have a name"),
                type_params,
                def: def.expect("typedef must have a definition"),
            }
        }

        Rule::extern_type => {
            // extern type
            let mut name = None;
            let mut type_params = Vec::new();

            for p in inner {
                match p.as_rule() {
                    Rule::type_name => {
                        name = Some(parse_identifier(p.into_inner().next().unwrap()));
                    }
                    Rule::type_params => {
                        type_params = parse_typevar_names(p);
                    }
                    _ => {}
                }
            }

            Typedef::Extern {
                name: name.expect("extern type must have a name"),
                type_params,
            }
        }

        _ => unreachable!("unexpected typedef form"),
    }
}

fn parse_typevar_names(pair: Pair<Rule>) -> Vec<TypeVarName> {
    assert_eq!(pair.as_rule(), Rule::type_params);
    pair.into_inner()
        .filter(|p| p.as_rule() == Rule::typevar_name)
        .map(|p| TypeVarName(parse_identifier(p.into_inner().next().unwrap())))
        .collect()
}

fn parse_typespec(pair: Pair<Rule>) -> TypeSpec {
    assert_eq!(pair.as_rule(), Rule::type_spec);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::bool_type => TypeSpec::Bool,
        Rule::string_type => TypeSpec::String,
        Rule::bitvector_type => {
            let width = parse_decimal(inner.into_inner().next().unwrap());
            TypeSpec::BitVector(width)
        }
        Rule::integer_type => {
            // let width = parse_decimal(inner.into_inner().next().unwrap());
            TypeSpec::Integer
        }
        Rule::double_type => TypeSpec::Double,
        Rule::float_type => TypeSpec::Float,
        Rule::tuple_type => {
            let elems = inner.into_inner().map(parse_simple_typespec).collect();
            TypeSpec::Tuple(elems)
        }
        Rule::union_type => {
            let ctors = inner.into_inner().map(parse_cons).collect();
            TypeSpec::Union(ctors)
        }
        Rule::function_type => TypeSpec::Function(parse_functype(inner)),
        Rule::type_alias => TypeSpec::Alias(parse_typealias(inner)),
        Rule::typevar_name => TypeSpec::Var(TypeVarName(parse_identifier(
            inner.into_inner().next().unwrap(),
        ))),
        _ => unreachable!("unexpected type_spec: {:?}", inner.as_rule()),
    }
}

fn parse_simple_typespec(pair: Pair<Rule>) -> SimpleTypeSpec {
    assert_eq!(pair.as_rule(), Rule::simple_type_spec);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::integer_type => SimpleTypeSpec::Integer,
        Rule::bool_type => SimpleTypeSpec::Bool,
        Rule::string_type => SimpleTypeSpec::String,
        Rule::bitvector_type => {
            let width = parse_decimal(inner.into_inner().next().unwrap());
            SimpleTypeSpec::BitVector(width)
        }
        Rule::double_type => SimpleTypeSpec::Double,
        Rule::float_type => SimpleTypeSpec::Float,
        Rule::tuple_type => {
            let elems = inner.into_inner().map(parse_simple_typespec).collect();
            SimpleTypeSpec::Tuple(elems)
        }
        Rule::type_alias => SimpleTypeSpec::Alias(parse_typealias(inner)),
        Rule::typevar_name => SimpleTypeSpec::Var(TypeVarName(parse_identifier(
            inner.into_inner().next().unwrap(),
        ))),
        Rule::function_type => SimpleTypeSpec::Function(parse_functype(inner)),
        _ => unreachable!("unexpected simple_type_spec"),
    }
}

fn parse_typealias(pair: Pair<Rule>) -> TypeAlias {
    assert_eq!(pair.as_rule(), Rule::type_alias);

    let mut name = None;
    let mut args = Vec::new();

    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::type_name => {
                name = Some(parse_identifier(p.into_inner().next().unwrap()));
            }
            Rule::type_args => {
                args = p
                    .into_inner()
                    .filter(|x| x.as_rule() == Rule::type_spec)
                    .map(parse_typespec)
                    .collect();
            }
            _ => {}
        }
    }

    TypeAlias {
        name: name.unwrap(),
        args,
    }
}

///////////////////////////////////////////////////////////////////////
///                            FUNCTION
///////////////////////////////////////////////////////////////////////
fn parse_functype(pair: Pair<Rule>) -> FunctionType {
    assert_eq!(pair.as_rule(), Rule::function_type);
    let mut params = Vec::new();
    let mut ret: Option<Box<TypeSpec>> = None;

    // iterate children exactly once and pick out the pieces we care about
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::func_params => {
                params = inner.into_inner().map(parse_fnparam).collect();
            }
            Rule::type_spec => {
                ret = Some(Box::new(parse_typespec(inner)));
            }
            _ => {}
        }
    }

    let ret = ret.expect("parse_functype: missing return type");

    FunctionType { params, ret }
}

fn parse_fnparam(pair: Pair<Rule>) -> FuncParam {
    assert_eq!(pair.as_rule(), Rule::func_param);
    let mut mutable = false;
    let mut ty = None;

    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::type_spec => ty = Some(parse_typespec(p)),
            Rule::mut_kw => mutable = true,
            _ => {}
        }
    }

    FuncParam {
        mutable,
        ty: ty.unwrap(),
    }
}

fn parse_cons(pair: Pair<Rule>) -> Constructor {
    assert_eq!(pair.as_rule(), Rule::constructor);

    let mut attrs = Vec::new();
    let mut name = None;
    let mut fields = Vec::new();

    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::attributes => {
                attrs = parse_attributes(p);
            }
            Rule::cons_name => {
                name = Some(parse_identifier(p.into_inner().next().unwrap()));
            }
            Rule::field_list => {
                fields = p.into_inner().map(parse_field).collect();
            }
            _ => {}
        }
    }

    Constructor {
        attributes: attrs,
        name: name.unwrap(),
        fields,
    }
}

fn parse_field(pair: Pair<Rule>) -> Field {
    assert_eq!(pair.as_rule(), Rule::field);

    let mut attrs = Vec::new();
    let mut name = None;
    let mut ty = None;

    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::attributes => {
                attrs = parse_attributes(p);
            }
            Rule::field_name => {
                name = Some(parse_identifier(p.into_inner().next().unwrap()));
            }
            Rule::simple_type_spec => {
                ty = Some(parse_simple_typespec(p));
            }
            _ => {}
        }
    }

    Field {
        attributes: attrs,
        name: name.unwrap(),
        ty: ty.unwrap(),
    }
}

fn parse_attributes(pair: Pair<Rule>) -> Vec<Attribute> {
    assert_eq!(pair.as_rule(), Rule::attributes);

    pair.into_inner()
        .filter(|p| p.as_rule() == Rule::attribute)
        .map(|attr_pair| {
            let mut inner = attr_pair.into_inner();
            let name = parse_identifier(inner.next().unwrap());
            let value = parse_expr(inner.next().unwrap());

            Attribute { name, value }
        })
        .collect()
}

fn parse_decimal(pair: Pair<Rule>) -> u64 {
    pair.as_str().parse().unwrap()
}

///////////////////////////////////////////////////////////////////////
///                            FUNCTION
///////////////////////////////////////////////////////////////////////

fn parse_fn(pair: Pair<Rule>) -> Function {
    assert_eq!(pair.as_rule(), Rule::function);

    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();

    if first.as_rule() == Rule::EOI {
        panic!("Unexpected empty function rule");
    }

    let name;
    let args;
    let return_type;

    if first.as_str() == "extern" {
        let _fn_kw = inner.next().unwrap(); // "function"
        name = inner.next().unwrap().as_str().to_string();

        args = if let Some(arg_list) = inner.peek() {
            if arg_list.as_rule() == Rule::arg_list {
                inner.next().unwrap().into_inner().map(parse_arg).collect()
            } else {
                vec![]
            }
        } else {
            vec![]
        };

        let _colon = inner.next().unwrap(); // ":"
        return_type = parse_typespec(inner.next().unwrap());
        Function::ExternFn(ExternFn {
            name,
            args,
            return_type,
        })
    } else {
        // normal function
        let _fn_kw = first; // "function"
        name = inner.next().unwrap().as_str().to_string();

        args = if let Some(arg_list) = inner.peek() {
            if arg_list.as_rule() == Rule::arg_list {
                inner.next().unwrap().into_inner().map(parse_arg).collect()
            } else {
                vec![]
            }
        } else {
            vec![]
        };

        let _colon = inner.next().unwrap(); // ":"
        return_type = parse_typespec(inner.next().unwrap());

        let _brace_open = inner.next().unwrap(); // "{"
        let expr_pair = inner.next().unwrap(); // expr
        let body = parse_expr(expr_pair);
        let _brace_close = inner.next().unwrap(); // "}"
        Function::FnDef(FnDef {
            name,
            args,
            return_type,
            body,
        })
    }
}

pub fn parse_arg(pair: Pair<Rule>) -> FnArg {
    assert_eq!(pair.as_rule(), Rule::arg);
    let mut inner = pair.into_inner();

    // check for optional "mut"
    let mut is_mut = false;
    let first = inner.peek().unwrap();
    if first.as_rule() == Rule::mut_kw {
        // define a rule mut_kw = { "mut" }
        inner.next();
        is_mut = true;
    }

    let name = Identifier(inner.next().unwrap().as_str().to_string());
    let ty = parse_simple_typespec(inner.next().unwrap());

    FnArg { name, ty, is_mut }
}

///////////////////////////////////////////////////////////////////////
///                            RELATION
///////////////////////////////////////////////////////////////////////

pub fn parse_relation(pair: Pair<Rule>) -> Relation {
    assert_eq!(pair.as_rule(), Rule::relation);
    let mut inner = pair.into_inner();

    let mut qualifier = None;
    if let Some(peek) = inner.peek() {
        if peek.as_rule() == Rule::io_qualifier {
            qualifier = Some(parse_ioqualifier(inner.next().unwrap()));
        }
    }
    let next = inner.next().unwrap();
    let semantics = match next.as_rule() {
        Rule::rel_sem => parse_rel_sem(next),
        _ => panic!("Unexpected rule in relation"),
    };

    // rel_name
    let name = inner.next().unwrap().as_str().to_string();

    // either (arg_list) or [simple_type_spec]
    let next = inner.next().unwrap();
    let kind = match next.as_rule() {
        Rule::arg_list => {
            let args = next.into_inner().map(parse_arg).collect();
            RelationKind::Args(args)
        }
        Rule::simple_type_spec => RelationKind::Typed(parse_typespec(next)),
        _ => panic!("Unexpected rule in relation: {:?}", next.as_rule()),
    };

    // optional primary_key
    let primary_key = if let Some(pk) = inner.next() {
        Some(parse_primary_key(pk))
    } else {
        None
    };

    Relation {
        qualifier,
        name,
        kind,
        primary_key,
        semantics,
    }
}

pub fn parse_ioqualifier(pair: Pair<Rule>) -> IoQualifier {
    match pair.as_str() {
        "input" => IoQualifier::Input,
        "output" => IoQualifier::Output,
        other => panic!("Invalid io_qualifier: {}", other),
    }
}

pub fn parse_rel_sem(pair: Pair<Rule>) -> RelSemantics {
    match pair.as_str() {
        "stream" => RelSemantics::Stream,
        "multiset" => RelSemantics::Multiset,
        "relation" => RelSemantics::Set,
        other => panic!("Invalid io_qualifier: {}", other),
    }
}

pub fn parse_primary_key(pair: Pair<Rule>) -> PrimaryKey {
    assert_eq!(pair.as_rule(), Rule::primary_key);
    let mut inner = pair.into_inner();

    let _kw_primary = inner.next().unwrap();
    let _kw_key = inner.next().unwrap();
    let _open_paren = inner.next().unwrap();
    let var_name = Identifier(inner.next().unwrap().as_str().to_string());
    let _close_paren = inner.next().unwrap();
    let expr = parse_expr(inner.next().unwrap());

    PrimaryKey { var_name, expr }
}

///////////////////////////////////////////////////////////////////////
///                            EXPRESSION
///////////////////////////////////////////////////////////////////////
use pest::pratt_parser::{Assoc, Op, PrattParser};

use std::sync::OnceLock;

use crate::Rule;

static EXPR_PRATT: OnceLock<PrattParser<Rule>> = OnceLock::new();

fn get_pratt_parser() -> &'static PrattParser<Rule> {
    EXPR_PRATT.get_or_init(|| {
        PrattParser::new()
            .op(Op::infix(Rule::assign, Assoc::Right))
            .op(Op::infix(Rule::r#impl, Assoc::Right))
            .op(Op::infix(Rule::logic_or, Assoc::Left))
            .op(Op::infix(Rule::logic_and, Assoc::Left))
            .op(Op::infix(Rule::bitwise_or, Assoc::Left))
            .op(Op::infix(Rule::bitwise_and, Assoc::Left))
            .op(Op::infix(Rule::bool_op, Assoc::Left))
            .op(Op::infix(Rule::concat, Assoc::Left))
            .op(Op::infix(Rule::shifts, Assoc::Left))
            .op(Op::infix(Rule::add_sub, Assoc::Left))
            .op(Op::infix(Rule::mul_div_mod, Assoc::Left))
            .op(Op::prefix(Rule::prefix))
            // prefix ops (highest precedence)
            .op(Op::postfix(Rule::postfix))
    })
}

fn parse_expr(pair: pest::iterators::Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::expr);

    get_pratt_parser()
        .map_primary(|primary| {
            let inner = primary.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::term => Expr::Term(parse_term(inner)),
                Rule::expr => parse_expr(inner),
                _ => panic!("unexpected primary in expr: {:?}", inner.as_rule()),
            }
        })
        .map_prefix(|op, rhs| {
            let rhs = Box::new(rhs);
            let op = match op.as_rule() {
                Rule::prefix => match op.as_str() {
                    "-" => UnaryOp::Neg,
                    "~" => UnaryOp::BitNot,
                    "not" => UnaryOp::Not,
                    _ => panic!("unknown prefix {}", op.as_str()),
                },
                _ => panic!("unexpected prefix rule: {:?}", op.as_rule()),
            };
            Expr::Unary { op, rhs }
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = Box::new(lhs);
            let rhs = Box::new(rhs);
            let op = match op.as_rule() {
                Rule::r#impl => BinaryOp::Impl,
                Rule::assign => BinaryOp::Assign,
                Rule::logic_or => BinaryOp::Or,
                Rule::logic_and => BinaryOp::And,
                Rule::bitwise_or => BinaryOp::BitOr,
                Rule::bitwise_and => BinaryOp::BitAnd,
                Rule::bool_op => match op.as_str() {
                    "==" => BinaryOp::Eq,
                    "!=" => BinaryOp::Ne,
                    "<" => BinaryOp::Lt,
                    "<=" => BinaryOp::Le,
                    ">" => BinaryOp::Gt,
                    ">=" => BinaryOp::Ge,
                    _ => unreachable!(),
                },
                Rule::concat => BinaryOp::Concat,
                Rule::shifts => match op.as_str() {
                    "<<" => BinaryOp::Shl,
                    ">>" => BinaryOp::Shr,
                    _ => unreachable!(),
                },
                Rule::add_sub => match op.as_str() {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    _ => unreachable!(),
                },
                Rule::mul_div_mod => match op.as_str() {
                    "*" => BinaryOp::Mul,
                    "/" => BinaryOp::Div,
                    "%" => BinaryOp::Mod,
                    _ => unreachable!(),
                },
                _ => panic!("unexpected infix rule {:?}", op.as_rule()),
            };
            Expr::Binary { lhs, op, rhs }
        })
        .map_postfix(|expr, postfix| {
            assert_eq!(postfix.as_rule(), Rule::postfix);
            let ops = postfix.into_inner();
            let mut parent = expr;
            for op in ops {
                match op.as_rule() {
                    Rule::slice => {
                        let mut inner = op.into_inner();
                        let start = inner.next().unwrap().as_str().parse::<usize>().unwrap();
                        let end = inner.next().unwrap().as_str().parse::<usize>().unwrap();
                        parent = Expr::Slice {
                            parent: Box::new(parent),
                            index: (start, end),
                        };
                    }
                    Rule::type_annotation => {
                        // type_annotation = ":" ~ simple_type_spec
                        let ty = parse_simple_typespec(op.into_inner().next().unwrap());
                        parent = Expr::TypeAnnotation {
                            parent: Box::new(parent),
                            ty,
                        };
                    }
                    Rule::struct_field => {
                        // .identifier
                        let ident = parse_identifier(op.into_inner().next().unwrap());
                        parent = Expr::Field {
                            base: Box::new(parent),
                            field: ident,
                        };
                    }
                    Rule::fcall => {
                        // ( expr_list? )
                        let args = op.into_inner().map(parse_expr).collect::<Vec<_>>();
                        parent = Expr::FCall {
                            func: Box::new(parent),
                            args,
                        };
                    }
                    Rule::dotcall => {
                        // .func_name(expr_list?)
                        let mut inner = op.into_inner();
                        let name = parse_identifier(inner.next().unwrap());
                        let args = inner.map(parse_expr).collect::<Vec<_>>();
                        parent = Expr::DotFCall {
                            parent: Box::new(parent),
                            name,
                            args,
                        };
                    }
                    Rule::tuple_field => {
                        // .decimal
                        let index = op
                            .into_inner()
                            .next()
                            .unwrap()
                            .as_str()
                            .parse::<u64>()
                            .unwrap();
                        parent = Expr::TupleIndex {
                            base: Box::new(parent),
                            index,
                        };
                    }
                    Rule::cast => {
                        // "as" simple_type_spec
                        let ty = parse_typespec(op.into_inner().next().unwrap());
                        parent = Expr::Cast {
                            base: Box::new(parent),
                            ty,
                        };
                    }
                    Rule::try_ => {
                        parent = Expr::Try(Box::new(parent));
                    }
                    _ => panic!("unexpected postfix rule {:?}", op.as_rule()),
                }
            }
            parent
        })
        .parse(pair.into_inner())
}

pub fn parse_term(pair: Pair<Rule>) -> Term {
    assert_eq!(pair.as_rule(), Rule::term);

    let inner: Pair<'_, Rule> = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::int_literal => {
            let n = inner.as_str().parse().unwrap();
            Term::Int(n)
        }
        Rule::bool_literal => Term::Bool(inner.as_str() == "true"),
        Rule::fp_literal => {
            let n = inner.as_str().parse().unwrap();
            Term::Float(n)
        }
        Rule::string_literal => {
            // strip quotes if needed
            let s = inner.as_str().trim_matches('"').to_string();
            Term::String(s)
        }
        Rule::vec_literal => {
            let exprs = inner
                .into_inner()
                .filter(|p| p.as_rule() == Rule::expr)
                .map(parse_expr)
                .collect();
            Term::Vec(exprs)
        }
        Rule::map_literal => {
            let kvs = inner
                .into_inner()
                .filter(|p| p.as_rule() == Rule::kv_pair)
                .map(|kv| {
                    let mut it = kv.into_inner();
                    let k = parse_expr(it.next().unwrap());
                    let v = parse_expr(it.next().unwrap());
                    (k, v)
                })
                .collect();
            Term::Map(kvs)
        }
        Rule::var_term => Term::Var(parse_identifier(inner.into_inner().next().unwrap())),
        Rule::vardecl_term => Term::VarDecl(parse_identifier(inner.into_inner().next().unwrap())),
        Rule::ite_term => {
            let mut it = inner.into_inner();
            let cond = parse_term(it.next().unwrap());
            let then_t = parse_term(it.next().unwrap());
            let else_t = it.next().map(parse_term);
            Term::IfThenElse {
                cond: Box::new(cond),
                then_term: Box::new(then_t),
                else_term: else_t.map(Box::new),
            }
        }
        Rule::match_term => {
            let mut it = inner.into_inner();
            let scrutinee = Box::new(parse_expr(it.next().unwrap()));
            let clauses = it
                .filter(|p| p.as_rule() == Rule::match_clause)
                .map(|cl| {
                    let mut c_it = cl.into_inner();
                    let pat = parse_pattern(c_it.next().unwrap());
                    let expr = parse_expr(c_it.next().unwrap());
                    (pat.to_expr(), expr)
                })
                .collect();
            Term::Match { scrutinee, clauses }
        }
        Rule::for_term => {
            let mut it = inner.into_inner();
            let pat = parse_forpattern(it.next().unwrap());
            let iter = Box::new(parse_expr(it.next().unwrap()));
            let body = parse_term(it.next().unwrap());
            Term::For {
                pattern: pat,
                iter,
                body: Box::new(body),
            }
        }
        Rule::return_term => {
            let mut it = inner.into_inner();
            let expr = it.next().map(parse_expr).map(Box::new);
            Term::Return(expr)
        }
        Rule::lambda_term => {
            // here you’ll likely want to parse arg list differently
            let mut it = inner.into_inner();
            // placeholder impl:
            let body = parse_expr(it.next_back().unwrap());
            Term::Lambda {
                params: Vec::new(),
                return_ty: None,
                body: Box::new(body),
            }
        }
        Rule::cons_term => {
            let name = parse_identifier(inner.clone().into_inner().next().unwrap());
            if let Rule::expr_list = inner.clone().into_inner().peek().unwrap().as_rule() {
                Term::ConsTerm {
                    name,
                    args: inner.into_inner().into_iter().map(parse_expr).collect(),
                    named_args: Vec::new(),
                }
            } else {
                Term::ConsTerm {
                    name,
                    args: Vec::new(),
                    named_args: inner
                        .into_inner()
                        .into_iter()
                        .map(parse_named_field)
                        .collect(),
                }
            }
        }
        Rule::EOI => Term::Wildcard,
        _ => panic!("unhandled term rule {:?}", inner.as_rule()),
    }
}

pub fn parse_named_field(pair: Pair<Rule>) -> (Identifier, Expr) {
    assert_eq!(pair.as_rule(), Rule::named_field);

    let name = parse_identifier(pair.clone().into_inner().next().unwrap());
    let val = parse_expr(pair.into_inner().next().unwrap());

    (name, val)
}

pub fn parse_pattern(pair: Pair<Rule>) -> Pattern {
    assert_eq!(pair.as_rule(), Rule::pattern);

    let inner = pair.into_inner().next().unwrap();
    if inner.as_str() == "_" {
        return Pattern::Wildcard;
    }
    match inner.as_rule() {
        Rule::int_literal => {
            let n = inner.as_str().parse().unwrap();
            Pattern::Int(n)
        }
        Rule::bool_literal => Pattern::Bool(inner.as_str() == "true"),
        Rule::string_literal => {
            // trim quotes if needed
            let s = inner.as_str().trim_matches('"').to_string();
            Pattern::String(s)
        }
        Rule::var_term => Pattern::Var(parse_identifier(inner.into_inner().next().unwrap())),
        Rule::vardecl_term => {
            Pattern::VarDecl(parse_identifier(inner.into_inner().next().unwrap()))
        }
        Rule::pattern => {
            // nested pattern — recurse
            parse_pattern(inner)
        }
        Rule::pattern_list => {
            let pats = inner.into_inner().map(parse_pattern).collect();
            Pattern::Tuple(pats)
        }
        Rule::named_pattern_list => {
            let fields = inner
                .into_inner()
                .map(|np| {
                    let mut it = np.into_inner();
                    let name = parse_identifier(it.next().unwrap()); // field_name
                    let pat = parse_pattern(it.next().unwrap());
                    (name, pat)
                })
                .collect::<Vec<_>>();
            // this is only valid when under a constructor
            Pattern::Cons {
                name: Identifier("".into()), // will be filled in Cons parse below
                args: vec![],
                named_args: fields,
            }
        }
        Rule::cons_name => {
            let mut it = inner.into_inner();
            let name = parse_identifier(it.next().unwrap());

            let mut args = Vec::new();
            let mut named_args = Vec::new();

            for child in it {
                match child.as_rule() {
                    Rule::pattern_list => {
                        args.extend(child.into_inner().map(parse_pattern));
                    }
                    Rule::named_pattern_list => {
                        named_args.extend(child.into_inner().map(|np| {
                            let mut it = np.into_inner();
                            let name = parse_identifier(it.next().unwrap());
                            let pat = parse_pattern(it.next().unwrap());
                            (name, pat)
                        }));
                    }
                    _ => {}
                }
            }

            Pattern::Cons {
                name,
                args,
                named_args,
            }
        }
        Rule::pattern_tuple => {
            let pats = inner.into_inner().map(parse_pattern).collect();
            Pattern::Tuple(pats)
        }
        _ => panic!("unhandled pattern rule {:?}", inner.as_rule()),
    }
}

pub fn parse_forpattern(pair: Pair<Rule>) -> ForPattern {
    assert_eq!(pair.as_rule(), Rule::for_pattern);

    let pattern = parse_pattern(pair);
    match pattern {
        Pattern::Tuple(patterns) => ForPattern::Tuple(patterns),
        Pattern::Cons {
            name,
            args,
            named_args,
        } => ForPattern::Cons {
            name,
            args,
            named_args,
        },
        Pattern::VarDecl(identifier) => ForPattern::VarDecl(identifier),
        Pattern::Var(identifier) => ForPattern::Var(identifier),
        Pattern::Wildcard => ForPattern::Wildcard,
        _ => panic!("Unexpected pattern"),
    }
}

///////////////////////////////////////////////////////////////////////
///                            RULE
///////////////////////////////////////////////////////////////////////

pub fn parse_rule(pair: Pair<Rule>) -> RuleDecl {
    assert_eq!(pair.as_rule(), Rule::rule_decl);
    let mut inner = pair.into_inner();

    let mut head_atoms = Vec::new();
    if let Some(lhs_pair) = inner.next() {
        assert_eq!(lhs_pair.as_rule(), Rule::rule_lhs);
        for atom_pair in lhs_pair.into_inner() {
            if atom_pair.as_rule() == Rule::atom {
                head_atoms.push(parse_atom(atom_pair));
            }
        }
    }

    // Parse optional body (rule_rhs)
    let mut body = Vec::new();
    if let Some(rhs_pair) = inner.next() {
        assert_eq!(rhs_pair.as_rule(), Rule::rule_rhs);
        for clause_pair in rhs_pair.into_inner() {
            if clause_pair.as_rule() == Rule::rhs_clause {
                body.push(parse_rhs(clause_pair));
            }
        }
    }

    RuleDecl {
        head: head_atoms,
        body,
    }
}

pub fn parse_atom(pair: Pair<Rule>) -> Atom {
    assert_eq!(pair.as_rule(), Rule::atom);
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::atom1 => Atom::Positional(parse_atom_pos(inner)),
        Rule::atom2 => Atom::Named(parse_atom_named(inner)),
        Rule::atom3 => Atom::Indexed(parse_atom_indexed(inner)),
        _ => panic!("unexpected atom start {:?}", inner.as_rule()),
    }
}

pub fn parse_atom_pos(pair: Pair<Rule>) -> AtomPositional {
    assert_eq!(pair.as_rule(), Rule::atom1);
    let mut inner = pair.into_inner();

    // optional binding
    let mut binding = None;
    let first = inner.peek().unwrap();
    if first.as_rule() == Rule::var_name {
        let var_pair = inner.next().unwrap();
        binding = Some(parse_identifier(var_pair));
    }

    // relation name
    let rel = parse_identifier(inner.next().unwrap());

    // arguments (expr list)
    let mut args = Vec::new();
    for p in inner {
        if p.as_rule() == Rule::expr {
            args.push(parse_expr(p));
        }
    }

    AtomPositional { binding, rel, args }
}

pub fn parse_atom_named(pair: Pair<Rule>) -> NamedAtom {
    assert_eq!(pair.as_rule(), Rule::atom2);
    let mut inner = pair.into_inner();

    // optional binding
    let mut binding = None;
    let first = inner.peek().unwrap();
    if first.as_rule() == Rule::var_name {
        let var_pair = inner.next().unwrap();
        binding = Some(parse_identifier(var_pair));

        // consume the "in" token (comes after var_name)
        let _in = inner.next();
    }

    // relation name
    let rel = parse_identifier(inner.next().unwrap());

    // parse `.field = expr` list
    let mut args = Vec::new();
    while let Some(p) = inner.next() {
        match p.as_rule() {
            Rule::arg_name => {
                let name = parse_identifier(p);
                let eq_expr = inner.next().unwrap();
                assert_eq!(eq_expr.as_rule(), Rule::expr);
                let expr = parse_expr(eq_expr);
                args.push((name, expr));
            }
            _ => {
                // ignore commas, parens
            }
        }
    }

    NamedAtom { binding, rel, args }
}

pub fn parse_atom_indexed(pair: Pair<Rule>) -> IndexedAtom {
    assert_eq!(pair.as_rule(), Rule::atom3);
    let mut inner = pair.into_inner();

    // first: relation name
    let rel_pair = inner.next().unwrap();
    let rel = parse_identifier(rel_pair);

    // second: index expression
    let expr_pair = inner.next().unwrap();
    let index = parse_expr(expr_pair);

    IndexedAtom { rel, index }
}

pub fn parse_rhs(pair: Pair<Rule>) -> RhsClause {
    assert_eq!(pair.as_rule(), Rule::rhs_clause);
    let mut inner = pair.into_inner();
    let first = inner.next().unwrap();

    if first.as_str() == "not" {
        let atom = inner.next().unwrap();
        return RhsClause::Not(parse_atom(atom));
    }

    match first.as_rule() {
        Rule::atom => RhsClause::Atom(parse_atom(first)),
        Rule::expr => {
            if let Some(eq) = inner.next() {
                assert_eq!(eq.as_rule(), Rule::assign);
                let rhs = inner.next().unwrap();
                RhsClause::Equality(parse_expr(first), parse_expr(rhs))
            } else {
                RhsClause::Expr(parse_expr(first))
            }
        }
        Rule::var_name => {
            let var = Identifier(first.as_str().to_string());
            inner.next(); // skip '='
            let value = parse_expr(inner.next().unwrap());
            inner.next(); // skip "."
            inner.next(); // skip "group_by"
            let key = parse_expr(inner.next().unwrap());
            RhsClause::GroupBy { var, value, key }
        }
        _ => panic!("unexpected rhs_clause {:?}", first.as_rule()),
    }
}

///////////////////////////////////////////////////////////////////////
///                            PROGRAM
///////////////////////////////////////////////////////////////////////

pub fn parse_datalog(pair: Pair<Rule>) -> Datalog {
    assert_eq!(pair.as_rule(), Rule::datalog);
    let decls: Vec<AnnotatedDecl> = pair
        .into_inner()
        .filter(|p| p.as_rule() == Rule::annotated_decl)
        .map(parse_annotated_decl)
        .collect();

    let mut imports = Vec::new();
    let mut typedefs = Vec::new();
    let mut functions = Vec::new();
    let mut relations = Vec::new();
    let mut rules = Vec::new();
    for decl in decls {
        match decl.decl {
            Decl::Import(import) => imports.push((decl.attrs, import)),
            Decl::Typedef(typedef) => typedefs.push((decl.attrs, typedef)),
            Decl::Function(function) => functions.push((decl.attrs, function)),
            Decl::Relation(relation) => relations.push((decl.attrs, relation)),
            Decl::RuleDecl(rule_decl) => rules.push((decl.attrs, rule_decl)),
        }
    }
    Datalog {
        imports,
        typedefs,
        functions,
        relations,
        rules,
    }
}

fn parse_annotated_decl(pair: Pair<Rule>) -> AnnotatedDecl {
    assert_eq!(pair.as_rule(), Rule::annotated_decl);
    let inner = pair.into_inner();

    let mut attrs = Vec::new();
    let mut decl = None;

    for p in inner {
        match p.as_rule() {
            Rule::attributes => {
                attrs = parse_attributes(p);
            }
            Rule::decl => {
                decl = Some(parse_decl(p));
            }
            _ => unreachable!(),
        }
    }

    AnnotatedDecl {
        attrs,
        decl: decl.expect("annotated_decl must contain a decl"),
    }
}

fn parse_decl(pair: Pair<Rule>) -> Decl {
    assert_eq!(pair.as_rule(), Rule::decl);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::import_decl => Decl::Import(parse_import(inner)),
        Rule::typedef => Decl::Typedef(parse_typedef(inner)),
        Rule::function => Decl::Function(parse_fn(inner)),
        Rule::relation => Decl::Relation(parse_relation(inner)),
        Rule::rule_decl => Decl::RuleDecl(parse_rule(inner)),
        _ => unreachable!(),
    }
}
