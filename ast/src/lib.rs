use std::fmt;

///////////////////////////////////////////////////////////////////////
///                            IDENTIFIER
///////////////////////////////////////////////////////////////////////
use ordered_float::OrderedFloat;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Identifier {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

///////////////////////////////////////////////////////////////////////
///                            IMPORT
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Import {
    pub module_path: Vec<Identifier>,
    pub alias: Option<Identifier>,
}

///////////////////////////////////////////////////////////////////////
///                            TYPE
///////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Typedef {
    Regular {
        name: Identifier,
        type_params: Vec<TypeVarName>,
        def: TypeSpec,
    },
    Extern {
        name: Identifier,
        type_params: Vec<TypeVarName>,
    },
}

impl Typedef {
    pub fn name(&self) -> &Identifier {
        match self {
            Typedef::Regular { name, .. } => name,
            Typedef::Extern { name, .. } => name,
        }
    }
    pub fn type_(&self) -> Option<&TypeSpec> {
        match self {
            Typedef::Regular { def, .. } => Some(def),
            Typedef::Extern { .. } => None,
        }
    }
    pub fn type_params(&self) -> &Vec<TypeVarName> {
        match self {
            Typedef::Regular { type_params, .. } => type_params,
            Typedef::Extern { type_params, .. } => type_params,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVarName(pub Identifier);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeSpec {
    Bool,
    String,
    BitVector(u64), // decimal width
    Integer,
    Double,
    Float,
    Tuple(Vec<SimpleTypeSpec>),
    Union(Vec<Constructor>),
    Function(FunctionType),
    Alias(TypeAlias),
    Var(TypeVarName),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SimpleTypeSpec {
    Bool,
    String,
    Integer,
    BitVector(u64),
    Double,
    Float,
    Tuple(Vec<SimpleTypeSpec>),
    Alias(TypeAlias),
    Var(TypeVarName),
    Function(FunctionType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionType {
    pub params: Vec<FuncParam>,
    pub ret: Box<TypeSpec>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncParam {
    pub mutable: bool,
    pub ty: TypeSpec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeAlias {
    pub name: Identifier,
    pub args: Vec<TypeSpec>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Constructor {
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub fields: Vec<Field>, // empty if no { .. }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Field {
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub ty: SimpleTypeSpec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Attribute {
    pub name: Identifier,
    pub value: Expr,
}

///////////////////////////////////////////////////////////////////////
///                            FUNCTION
///////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Function {
    FnDef(FnDef),
    ExternFn(ExternFn),
}
impl Function {
    pub fn args(&self) -> &Vec<FnArg> {
        match self {
            Function::FnDef(fn_def) => &fn_def.args,
            Function::ExternFn(extern_fn) => &extern_fn.args,
        }
    }

    pub fn ret_type(&self) -> TypeSpec {
        match self {
            Function::FnDef(fn_def) => fn_def.return_type.clone(),
            Function::ExternFn(extern_fn) => extern_fn.return_type.clone(),
        }
    }

    pub fn mutable_args(&self) -> Vec<FnArg> {
        self.args()
            .iter()
            .cloned()
            .filter(|arg| arg.is_mut)
            .collect()
    }
    pub fn immutable_args(&self) -> Vec<FnArg> {
        self.args()
            .iter()
            .cloned()
            .filter(|arg| !arg.is_mut)
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FnDef {
    pub name: String,
    pub args: Vec<FnArg>,
    pub return_type: TypeSpec,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExternFn {
    pub name: String,
    pub args: Vec<FnArg>,
    pub return_type: TypeSpec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FnArg {
    pub name: Identifier,
    pub ty: SimpleTypeSpec,
    pub is_mut: bool,
}

///////////////////////////////////////////////////////////////////////
///                            RELATION
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IoQualifier {
    Input,
    Output,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RelSemantics {
    Set,
    Stream,
    Multiset,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Relation {
    pub qualifier: Option<IoQualifier>,
    pub semantics: RelSemantics,
    pub name: String,
    pub kind: RelationKind,
    pub primary_key: Option<PrimaryKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RelationKind {
    Args(Vec<FnArg>),
    Typed(TypeSpec),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PrimaryKey {
    pub var_name: Identifier,
    pub expr: Expr,
}

///////////////////////////////////////////////////////////////////////
///                            EXPRESSION
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Expr {
    Term(Term),
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
    Slice {
        parent: Box<Expr>,
        index: (usize, usize),
    },
    TypeAnnotation {
        parent: Box<Expr>,
        ty: SimpleTypeSpec,
    },
    FCall {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    DotFCall {
        parent: Box<Expr>,
        name: Identifier,
        args: Vec<Expr>,
    },
    Field {
        base: Box<Expr>,
        field: Identifier,
    },
    TupleIndex {
        base: Box<Expr>,
        index: u64,
    },
    Cast {
        base: Box<Expr>,
        ty: TypeSpec,
    },
    Try(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Term {
    Int(i64),
    Bool(bool),
    String(String),
    Float(OrderedFloat<f64>),
    Vec(Vec<Expr>),
    Tuple(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    ConsTerm {
        name: Identifier,
        args: Vec<Expr>,                     // positional
        named_args: Vec<(Identifier, Expr)>, // named
    },
    Var(Identifier),
    Match {
        scrutinee: Box<Expr>,
        clauses: Vec<(Expr, Expr)>,
    },
    IfThenElse {
        cond: Box<Term>,
        then_term: Box<Term>,
        else_term: Option<Box<Term>>,
    },
    For {
        pattern: ForPattern,
        iter: Box<Expr>,
        body: Box<Term>,
    },
    Continue,
    Break,
    Return(Option<Box<Expr>>),
    VarDecl(Identifier),
    Lambda {
        params: Vec<Expr>, // you may want typed params instead
        return_ty: Option<SimpleTypeSpec>,
        body: Box<Expr>,
    },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Pattern {
    Tuple(Vec<Pattern>),
    Cons {
        name: Identifier,
        args: Vec<Pattern>,
        named_args: Vec<(Identifier, Pattern)>,
    },
    VarDecl(Identifier),
    Var(Identifier),
    Bool(bool),
    String(String),
    Int(i64),
    Wildcard,
}
impl Pattern {
    pub fn to_expr(&self) -> Expr {
        match self {
            Pattern::Tuple(patterns) => {
                Expr::Term(Term::Tuple(patterns.iter().map(|p| p.to_expr()).collect()))
            }
            Pattern::Cons {
                name,
                args,
                named_args,
            } => Expr::Term(Term::ConsTerm {
                name: name.clone(),
                args: args.iter().map(|arg| arg.to_expr()).collect(),
                named_args: named_args
                    .iter()
                    .map(|(name, val)| (name.clone(), val.to_expr()))
                    .collect(),
            }),
            Pattern::VarDecl(ident) => Expr::Term(Term::VarDecl(ident.clone())),
            Pattern::Var(ident) => Expr::Term(Term::Var(ident.clone())),
            Pattern::Bool(val) => Expr::Term(Term::Bool(*val)),
            Pattern::String(val) => Expr::Term(Term::String(val.clone())),
            Pattern::Int(val) => Expr::Term(Term::Int(*val)),
            Pattern::Wildcard => Expr::Term(Term::Wildcard),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ForPattern {
    Tuple(Vec<Pattern>),
    Cons {
        name: Identifier,
        args: Vec<Pattern>,
        named_args: Vec<(Identifier, Pattern)>,
    },
    VarDecl(Identifier),
    Var(Identifier),
    Wildcard,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BinaryOp {
    Impl,
    Assign,
    Or,
    And,
    BitOr,
    BitAnd,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Concat,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

///////////////////////////////////////////////////////////////////////
///                            RULE
///////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RuleDecl {
    pub head: Vec<Atom>,
    pub body: Vec<RhsClause>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Atom {
    Positional(AtomPositional),
    Named(NamedAtom),
    Indexed(IndexedAtom),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AtomPositional {
    pub binding: Option<Identifier>, // `x in R(...)`
    pub rel: Identifier,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NamedAtom {
    pub binding: Option<Identifier>,
    pub rel: Identifier,
    pub args: Vec<(Identifier, Expr)>, // .field = expr
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IndexedAtom {
    pub rel: Identifier,
    pub index: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RhsClause {
    Atom(Atom),
    Not(Atom),
    Expr(Expr),
    Equality(Expr, Expr),
    GroupBy {
        var: Identifier,
        value: Expr,
        key: Expr,
    },
}

///////////////////////////////////////////////////////////////////////
///                            PROGRAM
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Datalog {
    pub imports: Vec<(Vec<Attribute>, Import)>,
    pub typedefs: Vec<(Vec<Attribute>, Typedef)>,
    pub functions: Vec<(Vec<Attribute>, Function)>,
    pub relations: Vec<(Vec<Attribute>, Relation)>,
    pub rules: Vec<(Vec<Attribute>, RuleDecl)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnotatedDecl {
    pub attrs: Vec<Attribute>,
    pub decl: Decl,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Decl {
    Import(Import),
    Typedef(Typedef),
    Function(Function),
    Relation(Relation),
    RuleDecl(RuleDecl),
}
