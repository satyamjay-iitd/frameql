///////////////////////////////////////////////////////////////////////
///                            IDENTIFIER
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

///////////////////////////////////////////////////////////////////////
///                            IMPORT
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct Import {
    pub module_path: Vec<Identifier>,
    pub alias: Option<Identifier>,
}

///////////////////////////////////////////////////////////////////////
///                            TYPE
///////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct TypeVarName(pub Identifier);

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpec {
    BigInt,
    Bool,
    String,
    BitVector(u64), // decimal width
    Integer(u64),   // signed<width>
    Double,
    Float,
    Tuple(Vec<SimpleTypeSpec>),
    Union(Vec<Constructor>),
    Function(FunctionType),
    Alias(TypeAlias),
    Var(TypeVarName),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleTypeSpec {
    BigInt,
    Bool,
    String,
    BitVector(u64),
    Double,
    Float,
    Tuple(Vec<SimpleTypeSpec>),
    Alias(TypeAlias),
    Var(TypeVarName),
    Function(FunctionType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<FuncParam>,
    pub ret: Option<Box<TypeSpec>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub mutable: bool,
    pub ty: TypeSpec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub name: Identifier,
    pub args: Vec<TypeSpec>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub fields: Vec<Field>, // empty if no { .. }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub ty: SimpleTypeSpec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: Identifier,
    pub value: Expr,
}

///////////////////////////////////////////////////////////////////////
///                            FUNCTION
///////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    FnDef(FnDef),
    ExternFn(ExternFn),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub args: Vec<Arg>,
    pub return_type: TypeSpec,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFn {
    pub name: String,
    pub args: Vec<Arg>,
    pub return_type: TypeSpec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub name: String,
    pub ty: SimpleTypeSpec,
}

///////////////////////////////////////////////////////////////////////
///                            RELATION
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum IoQualifier {
    Input,
    Output,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Relation {
    pub qualifier: Option<IoQualifier>,
    pub name: String,
    pub kind: RelationKind,
    pub primary_key: Option<PrimaryKey>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelationKind {
    Args(Vec<Arg>),
    Typed(TypeSpec),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrimaryKey {
    pub var_name: String,
    pub expr: Expr,
}

///////////////////////////////////////////////////////////////////////
///                            EXPRESSION
///////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Int(i64),
    Bool(bool),
    String(String),
    Float(f64),
    Vec(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    ConsTerm {
        name: Identifier,
        args: Vec<Expr>,                     // positional
        named_args: Vec<(Identifier, Expr)>, // named
    },
    Var(Identifier),
    Match {
        scrutinee: Box<Expr>,
        clauses: Vec<(Pattern, Expr)>,
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone)]
pub struct RuleDecl {
    pub head: Vec<Atom>,
    pub body: Vec<RhsClause>,
}

#[derive(Debug, Clone)]
pub enum Atom {
    Positional(AtomPositional),
    Named(NamedAtom),
    Indexed(IndexedAtom),
}

#[derive(Debug, Clone)]
pub struct AtomPositional {
    pub binding: Option<Identifier>, // `x in R(...)`
    pub rel: Identifier,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct NamedAtom {
    pub binding: Option<Identifier>,
    pub rel: Identifier,
    pub args: Vec<(Identifier, Expr)>, // .field = expr
}

#[derive(Debug, Clone)]
pub struct IndexedAtom {
    pub rel: Identifier,
    pub index: Expr,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Datalog {
    pub imports: Vec<(Vec<Attribute>, Import)>,
    pub typedefs: Vec<(Vec<Attribute>, Typedef)>,
    pub functions: Vec<(Vec<Attribute>, Function)>,
    pub relations: Vec<(Vec<Attribute>, Relation)>,
    pub rules: Vec<(Vec<Attribute>, RuleDecl)>,
}

#[derive(Debug, Clone)]
pub struct AnnotatedDecl {
    pub attrs: Vec<Attribute>,
    pub decl: Decl,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Import(Import),
    Typedef(Typedef),
    Function(Function),
    Relation(Relation),
    Rule(RuleDecl),
}
