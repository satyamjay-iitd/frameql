use frameql_ast::FnArg;

use crate::{expr::Expr, r#type::Type};

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

    pub fn ret_type(&self) -> Type {
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
    pub return_type: Type,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExternFn {
    pub name: String,
    pub args: Vec<FnArg>,
    pub return_type: Type,
}
