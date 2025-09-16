use frameql_ast::{
    Attribute, Field as ASTField, Identifier, SimpleTypeSpec, TypeSpec, TypeVarName,
    Typedef as ASTTypeDef,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    TBool,
    TInt,
    TString,
    TFloat,
    TStruct(Vec<Constructor>),
    TTuple(Vec<Type>),
    TUser(Identifier, Vec<Type>),
    TVar(Identifier),
    TOpaque {
        name: Identifier,
        args: Vec<Type>,
    },
    TFunction {
        args: Vec<Type>,
        ret_type: Box<Type>,
    },
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Constructor {
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Field {
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub ty: Type,
}

impl From<ASTField> for Field {
    fn from(value: ASTField) -> Self {
        todo!()
    }
}

impl From<TypeSpec> for Type {
    fn from(value: TypeSpec) -> Self {
        todo!()
    }
}

impl From<SimpleTypeSpec> for Type {
    fn from(value: SimpleTypeSpec) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeDef {
    pub name: Identifier,
    pub params: Vec<TypeVarName>,
    pub type_: Option<Type>,
}

impl From<ASTTypeDef> for TypeDef {
    fn from(value: ASTTypeDef) -> Self {
        todo!()
    }
}

pub(crate) fn struct_fields(t: &Type) -> Vec<Field> {
    match t {
        Type::TStruct(type_cons) => {
            let mut fields: Vec<Field> = type_cons.iter().flat_map(|c| c.fields.clone()).collect();

            fields.sort_by(|a, b| a.name.cmp(&b.name));
            fields.dedup_by(|a, b| a.name == b.name);
            fields
        }
        _ => vec![],
    }
}
