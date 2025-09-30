use frameql_ast::{
    Attribute, Constructor as ASTCons, Field as ASTField, Identifier, SimpleTypeSpec, TypeSpec,
    TypeVarName, Typedef as ASTTypeDef,
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

impl From<ASTCons> for Constructor {
    fn from(value: ASTCons) -> Self {
        Constructor {
            attributes: value.attributes,
            name: value.name,
            fields: value.fields.into_iter().map(|f| f.into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Field {
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub ty: Type,
}

impl From<ASTField> for Field {
    fn from(value: ASTField) -> Self {
        Field {
            attributes: value.attributes,
            name: value.name,
            ty: value.ty.into(),
        }
    }
}

impl From<TypeSpec> for Type {
    fn from(value: TypeSpec) -> Self {
        match value {
            TypeSpec::Integer => Type::TInt,
            TypeSpec::Bool => Type::TBool,
            TypeSpec::String => Type::TString,
            TypeSpec::BitVector(_) => todo!(),
            TypeSpec::Double => todo!(),
            TypeSpec::Float => Type::TFloat,
            TypeSpec::Tuple(simple_type_specs) => {
                Type::TTuple(simple_type_specs.into_iter().map(|t| t.into()).collect())
            }
            TypeSpec::Alias(alias) => Type::TUser(
                alias.name,
                alias.args.into_iter().map(|x| x.into()).collect(),
            ),
            TypeSpec::Var(type_var_name) => Type::TVar(type_var_name.0),
            TypeSpec::Function(function_type) => Type::TFunction {
                args: function_type
                    .params
                    .into_iter()
                    .map(|p| p.ty.into())
                    .collect(),
                ret_type: Box::new((*function_type.ret).into()),
            },
            TypeSpec::Union(constructors) => {
                Type::TStruct(constructors.into_iter().map(|c| c.into()).collect())
            }
        }
    }
}

impl From<SimpleTypeSpec> for Type {
    fn from(value: SimpleTypeSpec) -> Self {
        match value {
            SimpleTypeSpec::Integer => Type::TInt,
            SimpleTypeSpec::Bool => Type::TBool,
            SimpleTypeSpec::String => Type::TString,
            SimpleTypeSpec::BitVector(_) => todo!(),
            SimpleTypeSpec::Double => todo!(),
            SimpleTypeSpec::Float => Type::TFloat,
            SimpleTypeSpec::Tuple(simple_type_specs) => {
                Type::TTuple(simple_type_specs.into_iter().map(|t| t.into()).collect())
            }
            SimpleTypeSpec::Alias(alias) => Type::TUser(
                alias.name,
                alias.args.into_iter().map(|x| x.into()).collect(),
            ),
            SimpleTypeSpec::Var(type_var_name) => Type::TVar(type_var_name.0),
            SimpleTypeSpec::Function(function_type) => Type::TFunction {
                args: function_type
                    .params
                    .into_iter()
                    .map(|p| p.ty.into())
                    .collect(),
                ret_type: Box::new((*function_type.ret).into()),
            },
        }
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
        TypeDef {
            name: value.name().clone(),
            params: value.type_params().clone(),
            type_: value.type_().map(|x| x.clone().into()),
        }
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
