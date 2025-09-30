use frameql_ast::{Identifier, RelSemantics, RelationKind};

use crate::{
    expr::{ECtx, Expr, expr_fold_ctx},
    r#type::{Constructor, Field, Type, TypeDef},
};

use frameql_ast::Relation as ASTRel;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RelRole {
    Input,
    Output,
    Internal,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Relation {
    pub role: RelRole,
    pub semantics: RelSemantics,
    pub name: Identifier,
    pub type_: Type,
    pub primary_key: Option<(Identifier, Expr)>,
}

pub(crate) fn rel_expr_map<F>(rel: Relation, mut f: F) -> Relation
where
    F: FnMut(&ECtx, Expr) -> Expr,
{
    let new_primary_key: Option<(Identifier, Expr)> = rel.primary_key.clone().map(|key| {
        let new_expr = expr_fold_ctx(
            &mut f,
            &ECtx::Key {
                relation: rel.clone(),
            },
            &key.1,
        );
        (key.0, new_expr)
    });

    Relation {
        primary_key: new_primary_key,
        ..rel
    }
}

pub struct RelationWithTDef(pub Option<TypeDef>, pub Relation);

impl From<ASTRel> for RelationWithTDef {
    fn from(value: ASTRel) -> Self {
        let role = match value.qualifier {
            Some(q) => match q {
                frameql_ast::IoQualifier::Input => RelRole::Input,
                frameql_ast::IoQualifier::Output => RelRole::Output,
            },
            None => RelRole::Internal,
        };
        let (type_def, rel_type) = match value.kind {
            RelationKind::Args(fn_args) => {
                let tspec = Type::TStruct(vec![Constructor {
                    attributes: vec![],
                    name: Identifier(value.name.clone()),
                    fields: fn_args
                        .iter()
                        .map(|arg| Field {
                            attributes: vec![],
                            name: arg.name.clone(),
                            ty: arg.ty.clone().into(),
                        })
                        .collect(),
                }]);
                (
                    Some(TypeDef {
                        name: Identifier(value.name.clone()),
                        params: vec![],
                        type_: Some(tspec),
                    }),
                    Type::TUser(Identifier(value.name.clone()), vec![]),
                )
            }
            RelationKind::Typed(type_spec) => (None, type_spec.into()),
        };
        RelationWithTDef(
            type_def,
            Relation {
                role,
                name: Identifier(value.name.clone()),
                primary_key: value.primary_key.map(|pk| (pk.var_name, pk.expr.into())),
                semantics: value.semantics.clone(),
                type_: rel_type,
            },
        )
    }
}
