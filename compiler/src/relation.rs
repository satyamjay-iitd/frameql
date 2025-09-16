use frameql_ast::{Attribute, Identifier};

use crate::{
    expr::{ECtx, Expr, expr_fold_ctx},
    r#type::Type,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum RelRole {
    Input,
    Output,
    Internal,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RelSemantics {
    Set,
    Stream,
    Multiset,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Relation {
    attrs: Vec<Attribute>,
    role: RelRole,
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
