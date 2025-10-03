use std::sync::Arc;

use crate::{
    planner::{ObjProvider, PhysicalExpr},
    query_builder::{MetadataFilter, ObjIDFilter, TrajectoryFilter},
};

#[derive(Clone, Debug)]
pub enum LogicalPlan {
    Scan(Scan),
    Filter(Filter),
    Union(Union),
}
impl LogicalPlan {
    pub fn inputs(&self) -> Vec<&LogicalPlan> {
        match self {
            LogicalPlan::Scan(_) => vec![],
            LogicalPlan::Filter(filter) => vec![&filter.input],
            LogicalPlan::Union(Union { inputs, .. }) => {
                inputs.iter().map(|arc| arc.as_ref()).collect()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scan {
    pub name: String,
    pub source: Arc<dyn ObjProvider>,
}

#[derive(Clone, Debug)]
pub enum FilterExpr {
    True,
    False,
    ObjId(ObjIDFilter),
    Metadata(MetadataFilter),
    Trajectory(TrajectoryFilter),
    Not(Arc<dyn PhysicalExpr>),
    Or(Arc<dyn PhysicalExpr>, Arc<dyn PhysicalExpr>),
    And(Arc<dyn PhysicalExpr>, Arc<dyn PhysicalExpr>),
}

#[derive(Clone, Debug)]
pub struct Filter {
    pub predicate: FilterExpr,
    pub input: Arc<LogicalPlan>,
}

#[derive(Clone, Debug)]
pub struct Union {
    pub inputs: Vec<Arc<LogicalPlan>>,
}
