use crate::{
    ClassId, Trajectory,
    planner::{MemTable, ObjProvider},
    query::{Filter, FilterExpr, LogicalPlan, Scan, Union},
};

use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum MetadataFilter {
    True,
    False,
    IsA(ClassId),
    And(Box<MetadataFilter>, Box<MetadataFilter>),
    Or(Box<MetadataFilter>, Box<MetadataFilter>),
    Not(Box<MetadataFilter>),
}

impl MetadataFilter {
    pub fn always() -> Self {
        MetadataFilter::True
    }

    pub fn never() -> Self {
        MetadataFilter::False
    }

    pub fn is_a(class_id: ClassId) -> Self {
        MetadataFilter::IsA(class_id)
    }

    pub fn and(self, other: impl Into<MetadataFilter>) -> Self {
        MetadataFilter::And(Box::new(self), Box::new(other.into()))
    }

    pub fn or(self, other: impl Into<MetadataFilter>) -> Self {
        MetadataFilter::Or(Box::new(self), Box::new(other.into()))
    }

    pub fn not(self) -> Self {
        MetadataFilter::Not(Box::new(self))
    }

    pub fn and_many<I>(iter: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<MetadataFilter>,
    {
        let mut iter = iter.into_iter().map(Into::into);
        let first = match iter.next() {
            Some(f) => f,
            None => return MetadataFilter::always(),
        };
        iter.fold(first, |acc, f| acc.and(f))
    }

    pub fn or_many<I>(iter: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<MetadataFilter>,
    {
        let mut iter = iter.into_iter().map(Into::into);
        let first = match iter.next() {
            Some(f) => f,
            None => return MetadataFilter::never(),
        };
        iter.fold(first, |acc, f| acc.or(f))
    }
}

impl From<ClassId> for MetadataFilter {
    fn from(class_id: ClassId) -> Self {
        MetadataFilter::IsA(class_id)
    }
}

#[derive(Clone, Debug)]
pub struct TrajectoryFilter(Trajectory);

impl From<Trajectory> for TrajectoryFilter {
    fn from(traj: Trajectory) -> Self {
        TrajectoryFilter(traj)
    }
}

#[derive(Clone, Debug)]
pub struct ObjIDFilter(pub(crate) u64);
impl From<u64> for ObjIDFilter {
    fn from(id: u64) -> Self {
        ObjIDFilter(id)
    }
}

pub struct LogicalPlanBuilder {
    query: Arc<LogicalPlan>,
}

impl LogicalPlanBuilder {
    pub fn scan(name: String, source: Arc<dyn ObjProvider>) -> LogicalPlanBuilder {
        LogicalPlanBuilder {
            query: Arc::new(LogicalPlan::Scan(Scan { name: name, source })),
        }
    }
    pub fn filter_by_obj_id(self, obj_id: u64) -> LogicalPlanBuilder {
        LogicalPlanBuilder {
            query: Arc::new(LogicalPlan::Filter(Filter {
                predicate: FilterExpr::ObjId(ObjIDFilter(obj_id)),
                input: self.query,
            })),
        }
    }
    pub fn filter_by_metadata(self, filter: MetadataFilter) -> LogicalPlanBuilder {
        LogicalPlanBuilder {
            query: Arc::new(LogicalPlan::Filter(Filter {
                predicate: FilterExpr::Metadata(filter),
                input: self.query,
            })),
        }
    }
    pub fn match_traj(self, traj: Trajectory) -> LogicalPlanBuilder {
        LogicalPlanBuilder {
            query: Arc::new(LogicalPlan::Filter(Filter {
                predicate: FilterExpr::Trajectory(TrajectoryFilter(traj)),
                input: self.query,
            })),
        }
    }
    pub fn union(self, other: LogicalPlan) -> LogicalPlanBuilder {
        LogicalPlanBuilder {
            query: Arc::new(LogicalPlan::Union(Union {
                inputs: vec![self.query, Arc::new(other)],
            })),
        }
    }
    pub fn build(self) -> LogicalPlan {
        Arc::unwrap_or_clone(self.query)
    }
}
