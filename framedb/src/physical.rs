use std::path::PathBuf;

use std::fmt::Debug;
use std::fs::File;
use std::io::{BufReader, Read};
use std::sync::{Arc, Mutex, RwLock};

use dyn_hash::DynHash;

use crate::logical::{
    Filter, FilterExpr, LogicalPlan, MetadataFilter, ObjIDFilter, TrajectoryFilter,
};
use crate::{Atom, Metadata, Trajectory};

pub trait ObjProvider: Debug + Sync + Send + DynHash {
    fn scan(&self) -> Arc<dyn ExecutionPlan>;
    fn insert_into(&self, input: Arc<dyn ExecutionPlan>) -> Arc<dyn ExecutionPlan>;
}
dyn_hash::hash_trait_object!(ObjProvider);

#[derive(Debug)]
pub struct MemTable {
    pub batches: Arc<Vec<Atom>>,
}

impl std::hash::Hash for MemTable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        "mem_table".hash(state);
    }
}

impl MemTable {
    pub fn try_new(partitions: Vec<Atom>) -> Self {
        Self {
            batches: Arc::new(partitions),
        }
    }
}

pub struct DataSourceExec {
    factory: Arc<dyn Fn() -> Box<dyn Iterator<Item = Atom> + Send> + Send + Sync>,
}

impl Debug for DataSourceExec {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl DataSourceExec {
    pub fn new(
        factory: Arc<dyn Fn() -> Box<dyn Iterator<Item = Atom> + Send> + Send + Sync>,
    ) -> Self {
        Self { factory }
    }
}

impl ExecutionPlan for DataSourceExec {
    fn name(&self) -> &str {
        "DataSourceExec"
    }

    fn children(&self) -> Vec<&Arc<dyn ExecutionPlan>> {
        Vec::new()
    }

    fn with_new_children(
        self: Arc<Self>,
        _children: Vec<Arc<dyn ExecutionPlan>>,
    ) -> Arc<dyn ExecutionPlan> {
        self
    }

    fn execute(&self) -> Box<dyn Iterator<Item = Atom> + Send> {
        (self.factory)()
    }
}

/// Iterator over batches
pub struct MemoryStream {
    data: Vec<Atom>,
    index: usize,
}

impl MemoryStream {
    pub fn new(data: Vec<Atom>) -> Self {
        Self { data, index: 0 }
    }
}

impl Iterator for MemoryStream {
    type Item = Atom;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.data.len() {
            return None;
        }
        self.index += 1;
        Some(self.data[self.index - 1].clone())
    }
}

impl ObjProvider for MemTable {
    fn scan(&self) -> Arc<dyn ExecutionPlan> {
        let data = self.batches.clone();
        Arc::new(DataSourceExec::new(Arc::new(move || {
            let cloned = (*data).clone();
            Box::new(cloned.into_iter())
        })))
    }

    fn insert_into(&self, _input: Arc<dyn ExecutionPlan>) -> Arc<dyn ExecutionPlan> {
        todo!()
    }
}

struct FileSourceConfig {
    iter: Box<dyn Iterator<Item = Atom> + Send>,
}

#[derive(Debug, Hash)]
pub struct FileTable {
    file_path: PathBuf,
}

impl FileTable {
    pub fn try_new(file_path: PathBuf) -> Self {
        Self { file_path }
    }
}
impl ObjProvider for FileTable {
    fn scan(&self) -> Arc<dyn ExecutionPlan> {
        let path: String = self.file_path.to_string_lossy().into_owned();
        let factory = Arc::new(move || {
            let file = File::open(&path).expect("cannot open file");
            let reader = BufReader::new(file);
            Box::new(BincodeIter { reader }) as Box<dyn Iterator<Item = _> + Send>
        });
        Arc::new(DataSourceExec::new(factory))
    }

    fn insert_into(&self, _input: Arc<dyn ExecutionPlan>) -> Arc<dyn ExecutionPlan> {
        todo!()
    }
}

struct BincodeIter<R> {
    reader: R,
}

impl<R: Read> Iterator for BincodeIter<R> {
    type Item = Atom;

    fn next(&mut self) -> Option<Self::Item> {
        let mut len_buf = [0u8; 8];
        if self.reader.read_exact(&mut len_buf).is_err() {
            return None; // EOF
        }
        let len = u64::from_le_bytes(len_buf) as usize;

        let mut buf = vec![0u8; len];
        if self.reader.read_exact(&mut buf).is_err() {
            return None;
        }

        Some(
            bincode::decode_from_slice(&buf, bincode::config::standard())
                .unwrap()
                .0,
        )
    }
}

#[derive(Debug)]
pub struct DataSinkExec {
    input: Arc<dyn ExecutionPlan>,
    sink: Arc<dyn DataSink>,
}

impl DataSinkExec {
    /// Create a plan to write to `sink`
    pub fn new(input: Arc<dyn ExecutionPlan>, sink: Arc<dyn DataSink>) -> Self {
        Self { input, sink }
    }
}

pub fn execute_input_stream(input: Arc<dyn ExecutionPlan>) -> SendableRecordBatchStream {
    let input_stream = input.execute();
    input_stream
}

impl ExecutionPlan for DataSinkExec {
    fn name(&self) -> &str {
        "DataSinkExec"
    }

    fn children(&self) -> Vec<&Arc<dyn ExecutionPlan>> {
        vec![&self.input]
    }

    fn with_new_children(
        self: Arc<Self>,
        children: Vec<Arc<dyn ExecutionPlan>>,
    ) -> Arc<dyn ExecutionPlan> {
        Arc::new(Self::new(Arc::clone(&children[0]), Arc::clone(&self.sink)))
    }

    fn execute(&self) -> Box<dyn Iterator<Item = Atom> + Send> {
        let data = execute_input_stream(Arc::clone(&self.input));
        let sink = Arc::clone(&self.sink);
        sink.write_all(data);

        Box::new(vec![].into_iter())
    }
}

pub trait DataSink: Debug + Send + Sync {
    fn write_all(&self, data: Box<dyn Iterator<Item = Atom> + Send>) -> u64;
}
pub type PartitionData = Arc<RwLock<Vec<Atom>>>;

#[derive(Debug)]
pub struct MemSink {
    /// Target locations for writing data
    batches: PartitionData,
}

impl MemSink {
    pub fn new(batches: PartitionData) -> Self {
        Self { batches }
    }
}

pub type SendableRecordBatchStream = Box<dyn Iterator<Item = Atom> + Send>;
impl DataSink for MemSink {
    fn write_all(&self, mut data: SendableRecordBatchStream) -> u64 {
        let mut new_batches = vec![];
        let mut row_count = 0;
        while let Some(batch) = data.next() {
            row_count += 1;
            new_batches.push(batch);
        }

        let mut target = self.batches.write().unwrap();
        target.extend(new_batches);

        row_count as u64
    }
}

pub trait ExecutionPlan: Debug + Send + Sync {
    fn name(&self) -> &str;
    fn children(&self) -> Vec<&Arc<dyn ExecutionPlan>>;
    fn with_new_children(
        self: Arc<Self>,
        children: Vec<Arc<dyn ExecutionPlan>>,
    ) -> Arc<dyn ExecutionPlan>;
    fn execute(&self) -> Box<dyn Iterator<Item = Atom> + Send>;

    fn static_name() -> &'static str
    where
        Self: Sized,
    {
        let full_name = std::any::type_name::<Self>();
        let maybe_start_idx = full_name.rfind(':');
        match maybe_start_idx {
            Some(start_idx) => &full_name[start_idx + 1..],
            None => "UNKNOWN",
        }
    }

    fn visualize(&self, _img_dir: PathBuf) -> anyhow::Result<()> {
        todo!()
    }
}

pub trait PhysicalExpr: Send + Sync + Debug + DynHash {
    fn evaluate(&self, obj_id: &u64, traj: &Trajectory, metadata: &Metadata) -> bool;

    fn children(&self) -> Vec<&Arc<dyn PhysicalExpr>>;

    fn with_new_children(
        self: Arc<Self>,
        children: Vec<Arc<dyn PhysicalExpr>>,
    ) -> Arc<dyn PhysicalExpr>;
}

dyn_hash::hash_trait_object!(PhysicalExpr);

pub trait PhysicalPlanner: Send + Sync {
    fn create_physical_plan(&self, logical_plan: &LogicalPlan) -> Arc<dyn ExecutionPlan>;
    fn create_physical_expr(&self, expr: &FilterExpr) -> Arc<dyn PhysicalExpr>;
}

#[derive(Debug)]
struct ExecutionPlanChild {
    index: usize,
    plan: Arc<dyn ExecutionPlan>,
}

enum ChildrenContainer {
    None,
    One(Arc<dyn ExecutionPlan>),
    Multiple(Vec<Arc<dyn ExecutionPlan>>),
}

impl ChildrenContainer {
    fn one(self) -> Arc<dyn ExecutionPlan> {
        match self {
            Self::One(p) => p,
            _ => panic!("More than one child in ChildrenContainer"),
        }
    }
    fn two(self) -> [Arc<dyn ExecutionPlan>; 2] {
        match self {
            Self::Multiple(v) if v.len() == 2 => v.try_into().unwrap(),
            _ => panic!("ChildrenContainer doesn't contain exactly 2 children"),
        }
    }
    fn vec(self) -> Vec<Arc<dyn ExecutionPlan>> {
        match self {
            Self::None => vec![],
            Self::One(p) => vec![p],
            Self::Multiple(v) => v,
        }
    }
}

#[derive(Debug)]
enum NodeState {
    ZeroOrOneChild,
    TwoOrMoreChildren(Mutex<Vec<ExecutionPlanChild>>),
}

#[derive(Debug)]
struct LogicalNode<'a> {
    node: &'a LogicalPlan,
    // None if root
    parent_index: Option<usize>,
    state: NodeState,
}

pub struct DefaultPhysicalPlanner;

impl DefaultPhysicalPlanner {
    fn create_initial_plan(&self, logical_plan: &LogicalPlan) -> Arc<dyn ExecutionPlan> {
        let mut flat_tree = vec![];
        let mut dfs_visit_stack = vec![(None, logical_plan)];
        let mut flat_tree_leaf_indices = vec![];
        while let Some((parent_index, node)) = dfs_visit_stack.pop() {
            let current_index = flat_tree.len();
            dfs_visit_stack.extend(node.inputs().iter().map(|&n| (Some(current_index), n)));
            let state = match node.inputs().len() {
                0 => {
                    flat_tree_leaf_indices.push(current_index);
                    NodeState::ZeroOrOneChild
                }
                1 => NodeState::ZeroOrOneChild,
                _ => {
                    let ready_children = Vec::with_capacity(node.inputs().len());
                    let ready_children = Mutex::new(ready_children);
                    NodeState::TwoOrMoreChildren(ready_children)
                }
            };
            let node = LogicalNode {
                node,
                parent_index,
                state,
            };
            flat_tree.push(node);
        }
        let flat_tree = Arc::new(flat_tree);

        let mut outputs = Vec::new();

        for index in flat_tree_leaf_indices {
            let result = self.task_helper(index, Arc::clone(&flat_tree));
            if let Some(plan) = result {
                outputs.push(plan);
            }
        }

        if outputs.len() != 1 {
            panic!("Failed to convert LogicalPlan to ExecutionPlan: More than one root detected");
        }
        let plan = outputs.pop().unwrap();
        plan
    }

    fn optimize_physical_plan(
        &self,
        plan: Arc<dyn ExecutionPlan + 'static>,
    ) -> Arc<dyn ExecutionPlan + 'static> {
        plan
    }

    fn task_helper<'a>(
        &'a self,
        leaf_starter_index: usize,
        flat_tree: Arc<Vec<LogicalNode<'a>>>,
    ) -> Option<Arc<dyn ExecutionPlan>> {
        let mut node = flat_tree
            .get(leaf_starter_index)
            .expect("Invalid index whilst creating initial physical plan");
        let mut plan = self.map_logical_node_to_physical(node.node, ChildrenContainer::None);
        let mut current_index = leaf_starter_index;
        // parent_index is None only for root
        while let Some(parent_index) = node.parent_index {
            node = flat_tree
                .get(parent_index)
                .expect("Invalid index whilst creating initial physical plan");
            match &node.state {
                NodeState::ZeroOrOneChild => {
                    plan =
                        self.map_logical_node_to_physical(node.node, ChildrenContainer::One(plan));
                }
                NodeState::TwoOrMoreChildren(children) => {
                    let mut children: Vec<ExecutionPlanChild> = {
                        let mut guard = children.lock().unwrap();
                        guard.push(ExecutionPlanChild {
                            index: current_index,
                            plan,
                        });
                        if guard.len() < node.node.inputs().len() {
                            return None;
                        }
                        std::mem::take(guard.as_mut())
                    };

                    children.sort_unstable_by_key(|epc| std::cmp::Reverse(epc.index));
                    let children = children.into_iter().map(|epc| epc.plan).collect();
                    let children = ChildrenContainer::Multiple(children);
                    plan = self.map_logical_node_to_physical(node.node, children);
                }
            }
            current_index = parent_index;
        }
        Some(plan)
    }
    fn map_logical_node_to_physical(
        &self,
        node: &LogicalPlan,
        children: ChildrenContainer,
    ) -> Arc<dyn ExecutionPlan> {
        match node {
            LogicalPlan::Scan(source) => source.source.scan(),
            LogicalPlan::Filter(Filter {
                predicate,
                input: _,
            }) => {
                let physical_input = children.one();

                let runtime_expr = self.create_physical_expr(predicate);

                let filter = FilterExec::new(Arc::clone(&runtime_expr), physical_input);
                Arc::new(filter)
            }
            LogicalPlan::Union(_) => Arc::new(UnionExec::new(children.vec())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FilterExec {
    predicate: Arc<dyn PhysicalExpr>,
    input: Arc<dyn ExecutionPlan>,
}

impl FilterExec {
    pub fn new(predicate: Arc<dyn PhysicalExpr>, input: Arc<dyn ExecutionPlan>) -> Self {
        Self {
            predicate,
            input: Arc::clone(&input),
        }
    }
}

impl ExecutionPlan for FilterExec {
    fn name(&self) -> &str {
        "FilterExec"
    }

    fn children(&self) -> Vec<&Arc<dyn ExecutionPlan>> {
        vec![&self.input]
    }

    fn with_new_children(
        self: Arc<Self>,
        mut children: Vec<Arc<dyn ExecutionPlan>>,
    ) -> Arc<dyn ExecutionPlan> {
        Arc::new(FilterExec::new(
            Arc::clone(&self.predicate),
            children.swap_remove(0),
        ))
    }

    fn execute(&self) -> Box<dyn Iterator<Item = Atom> + Send> {
        Box::new(FilterExecStream {
            predicate: Arc::clone(&self.predicate),
            input: self.input.execute(),
        })
    }
}

struct FilterExecStream {
    predicate: Arc<dyn PhysicalExpr>,
    input: SendableRecordBatchStream,
}

impl Iterator for FilterExecStream {
    type Item = Atom;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(Atom {
            obj_id,
            trajectory,
            metadata,
        }) = self.input.next()
        {
            let maybe_passes = self.predicate.evaluate(&obj_id, &trajectory, &metadata);

            match maybe_passes {
                true => {
                    return Some(Atom {
                        obj_id,
                        trajectory,
                        metadata,
                    });
                }
                false => continue,
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct UnionExec {
    inputs: Vec<Arc<dyn ExecutionPlan>>,
}

impl UnionExec {
    pub fn new(inputs: Vec<Arc<dyn ExecutionPlan>>) -> Self {
        UnionExec { inputs }
    }
}

impl ExecutionPlan for UnionExec {
    fn name(&self) -> &str {
        "UnionExec"
    }

    fn children(&self) -> Vec<&Arc<dyn ExecutionPlan>> {
        self.inputs.iter().collect()
    }

    fn with_new_children(
        self: Arc<Self>,
        children: Vec<Arc<dyn ExecutionPlan>>,
    ) -> Arc<dyn ExecutionPlan> {
        Arc::new(UnionExec::new(children))
    }

    fn execute(&self) -> Box<dyn Iterator<Item = Atom> + Send> {
        let iter = self
            .inputs
            .iter()
            .map(|input| input.execute())
            .collect::<Vec<_>>()
            .into_iter()
            .flatten();

        Box::new(iter)
    }
}

impl PhysicalPlanner for DefaultPhysicalPlanner {
    fn create_physical_plan(&self, logical_plan: &LogicalPlan) -> Arc<dyn ExecutionPlan> {
        let plan = self.create_initial_plan(logical_plan);

        self.optimize_physical_plan(plan)
    }

    fn create_physical_expr(&self, expr: &FilterExpr) -> Arc<dyn PhysicalExpr> {
        Arc::new(expr.clone()) as Arc<dyn PhysicalExpr>
    }
}

impl PhysicalExpr for FilterExpr {
    fn evaluate(&self, obj_id: &u64, traj: &Trajectory, metadata: &Metadata) -> bool {
        match self {
            FilterExpr::True => true,
            FilterExpr::False => false,
            FilterExpr::ObjId(obj_idfilter) => obj_idfilter.evaluate(obj_id, traj, metadata),
            FilterExpr::Metadata(metadata_filter) => {
                metadata_filter.evaluate(obj_id, traj, metadata)
            }
            FilterExpr::Trajectory(trajectory_filter) => {
                trajectory_filter.evaluate(obj_id, traj, metadata)
            }
            FilterExpr::Not(filter_expr) => !(filter_expr.evaluate(obj_id, traj, metadata)),
            FilterExpr::Or(expr1, expr2) => {
                expr1.evaluate(obj_id, traj, metadata) || expr2.evaluate(obj_id, traj, metadata)
            }
            FilterExpr::And(expr1, expr2) => {
                expr1.evaluate(obj_id, traj, metadata) && expr2.evaluate(obj_id, traj, metadata)
            } // FilterExpr::BBox(expr) => expr.evaluate(obj_id, traj, metadata),
        }
    }

    fn children(&self) -> Vec<&Arc<dyn PhysicalExpr>> {
        match self {
            FilterExpr::Not(filter_expr) => {
                vec![filter_expr]
            }
            FilterExpr::Or(filter_expr, filter_expr1) => vec![filter_expr, filter_expr1],
            FilterExpr::And(filter_expr, filter_expr1) => vec![filter_expr, filter_expr1],
            FilterExpr::True
            | FilterExpr::False
            | FilterExpr::ObjId(_)
            | FilterExpr::Metadata(_)
            | FilterExpr::Trajectory(_) => vec![],
        }
    }

    fn with_new_children(
        self: Arc<Self>,
        _children: Vec<Arc<dyn PhysicalExpr>>,
    ) -> Arc<dyn PhysicalExpr> {
        self
    }
}

impl PhysicalExpr for ObjIDFilter {
    fn evaluate(&self, obj_id: &u64, _traj: &Trajectory, _metadata: &Metadata) -> bool {
        *obj_id == self.0
    }

    fn children(&self) -> Vec<&Arc<dyn PhysicalExpr>> {
        vec![]
    }

    fn with_new_children(
        self: Arc<Self>,
        _children: Vec<Arc<dyn PhysicalExpr>>,
    ) -> Arc<dyn PhysicalExpr> {
        self
    }
}

impl PhysicalExpr for TrajectoryFilter {
    fn evaluate(&self, _obj_id: &u64, _traj: &Trajectory, _metadata: &Metadata) -> bool {
        todo!()
    }

    fn children(&self) -> Vec<&Arc<dyn PhysicalExpr>> {
        vec![]
    }

    fn with_new_children(
        self: Arc<Self>,
        _children: Vec<Arc<dyn PhysicalExpr>>,
    ) -> Arc<dyn PhysicalExpr> {
        self
    }
}

impl PhysicalExpr for MetadataFilter {
    fn evaluate(&self, obj_id: &u64, traj: &Trajectory, metadata: &Metadata) -> bool {
        match self {
            MetadataFilter::True => true,
            MetadataFilter::False => false,
            MetadataFilter::IsA(class_id) => metadata.class.contains(class_id),
            MetadataFilter::And(f1, f2) => {
                f1.evaluate(obj_id, traj, metadata) && f2.evaluate(obj_id, traj, metadata)
            }
            MetadataFilter::Or(f1, f2) => {
                f1.evaluate(obj_id, traj, metadata) || f2.evaluate(obj_id, traj, metadata)
            }
            MetadataFilter::Not(f) => !(f.evaluate(obj_id, traj, metadata)),
        }
    }

    fn children(&self) -> Vec<&Arc<dyn PhysicalExpr>> {
        vec![]
    }

    fn with_new_children(
        self: Arc<Self>,
        _children: Vec<Arc<dyn PhysicalExpr>>,
    ) -> Arc<dyn PhysicalExpr> {
        self
    }
}
