use std::{path::PathBuf, str::FromStr, sync::Arc};

use framedb::{
    planner::{DefaultPhysicalPlanner, ExecutionPlan, FileTable, MemTable, PhysicalPlanner},
    query::LogicalPlan,
    query_builder::LogicalPlanBuilder,
};

fn test_insert_obj() {
    let plan: LogicalPlan = LogicalPlanBuilder::scan(
        ":mem:".to_string(),
        Arc::new(FileTable::try_new(
            PathBuf::from_str("/home/satyam").unwrap(),
        )),
    )
    .filter_by_obj_id(1)
    .build();
    let planner = DefaultPhysicalPlanner;
    let phys_plan: Arc<dyn ExecutionPlan + 'static> = planner.create_physical_plan(&plan);

    let iterator = phys_plan.execute();
}
