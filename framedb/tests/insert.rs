use std::{path::PathBuf, str::FromStr as _, sync::Arc};

use framedb::{
    logical::{LogicalPlan, LogicalPlanBuilder},
    physical::{DefaultPhysicalPlanner, ExecutionPlan, FileTable, PhysicalPlanner},
};

fn test_insert_obj() {
    let plan: LogicalPlan = LogicalPlanBuilder::scan(
        ":mem:".to_string(),
        Arc::new(FileTable::try_new(
            PathBuf::from_str("/home/satyam/Downloads/MOTSChallenge/train/instances_txt/0009.txt")
                .unwrap(),
        )),
    )
    .filter_by_obj_id(2001)
    .build();
    let planner = DefaultPhysicalPlanner;
    let phys_plan: Arc<dyn ExecutionPlan + 'static> = planner.create_physical_plan(&plan);

    let iterator = phys_plan.execute();
    for x in iterator {
        println!("{:?}", x);
    }
}
