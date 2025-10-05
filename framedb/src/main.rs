use std::{
    collections::{BTreeMap, HashMap, HashSet},
    io,
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};

use bounding_box::BoundingBox;
use eframe::egui;
use framedb::{
    ClassId, Metadata, Trajectory,
    logical::{LogicalPlan, LogicalPlanBuilder, MetadataFilter},
    physical::{DefaultPhysicalPlanner, ExecutionPlan, FileTable, PhysicalPlanner as _},
    server::server,
    util::{read_annotations_from_file, write_bin},
};

use crate::dashboard::MyApp;

mod dashboard;

#[tokio::main]
async fn main() -> io::Result<()> {
    server().await;
    // let options = eframe::NativeOptions {
    //     viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
    //     ..Default::default()
    // };
    // eframe::run_native(
    //     "Multiple viewports",
    //     options,
    //     Box::new(|_cc| Ok(Box::<MyApp>::default())),
    // )
    // .unwrap();
    // let annotations = read_annotations_from_file(
    //     "/home/satyam/Downloads/MOTSChallenge/train/instances_txt/0002.txt",
    // )?;

    // let mut trajectory: HashMap<u64, (Trajectory, Metadata)> = HashMap::new();
    // for ann in annotations {
    //     trajectory
    //         .entry(ann.object_id)
    //         .or_insert((
    //             Trajectory(BTreeMap::new()),
    //             Metadata {
    //                 class: HashSet::from([ann.class_id]),
    //             },
    //         ))
    //         .0
    //         .0
    //         .insert(
    //             ann.time_frame,
    //             BoundingBox::new(
    //                 ann.bbox.left,
    //                 ann.bbox.left + ann.bbox.width,
    //                 ann.bbox.top,
    //                 ann.bbox.top + ann.bbox.height,
    //             ),
    //         );
    // }

    // let atoms: Vec<_> = trajectory
    //     .into_iter()
    //     .map(|(id, (traj, meta))| Atom(id, traj, meta))
    //     .collect();

    // write_bin("/tmp/framedb", &atoms).unwrap();

    // let plan: LogicalPlan = LogicalPlanBuilder::scan(
    //     "MOT".to_string(),
    //     Arc::new(FileTable::try_new(
    //         PathBuf::from_str("/tmp/framedb").unwrap(),
    //     )),
    // )
    // .filter_by_obj_id(2015)
    // .filter_by_metadata(MetadataFilter::IsA(ClassId(2)))
    // .build();
    // let planner = DefaultPhysicalPlanner;
    // let phys_plan: Arc<dyn ExecutionPlan + 'static> = planner.create_physical_plan(&plan);

    // let iterator = phys_plan.execute();
    // phys_plan
    //     .visualize("/home/satyam/Downloads/MOTSChallenge/train/images/0011/".into())
    //     .unwrap();

    // for x in iterator {
    //     x.plot("/tmp/2104.jpg", 1920, 1080).unwrap();
    //     break;
    // }

    Ok(())
}
