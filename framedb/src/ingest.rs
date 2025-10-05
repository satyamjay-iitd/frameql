use bounding_box::BoundingBox;
use cocotools::coco::object_detection::{Bbox, CocoRle, Rle};
use uuid::Uuid;

use crate::logical::LogicalPlan;
use crate::physical::{DefaultPhysicalPlanner, ExecutionPlan, PhysicalPlanner as _};
use crate::{Atom, ClassId, Metadata, Trajectory};

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs::{self, File, create_dir, rename};
use std::io::Write;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub struct FrameDb {
    base_dir: PathBuf,
}

impl FrameDb {
    pub fn new(dir: PathBuf) -> Self {
        FrameDb { base_dir: dir }
    }

    pub fn ingest(&self, video_file: PathBuf, ann_file: String, title: String, desc: String) {
        let id = Uuid::new_v4().to_string();
        let sub_dir: PathBuf = [self.base_dir.clone(), PathBuf::from(id)].iter().collect();
        create_dir(sub_dir.clone()).unwrap();

        // Move file
        let video_path: PathBuf = [sub_dir.clone(), PathBuf::from("video.mp4")]
            .iter()
            .collect();
        rename(video_file, video_path).unwrap();

        let ann_path: PathBuf = [sub_dir.clone(), PathBuf::from("annotation")]
            .iter()
            .collect();

        let mut metadata: HashMap<String, String> = HashMap::new();
        metadata.insert("title".to_string(), title);
        metadata.insert("description".to_string(), desc);
        let json = serde_json::to_string_pretty(&metadata).unwrap();

        let metadata_path: PathBuf = [sub_dir, PathBuf::from("metadata.json")].iter().collect();
        fs::write(&metadata_path, json).expect("Unable to write metadata.json");

        write_bin(
            ann_path,
            &anns_to_atoms(read_annotations_from_file(ann_file).unwrap()),
        )
        .unwrap();
    }

    pub fn query(&self, logical_plan: LogicalPlan) {
        let planner = DefaultPhysicalPlanner;
        let phys_plan: Arc<dyn ExecutionPlan + 'static> =
            planner.create_physical_plan(&logical_plan);
        let iterator = phys_plan.execute();
    }
}

pub fn anns_to_atoms(annotations: Vec<Annotation>) -> Vec<Atom> {
    let mut trajectory: HashMap<u64, (Trajectory, Metadata)> = HashMap::new();
    for ann in annotations {
        trajectory
            .entry(ann.object_id)
            .or_insert((
                Trajectory(BTreeMap::new()),
                Metadata {
                    class: HashSet::from([ann.class_id]),
                },
            ))
            .0
            .0
            .insert(
                ann.time_frame,
                BoundingBox::new(
                    ann.bbox.left,
                    ann.bbox.left + ann.bbox.width,
                    ann.bbox.top,
                    ann.bbox.top + ann.bbox.height,
                ),
            );
    }

    trajectory
        .into_iter()
        .map(|(id, (traj, meta))| Atom(id, traj, meta))
        .collect()
}

pub fn write_bin(path: PathBuf, data: &[Atom]) -> std::io::Result<()> {
    let mut file = File::create(path)?;
    for item in data {
        let encoded = bincode::encode_to_vec(item, bincode::config::standard()).unwrap();
        let len = encoded.len() as u64;
        file.write_all(&len.to_le_bytes())?;
        file.write_all(&encoded)?;
    }
    Ok(())
}

#[derive(Debug)]
pub struct Annotation {
    pub time_frame: u64,
    pub object_id: u64,
    pub class_id: ClassId,
    pub bbox: Bbox,
}

fn parse_annotation_line(line: &str) -> Option<Annotation> {
    // Split the line by whitespace into at most 6 parts
    let mut parts = line.splitn(6, ' ');

    let time_frame = parts.next()?.parse().ok()?;
    let object_id = parts.next()?.parse().ok()?;
    let class_id = ClassId(parts.next()?.parse().ok()?);
    let img_height = parts.next()?.parse().ok()?;
    let img_width = parts.next()?.parse().ok()?;
    let rle: String = parts.next()?.to_string();
    let rle: CocoRle = CocoRle {
        size: vec![img_height, img_width],
        counts: rle,
    };
    let rle: Rle = (&rle).into();
    let bbox: Bbox = (&rle).into();

    Some(Annotation {
        time_frame,
        object_id,
        class_id,
        bbox,
    })
}

pub fn read_annotations_from_file<P: AsRef<Path>>(filename: P) -> io::Result<Vec<Annotation>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);

    let mut annotations = Vec::new();

    for line_result in reader.lines() {
        let line = line_result?;
        if let Some(annotation) = parse_annotation_line(&line) {
            annotations.push(annotation);
        }
    }

    Ok(annotations)
}
