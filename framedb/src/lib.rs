use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::PathBuf,
};

use bincode::{Decode, Encode};
use bounding_box::BoundingBox;

use crate::planner::Atom;
pub mod planner;
pub mod query;
pub mod query_builder;
pub mod util;

pub type ObjID = u64;

#[derive(Debug, Clone, Encode, Decode)]
pub struct Trajectory(pub BTreeMap<u64, BoundingBox>);

impl Trajectory {
    fn clip_mut(&mut self, start: u64, end: u64) {}
    fn clip(&self, start: u64, end: u64) -> Trajectory {
        todo!()
    }
}

#[derive(Eq, Hash, PartialEq, PartialOrd, Ord, Clone, Copy, Debug, Encode, Decode)]
pub struct ClassId(pub u32);

#[derive(Debug, Clone, Encode, Decode)]
pub struct Metadata {
    pub class: HashSet<ClassId>,
}

pub enum StorageBackend {
    Mem,
    File(PathBuf),
}

pub struct FrameDB {
    backend: StorageBackend,
}

impl FrameDB {
    pub fn new(path: PathBuf) -> Self {
        let backend = if path.to_string_lossy() == ":mem:" {
            StorageBackend::Mem
        } else {
            StorageBackend::File(path)
        };
        Self { backend }
    }

    pub fn insert(
        &mut self,
        obj_id: ObjID,
        metadata: Option<Metadata>,
        bbox: BoundingBox,
        frame_id: u64,
    ) {
        todo!()
    }

    pub fn trajectory_of(&self, obj_id: u64) -> Trajectory {
        todo!()
    }

    pub fn trajectories_of(&self, class_id: ClassId) -> HashMap<ObjID, Trajectory> {
        todo!()
    }
}
