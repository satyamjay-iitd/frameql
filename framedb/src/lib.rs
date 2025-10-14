use std::{
    collections::{BTreeMap, HashSet},
    hash::Hash,
};

use bincode::{Decode, Encode};
use bounding_box::BoundingBox;
use plotters::prelude::*;

pub mod ann_parser;
// pub mod dashboard;
pub mod ingest;
pub mod logical;
pub mod ltl;
pub mod physical;
pub mod server;
pub mod util;

pub type ObjID = u64;

#[derive(Debug, Clone, Encode, Decode)]
pub struct Trajectory(pub BTreeMap<u64, BoundingBox>);

impl std::hash::Hash for Trajectory {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.iter().map(|(k, v)| {
            k.hash(state);
            (v.xmin() as u32).hash(state);
            (v.ymin() as u32).hash(state);
            (v.xmax() as u32).hash(state);
            (v.ymax() as u32).hash(state);
        });
    }
}

impl Trajectory {
    pub fn clip_mut(&mut self, _start: u64, _end: u64) {
        todo!()
    }
    pub fn clip(&self, _start: u64, _end: u64) -> Trajectory {
        todo!()
    }

    fn range(&self) -> (u64, u64) {
        (
            *self.0.first_key_value().unwrap().0,
            *self.0.last_key_value().unwrap().0,
        )
    }

    pub fn iter(&self) -> impl Iterator<Item = (&u64, &BoundingBox)> {
        self.0.iter()
    }
}

#[derive(Eq, Hash, PartialEq, PartialOrd, Ord, Clone, Copy, Debug, Encode, Decode)]
pub struct ClassId(pub u32);

#[derive(Debug, Clone, Encode, Decode)]
pub struct Metadata {
    pub class: HashSet<ClassId>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct Atom {
    pub obj_id: u64,
    pub trajectory: Trajectory,
    pub metadata: Metadata,
}

impl Atom {
    pub fn plot(
        &self,
        output_path: &str,
        img_w: u32,
        img_h: u32,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let Atom {
            obj_id,
            trajectory: Trajectory(bboxes),
            ..
        } = self;

        // Create drawing area
        let root = BitMapBackend::new(output_path, (img_w, img_h)).into_drawing_area();
        root.fill(&WHITE)?;

        let mut chart = ChartBuilder::on(&root)
            .caption(
                format!("Trajectory of Atom ID {}", obj_id),
                ("sans-serif", 30),
            )
            .margin(20)
            .x_label_area_size(30)
            .y_label_area_size(30)
            .build_cartesian_2d(0f64..1920f64, 0f64..1080f64)?;

        chart.configure_mesh().draw()?;

        // Plot each bounding box
        // for (_frame, bbox) in bboxes {
        //     chart.draw_series(std::iter::once(Rectangle::new(
        //         [(bbox.xmin(), bbox.ymin()), (bbox.xmax(), bbox.ymax())],
        //         ShapeStyle::from(&RED).stroke_width(1),
        //     )))?;
        // }
        for bbox in bboxes.values() {
            let center_x = (bbox.xmin() + bbox.xmax()) / 2.0;
            let center_y = (bbox.ymin() + bbox.ymax()) / 2.0;

            chart.draw_series(std::iter::once(
                Circle::new((center_x, center_y), 2, RED.filled()), // radius=2
            ))?;
        }

        // Save to file
        root.present()?;
        Ok(())
    }
}
