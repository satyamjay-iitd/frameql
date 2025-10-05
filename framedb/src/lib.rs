use std::collections::{BTreeMap, HashSet};

use bincode::{Decode, Encode};
use bounding_box::BoundingBox;
use plotters::prelude::*;

pub mod dashboard;
pub mod ingest;
pub mod logical;
pub mod physical;
pub mod server;
pub mod util;

pub type ObjID = u64;

#[derive(Debug, Clone, Encode, Decode)]
pub struct Trajectory(pub BTreeMap<u64, BoundingBox>);

impl Trajectory {
    pub fn clip_mut(&mut self, _start: u64, _end: u64) {
        todo!()
    }
    pub fn clip(&self, _start: u64, _end: u64) -> Trajectory {
        todo!()
    }
}

#[derive(Eq, Hash, PartialEq, PartialOrd, Ord, Clone, Copy, Debug, Encode, Decode)]
pub struct ClassId(pub u32);

#[derive(Debug, Clone, Encode, Decode)]
pub struct Metadata {
    pub class: HashSet<ClassId>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct Atom(pub u64, pub Trajectory, pub Metadata);

impl Atom {
    pub fn plot(
        &self,
        output_path: &str,
        img_w: u32,
        img_h: u32,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let Atom(id, Trajectory(bboxes), _) = self;

        // Create drawing area
        let root = BitMapBackend::new(output_path, (img_w, img_h)).into_drawing_area();
        root.fill(&WHITE)?;

        let mut chart = ChartBuilder::on(&root)
            .caption(format!("Trajectory of Atom ID {}", id), ("sans-serif", 30))
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
        for (_frame, bbox) in bboxes {
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
