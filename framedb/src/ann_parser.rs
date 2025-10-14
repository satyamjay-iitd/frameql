use std::{
    fs::File,
    io::{self, BufRead as _},
    path::Path,
};

use cocotools::coco::object_detection::{Bbox, CocoRle, Rle};
use strum_macros::EnumString;

use crate::{ClassId, ingest::Annotation};

#[derive(EnumString, Debug)]
#[strum(serialize_all = "UPPERCASE")] // matches KPF, COCO, DIVE
pub enum AnnFormat {
    COCO,
    DIVE,
    KPF,
}

fn parse_coco_ann<P: AsRef<Path>>(filename: P) -> io::Result<Vec<Annotation>> {
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

pub fn parse_anns_from_file<P: AsRef<Path>>(
    ann_type: AnnFormat,
    filename: P,
) -> io::Result<Vec<Annotation>> {
    match ann_type {
        AnnFormat::COCO => parse_coco_ann(filename),
        AnnFormat::DIVE => todo!(),
        AnnFormat::KPF => todo!(),
    }
}
