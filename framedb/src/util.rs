use cocotools::coco::object_detection::{Bbox, CocoRle, Rle};

use crate::{Atom, ClassId};
use std::fs::File;
use std::io::Write;
use std::io::{self, BufRead};
use std::path::Path;

pub fn write_bin(path: &str, data: &[Atom]) -> std::io::Result<()> {
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
