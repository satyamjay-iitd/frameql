use bincode::config::standard;
use bounding_box::BoundingBox;
use cocotools::coco::object_detection::Bbox;
use image::Rgb;
use imageproc::drawing::draw_hollow_rect_mut;
use imageproc::rect::Rect;

use std::collections::hash_map::DefaultHasher;
use uuid::Uuid;

use crate::ann_parser::{AnnFormat, parse_anns_from_file};
use crate::logical::LogicalPlan;
use crate::physical::{DefaultPhysicalPlanner, ExecutionPlan, PhysicalPlanner as _};
use crate::{Atom, ClassId, Metadata, Trajectory};

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs::{self, File, create_dir, create_dir_all, rename};
use std::hash::{Hash, Hasher as _};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ffmpeg::{
    codec,
    codec::encoder::video::Video as VideoEncoder,
    format,
    software::scaling::{context::Context as ScalingContext, flag::Flags},
    util::{frame::video::Video, rational::Rational},
};
use ffmpeg_next as ffmpeg;

pub fn draw_bboxes_on_frames(
    frame_paths: &Vec<std::path::PathBuf>,
    frame_id_to_bboxes: &std::collections::BTreeMap<u64, Vec<BoundingBox>>,
    output_dir: &Path,
) -> anyhow::Result<()> {
    fs::create_dir_all(output_dir)?;

    for (i, frame_path) in frame_paths.iter().enumerate() {
        let frame_id = i as u64;
        let mut img = image::open(frame_path).unwrap().to_rgb8();

        if let Some(bboxes) = frame_id_to_bboxes.get(&frame_id) {
            for bbox in bboxes {
                draw_hollow_rect_mut(
                    &mut img,
                    Rect::at(bbox.xmin().round() as i32, bbox.ymin().round() as i32)
                        .of_size(bbox.width().round() as u32, bbox.height().round() as u32),
                    Rgb([255, 0, 0]), // red box
                );
            }
            let out_path = output_dir.join(frame_path.file_name().unwrap());
            img.save(out_path)?;
        }
    }

    Ok(())
}

pub struct FrameDb {
    base_dir: PathBuf,
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct StoredVideoInfo {
    pub id: String,
    pub name: String,
    pub num_frames: u32,
    pub description: String,
}

impl FrameDb {
    pub fn new(base_dir: PathBuf) -> Self {
        FrameDb { base_dir }
    }

    pub fn ingest(
        &self,
        frame_dir: PathBuf,
        ann_format: AnnFormat,
        ann_file: String,
        title: String,
        desc: String,
    ) {
        let id = Uuid::new_v4().to_string();
        let sub_dir: PathBuf = [self.base_dir.clone(), PathBuf::from(id.clone())]
            .iter()
            .collect();
        create_dir(&sub_dir).unwrap();

        let frame_dir2 = sub_dir.clone().join("frames");
        create_dir(&frame_dir2).unwrap();

        rename(&frame_dir, &frame_dir2).unwrap();

        println!("Frames to video");
        self.frames_to_video(&frame_dir2, &sub_dir.join("video.mp4"), 30)
            .unwrap();

        let ann_path: PathBuf = [sub_dir.clone(), PathBuf::from("annotation")]
            .iter()
            .collect();

        let info = StoredVideoInfo {
            id,
            name: title,
            description: desc,
            num_frames: fs::read_dir(frame_dir2).unwrap().count() as u32,
        };
        let json = serde_json::to_string_pretty(&info).unwrap();

        let metadata_path: PathBuf = [sub_dir, PathBuf::from("metadata.json")].iter().collect();
        fs::write(&metadata_path, json).expect("Unable to write metadata.json");

        let anns: Vec<Annotation> = parse_anns_from_file(ann_format, ann_file).unwrap();
        write_bin(ann_path, &anns_to_atoms(anns)).unwrap();
    }

    pub fn query(&self, video_id: String, logical_plan: LogicalPlan) -> PathBuf {
        let mut hasher = DefaultHasher::new();
        video_id.hash(&mut hasher);
        logical_plan.hash(&mut hasher);
        let query_hash = hasher.finish().to_string();

        let sub_dir: PathBuf = [self.base_dir.clone(), PathBuf::from(video_id.clone())]
            .iter()
            .collect();
        let query_dir = sub_dir.join("query_results").join(query_hash);
        if !query_dir.exists() {
            create_dir_all(&query_dir).unwrap();
        }
        let frame_out_dir = query_dir.join("frames");
        let video_out_path = query_dir.join("video.mp4");

        if video_out_path.exists() {
            return video_out_path;
        }

        let planner = DefaultPhysicalPlanner;
        let phys_plan: Arc<dyn ExecutionPlan + 'static> =
            planner.create_physical_plan(&logical_plan);
        let iterator = phys_plan.execute();

        let frame_paths = self.sorted_frames(&video_id).unwrap();
        let mut frame_id_to_bboxes: BTreeMap<u64, Vec<BoundingBox>> = BTreeMap::new();
        for obj in iterator {
            let x: Atom = obj;
            for (frame_id, bbox) in x.trajectory.iter() {
                frame_id_to_bboxes
                    .entry(*frame_id)
                    .or_insert(vec![])
                    .push(bbox.clone());
            }
        }
        // let mut filtered_frame_paths = vec![];
        // for (idx, bboxes) in frame_id_to_bboxes.iter() {
        //     if !bboxes.is_empty() {
        //         filtered_frame_paths.push(frame_paths[*idx as usize].clone());
        //     }
        // }
        // println!("{:?}", filtered_frame_paths);
        draw_bboxes_on_frames(&frame_paths, &frame_id_to_bboxes, &frame_out_dir).unwrap();
        self.frames_to_video(&frame_out_dir, &video_out_path, 30)
            .unwrap();
        video_out_path
    }

    pub fn get_vid_info_by_id(&self, id: &str) -> Option<StoredVideoInfo> {
        let metadata_path = self.base_dir.join(id).join("metadata.json");
        let info: String = fs::read_to_string(metadata_path).ok()?;
        let info: StoredVideoInfo = serde_json::from_str(&info).unwrap();
        Some(info)
    }

    pub fn list_stored_vid(&self) -> Vec<StoredVideoInfo> {
        let mut vids = Vec::new();

        if let Ok(entries) = fs::read_dir(&self.base_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    let video_path = path.join("frames");
                    let metadata_path = path.join("metadata.json");
                    if video_path.exists() && metadata_path.exists() {
                        let info: String = fs::read_to_string(metadata_path).unwrap();
                        let info: StoredVideoInfo = serde_json::from_str(&info).unwrap();
                        if let Some(id) = path.file_name().and_then(|n| n.to_str()) {
                            assert_eq!(id, info.id);
                            vids.push(info);
                            // vids.push(id.to_string());
                        }
                    }
                }
            }
        }

        vids
    }

    pub fn list_query_vid(&self) -> Vec<String> {
        todo!()
    }

    pub fn get_stored_vid_by_id(&self, id: &str) -> Option<PathBuf> {
        let video_path = self.base_dir.join(id).join("video.mp4");
        if video_path.exists() {
            Some(video_path)
        } else {
            None
        }
    }

    pub fn thumbnail(&self, id: &str) -> Option<PathBuf> {
        fs::read_dir(self.base_dir.join(id).join("frames"))
            .ok()?
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .find(|p| {
                p.extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| matches!(ext, "jpg" | "jpeg" | "png"))
                    .unwrap_or(false)
            })
    }

    pub fn sorted_frames(&self, id: &str) -> Option<Vec<PathBuf>> {
        let mut frames: Vec<_> = fs::read_dir(self.base_dir.join(id).join("frames"))
            .ok()?
            .filter_map(|entry| entry.ok().map(|e| e.path()))
            .collect();

        frames.sort_by_key(|path| {
            path.file_stem()
                .and_then(|s| s.to_str())
                .and_then(|s| s.parse::<u32>().ok())
                .unwrap_or(u32::MAX)
        });

        Some(frames)
    }

    pub fn get_query_vid_by_name(&self, name: &str) -> PathBuf {
        todo!()
    }

    pub fn frames_to_video(
        &self,
        frames_dir: &Path,
        output_path: &Path,
        fps: u32,
    ) -> Result<(), ffmpeg::Error> {
        ffmpeg::init()?;

        // Collect sorted frames
        let mut frame_files: Vec<_> = fs::read_dir(frames_dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| {
                p.extension().map_or(false, |ext| {
                    matches!(ext.to_str(), Some("jpg" | "jpeg" | "png"))
                })
            })
            .collect();
        frame_files.sort();

        if frame_files.is_empty() {
            eprintln!("No frames found in {:?}", frames_dir);
            return Ok(());
        }

        // Open first frame
        let first = image::open(&frame_files[0]).unwrap().to_rgb8();
        let (width, height) = first.dimensions();
        // h264 wants even width and height
        let width = if width % 2 == 0 { width } else { width + 1 };
        let height = if height % 2 == 0 { height } else { height + 1 };

        // Output context
        let mut octx = format::output(output_path)?;
        let codec = ffmpeg::encoder::find(codec::Id::H264).ok_or(ffmpeg::Error::EncoderNotFound)?;
        let global = octx.format().flags().contains(format::Flags::GLOBAL_HEADER);
        let mut stream = octx.add_stream(codec)?;

        let time_base = Rational::new(1, fps as i32);
        // Configure encoder
        let mut enc_ctx = codec::context::Context::new_with_codec(codec)
            .encoder()
            .video()?;
        enc_ctx.set_width(width as u32);
        enc_ctx.set_height(height as u32);
        enc_ctx.set_format(ffmpeg::format::Pixel::YUV420P);
        enc_ctx.set_time_base(Rational::new(1, fps as i32));
        enc_ctx.set_frame_rate(Some(Rational::new(fps as i32, 1)));

        if global {
            enc_ctx.set_flags(codec::Flags::GLOBAL_HEADER);
        }

        let mut encoder = enc_ctx.open_as(codec)?;
        stream.set_parameters(&encoder);
        let stream_idx = stream.index();

        octx.write_header().unwrap();

        // Scaler: RGB → YUV420P
        let mut scaler = ScalingContext::get(
            ffmpeg::format::Pixel::RGB24,
            width as u32,
            height as u32,
            ffmpeg::format::Pixel::YUV420P,
            width as u32,
            height as u32,
            Flags::BILINEAR,
        )?;

        let mut pts = 0;
        let num_frames = frame_files.len();

        for (idx, img_path) in frame_files.into_iter().enumerate() {
            if idx % 10 == 0 {
                println!("Encoded {}/{} Frames", idx, num_frames);
            }
            let img = image::open(&img_path).unwrap().to_rgb8();
            let img_buf = img.as_raw();

            let mut rgb_frame =
                Video::new(ffmpeg::format::Pixel::RGB24, width as u32, height as u32);
            let bytes_per_row = (width * 3) as usize;
            let stride = rgb_frame.stride(0) as usize;
            let data = rgb_frame.data_mut(0);

            for (row_idx, row) in img_buf.chunks_exact(bytes_per_row).enumerate() {
                let start = row_idx * stride;
                let end = start + bytes_per_row;
                data[start..end].copy_from_slice(row);
            }

            // for (i, row) in img.rows().enumerate() {
            //     let start = i * stride as usize;
            //     let end = start + (width * 3) as usize;
            //     rgb_frame.data_mut(0)[start..end]
            //         .copy_from_slice(row.flat_map(|p| p.0).collect::<Vec<u8>>().as_slice());
            // }

            let mut yuv_frame = Video::empty();
            scaler.run(&rgb_frame, &mut yuv_frame)?;
            yuv_frame.set_pts(Some(pts));
            pts += 1;

            encoder.send_frame(&yuv_frame)?;

            let mut packet = ffmpeg::Packet::empty();
            while encoder.receive_packet(&mut packet).is_ok() {
                packet.set_stream(stream_idx);
                packet.rescale_ts(time_base, octx.stream(stream_idx).unwrap().time_base());
                packet.write_interleaved(&mut octx).unwrap();
            }
        }

        encoder.send_eof()?;
        let mut packet = ffmpeg::Packet::empty();
        while encoder.receive_packet(&mut packet).is_ok() {
            packet.set_stream(stream_idx);
            packet.rescale_ts(time_base, octx.stream(stream_idx).unwrap().time_base());
            packet.write_interleaved(&mut octx).unwrap();
        }

        octx.write_trailer()?;
        println!("Encoding Complete");
        Ok(())
    }

    pub fn get_ann_by_id(&self, id: &str) -> Vec<Atom> {
        let annotation_path = self.base_dir.join(id).join("annotation");
        read_bin(annotation_path)
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
        .map(|(id, (traj, meta))| Atom {
            obj_id: id,
            trajectory: traj,
            metadata: meta,
        })
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

pub fn read_bin(path: PathBuf) -> Vec<Atom> {
    let mut file = File::open(path).expect("Invalid Path");
    let mut atoms = Vec::new();

    loop {
        // Step 1: read the 8-byte length prefix
        let mut len_buf = [0u8; 8];
        if let Err(e) = file.read_exact(&mut len_buf) {
            if e.kind() == io::ErrorKind::UnexpectedEof {
                break; // reached end of file cleanly
            } else {
                panic!("Unable to decode");
            }
        }

        let len = u64::from_le_bytes(len_buf) as usize;

        // Step 2: read that many bytes
        let mut data_buf = vec![0u8; len];
        file.read_exact(&mut data_buf).expect("Unable to decode");

        // Step 3: decode
        let (decoded, _): (Atom, usize) =
            bincode::decode_from_slice(&data_buf, standard()).expect("Unable to decode");

        atoms.push(decoded);
    }

    atoms
}

#[derive(Debug)]
pub struct Annotation {
    pub time_frame: u64,
    pub object_id: u64,
    pub class_id: ClassId,
    pub bbox: Bbox,
}
