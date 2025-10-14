extern crate ffmpeg_next as ffmpeg;
use axum::Json;
use axum::extract::{DefaultBodyLimit, Path as UrlPath, State};
use axum::http::{HeaderMap, StatusCode, header};
use axum::response::Response;
use axum::routing::get;
use axum::{Router, extract::Multipart, response::IntoResponse, routing::post};
use image::{ImageBuffer, Rgb};
use infer::MatcherType;
use serde::Serialize;
use std::fs::File;
use std::io::{Read as _, Seek as _, SeekFrom};
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use tokio::io::AsyncWriteExt as _;
use tokio::sync::Mutex;
use tower_http::cors::{Any, CorsLayer};
use utoipa::{OpenApi, ToSchema};
use utoipa_swagger_ui::SwaggerUi;
use uuid::Uuid;

use ffmpeg::format::{Pixel, input};
use ffmpeg::media::Type;
use ffmpeg::software::scaling::{context::Context, flag::Flags};
use ffmpeg::util::frame::video::Video;
use std::path::Path;

use crate::ann_parser::AnnFormat;
use crate::ingest::{FrameDb, StoredVideoInfo};
use crate::logical::plan_from_str;

pub type SharedIngestor = Arc<Mutex<FrameDb>>;

use std::fs;
use zip::ZipArchive;

fn extract_zip_to_tmp(path: &Path) -> std::io::Result<PathBuf> {
    let tmp_dir = tempfile::tempdir().unwrap();
    let file = File::open(path).unwrap();
    let mut archive = ZipArchive::new(file).unwrap();

    for i in 0..archive.len() {
        let mut file = archive.by_index(i).unwrap();
        let out_path = tmp_dir.path().join(file.name());

        if let Some(ext) = out_path.extension().and_then(|e| e.to_str()) {
            if ext.eq_ignore_ascii_case("jpg") || ext.eq_ignore_ascii_case("png") {
                let mut out_file =
                    fs::File::create(&out_path).expect("Zipfile must not contain any directory.");
                std::io::copy(&mut file, &mut out_file).unwrap();
            }
        }
    }

    Ok(tmp_dir.keep())
}

fn extract_frames_from_video(video_path: &Path) -> std::io::Result<PathBuf> {
    // initialize ffmpeg
    ffmpeg::init()?;

    let tmp_dir = video_path.parent().unwrap();
    // create a temp dir for frames
    // let tmp_dir = tempfile::tempdir()?.keep();
    let frames_dir = tmp_dir.join("frames");
    if frames_dir.exists() {
        fs::remove_dir_all(&frames_dir)?; // deletes directory and everything inside
    }
    fs::create_dir(&frames_dir)?;

    // write uploaded video bytes to a temp file
    // let video_path = tmp_dir.join("input.mp4");
    // fs::write(&video_path, video_data)?;

    // open video input
    let mut ictx = input(&video_path)?;
    let input_stream = ictx
        .streams()
        .best(Type::Video)
        .ok_or(ffmpeg::Error::StreamNotFound)?;
    let video_stream_index = input_stream.index();

    let context_decoder =
        ffmpeg::codec::context::Context::from_parameters(input_stream.parameters())?;
    let mut decoder = context_decoder.decoder().video()?;

    // setup scaler to convert pixel format
    let mut scaler = Context::get(
        decoder.format(),
        decoder.width(),
        decoder.height(),
        Pixel::RGB24,
        decoder.width(),
        decoder.height(),
        Flags::BILINEAR,
    )?;

    let mut frame_index = 0usize;

    let mut receive_and_process_decoded_frames =
        |decoder: &mut ffmpeg::decoder::Video| -> Result<(), ffmpeg::Error> {
            let mut decoded = Video::empty();
            while decoder.receive_frame(&mut decoded).is_ok() {
                let mut rgb_frame = Video::empty();
                scaler.run(&decoded, &mut rgb_frame)?;

                save_frame(&rgb_frame, &frames_dir, frame_index).unwrap();
                frame_index += 1;
            }
            Ok(())
        };

    // iterate over all packets
    for (stream, packet) in ictx.packets() {
        if stream.index() == video_stream_index {
            decoder.send_packet(&packet)?;
            receive_and_process_decoded_frames(&mut decoder)?;
        }
    }

    decoder.send_eof()?;
    receive_and_process_decoded_frames(&mut decoder)?;
    Ok(frames_dir)
}

fn save_frame(frame: &Video, frames_dir: &PathBuf, index: usize) -> std::io::Result<()> {
    let (w, h) = (frame.width(), frame.height());
    let buf = ImageBuffer::<Rgb<u8>, _>::from_raw(w, h, frame.data(0).to_vec()).unwrap();
    let frame_path = frames_dir.join(format!("frame_{:05}.png", index));
    buf.save(frame_path.clone()).unwrap();
    Ok(())
}

async fn upload_handler(
    State(db): State<Arc<Mutex<FrameDb>>>,
    mut multipart: Multipart,
) -> impl IntoResponse {
    let mut frame_dir = PathBuf::new();
    let mut ann_file = String::new();
    let mut title = String::new();
    let mut description = String::new();
    let mut ann_format = "NULL".to_string();

    while let Some(mut field) = multipart.next_field().await.unwrap() {
        let name = field.name().unwrap_or("").to_string();

        match name.as_str() {
            "video" => {
                let file_path: PathBuf =
                    format!("/tmp/{}_video.framedb.part", Uuid::new_v4()).into();
                let mut file = tokio::fs::File::create(&file_path).await.unwrap();
                while let Some(chunk) = field.chunk().await.unwrap() {
                    file.write_all(&chunk).await.unwrap();
                }

                let frames_dir2 = if infer::get_from_path(&file_path)
                    .unwrap()
                    .unwrap()
                    .mime_type()
                    == "application/zip"
                {
                    println!("Extracting frame");
                    extract_zip_to_tmp(&file_path).unwrap()
                } else if infer::get_from_path(&file_path)
                    .unwrap()
                    .unwrap()
                    .matcher_type()
                    == MatcherType::Video
                {
                    println!("Decoding video");
                    extract_frames_from_video(&file_path).unwrap()
                } else {
                    panic!("Unsupported file type");
                };
                frame_dir = frames_dir2;
            }

            "ann_format" => {
                ann_format = field.text().await.unwrap();
            }

            "annotations" => {
                ann_file = format!("/tmp/uploads/{}_annotations.txt", Uuid::new_v4());
                let mut file = tokio::fs::File::create(&ann_file).await.unwrap();

                while let Some(chunk) = field.chunk().await.unwrap() {
                    file.write_all(&chunk).await.unwrap();
                }
            }

            "title" => {
                title = field.text().await.unwrap();
            }

            "desc" => {
                description = field.text().await.unwrap();
            }

            // "tags" => {
            //     tags = field.text().await.unwrap_or_default();
            // }
            _ => {
                println!("Unknown field: {}", name);
            }
        }
    }

    let ann_format = AnnFormat::from_str(&ann_format).unwrap();
    let db_ingestor = db.lock().await;
    db_ingestor.ingest(frame_dir, ann_format, ann_file, title, description);
    "Upload successful"
}

async fn serve_video(
    UrlPath(name): UrlPath<String>,
    State(db): State<Arc<Mutex<FrameDb>>>,
    headers: HeaderMap,
) -> impl IntoResponse {
    let db = db.lock().await;
    let path = db.get_stored_vid_by_id(&name).unwrap();
    match send_video_file(&path, &headers) {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("Error: {:?}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Failed to read video").into_response()
        }
    }
}

fn send_video_file(path: &Path, headers: &HeaderMap) -> std::io::Result<Response> {
    let mut file = File::open(path)?;
    let metadata = file.metadata()?;
    let file_size = metadata.len();

    // Parse Range header
    let range = headers
        .get(header::RANGE)
        .and_then(|r| r.to_str().ok())
        .unwrap_or("");

    let (start, end) = if let Some(range) = range.strip_prefix("bytes=") {
        let parts: Vec<&str> = range.split('-').collect();
        let start: u64 = parts[0].parse().unwrap_or(0);
        let end: u64 = parts
            .get(1)
            .and_then(|s| s.parse().ok())
            .unwrap_or(file_size - 1);
        (start, end.min(file_size - 1))
    } else {
        (0, file_size - 1)
    };

    let chunk_size = end - start + 1;
    let mut buffer = vec![0; chunk_size as usize];
    file.seek(SeekFrom::Start(start))?;
    file.read_exact(&mut buffer)?;

    // Build response
    let mime = infer::get_from_path(path).unwrap().unwrap().mime_type();

    let response = Response::builder()
        .status(if start > 0 {
            StatusCode::PARTIAL_CONTENT
        } else {
            StatusCode::OK
        })
        .header(header::CONTENT_TYPE, mime)
        .header(header::CONTENT_LENGTH, chunk_size.to_string())
        .header(
            header::CONTENT_RANGE,
            format!("bytes {}-{}/{}", start, end, file_size),
        )
        .body(buffer.into())
        .unwrap();

    Ok(response)
}

#[derive(Serialize, ToSchema)]
struct VideoInfo {
    id: String,
    name: String,
    path: String,
    thumbnail_url: String,
}

#[utoipa::path(
    get,
    path = "/video",
    responses(
        (status = 200, description = "Status of the node", body = Vec<VideoInfo>)
    )
)]
pub async fn list_video(State(db): State<Arc<Mutex<FrameDb>>>) -> impl IntoResponse {
    let db = db.lock().await;

    let videos: Vec<StoredVideoInfo> = db.list_stored_vid();

    let result: Vec<VideoInfo> = videos
        .into_iter()
        .map(|i| VideoInfo {
            id: i.id.clone(),
            path: format!("/video/{}", i.id),
            thumbnail_url: format!("/thumbnail/{}", i.id),
            name: i.name,
        })
        .collect();

    Json(result)
}

pub async fn serve_thumbnail(
    UrlPath(name): UrlPath<String>,
    State(db): State<Arc<Mutex<FrameDb>>>,
) -> impl IntoResponse {
    let not_found_response = Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(axum::body::Body::from("Thumbnail not found"))
        .unwrap();

    match db
        .lock()
        .await
        .thumbnail(&name)
        .and_then(|path| Some((path.clone(), tokio::fs::read(path))))
    {
        Some((path, fut)) => match fut.await {
            Ok(bytes) => Response::builder()
                .status(StatusCode::OK)
                .header(
                    header::CONTENT_TYPE,
                    infer::get_from_path(&path).unwrap().unwrap().mime_type(),
                )
                .body(axum::body::Body::from(bytes))
                .unwrap(),
            Err(_) => not_found_response,
        },
        None => not_found_response,
    }
}

async fn query(
    UrlPath((video_id, query_str)): UrlPath<(String, String)>,
    State(db): State<Arc<Mutex<FrameDb>>>,
    headers: HeaderMap,
) -> impl IntoResponse {
    let db = db.lock().await;
    let ann = db.get_ann_by_id(&video_id);
    let logical_plan = plan_from_str(&query_str, ann);
    let out_vid_path = db.query(video_id, logical_plan);
    match send_video_file(&out_vid_path, &headers) {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("Error: {:?}", e);
            (StatusCode::INTERNAL_SERVER_ERROR, "Failed to read video").into_response()
        }
    }
}

#[derive(OpenApi)]
#[openapi(paths(list_video))]
struct ApiDoc;

pub async fn server() {
    let db_ingestor = Arc::new(Mutex::new(FrameDb::new(PathBuf::from("/tmp/uploads"))));
    let app = Router::new()
        .route("/upload", post(upload_handler))
        .route("/video", get(list_video))
        .route("/video/{name}", get(serve_video))
        .route("/thumbnail/{name}", get(serve_thumbnail))
        .route("/query/{video_id}/{query_str}", get(query))
        .with_state(db_ingestor)
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any),
        )
        .layer(DefaultBodyLimit::max(1024 * 1024 * 1024));

    let app = app.merge(SwaggerUi::new("/docs").url("/api-doc/openapi.json", ApiDoc::openapi()));

    println!("Server running at http://0.0.0.0:3000");
    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:3000"))
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
