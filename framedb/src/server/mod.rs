use axum::{Router, extract::Extension, extract::Multipart, response::IntoResponse, routing::post};
use infer::archive::is_zip;
use infer::is_video;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_http::cors::CorsLayer;
use uuid::Uuid;

use crate::ingest::FrameDb;

pub type SharedIngestor = Arc<Mutex<FrameDb>>;

async fn upload_handler(
    Extension(db_ingestor): Extension<SharedIngestor>,
    mut multipart: Multipart,
) -> impl IntoResponse {
    let mut video_file = PathBuf::new();
    let mut ann_file = String::new();
    let mut title = String::new();
    let mut description = String::new();
    // let mut tags = vec![];

    while let Some(field) = multipart.next_field().await.unwrap() {
        let name = field.name().unwrap_or("").to_string();

        match name.as_str() {
            "video" => {
                video_file = PathBuf::from(format!("/tmp/uploads/{}_video.mp4", Uuid::new_v4()));
                let video_data = field.bytes().await.unwrap();
                if is_zip(&video_data) {
                } else if is_video(&video_data) {
                } else {
                    panic!("");
                }

                // let mut file = File::create(&video_file).unwrap();
                // file.write_all(&video_data).unwrap();
            }

            "annotations" => {
                ann_file = format!("/tmp/uploads/{}_annotations.txt", Uuid::new_v4());
                let ann_data = field.bytes().await.unwrap();

                let mut file = File::create(&ann_file).unwrap();
                file.write_all(&ann_data).unwrap();
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
    let db_ingestor = db_ingestor.lock().await;
    db_ingestor.ingest(video_file, ann_file, title, description);
    "Upload successful"
}

pub async fn server() {
    let db_ingestor = Arc::new(Mutex::new(FrameDb::new(PathBuf::from("/tmp/uploads"))));
    let app = Router::new()
        .route("/upload", post(upload_handler))
        .layer(Extension(db_ingestor))
        .layer(CorsLayer::permissive());

    println!("Server running at http://0.0.0.0:3000");
    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:3000"))
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
