import os
import cv2
import numpy as np
from tqdm import tqdm
import json
from pycocotools import mask as maskUtils

anno_file = "/home/satyam/Downloads/MOTSChallenge/train/instances_txt/0002.txt"
image_dir = "/home/satyam/Downloads/MOTSChallenge/train/images/0002/"

def decode_rle(rle_str, height, width):
    # pycocotools expects {"size": [h,w], "counts": rle_bytes}
    rle = {
        "size": [height, width],
        "counts": rle_str.encode("utf-8")
    }
    mask = maskUtils.decode(rle)  # shape (h,w,1)
    return mask.astype(np.uint8) * 255

tracks_file = "tracks.json"

if os.path.exists(tracks_file):
    with open(tracks_file, "r") as f:
        tracks = json.load(f)

    # convert back to int keys and tuples if needed
    tracks = {
        int(obj_id): [(frame_id, (x, y)) for frame_id, (x, y) in points]
        for obj_id, points in tracks.items()
    }
    print("Loaded tracks from file")
else:
    tracks = {}
    with open(anno_file, "r") as f:
        for line in tqdm(f):
            if not line.strip():
                continue
            time_frame, obj_id, class_id, h, w, rle = line.strip().split(maxsplit=5)
            frame_id = int(time_frame)
            obj_id = int(obj_id)
            class_id = int(class_id)
            h, w = int(h), int(w)

            # Decode mask from RLE (optional: if you want centroids, not just bounding boxes)
            # Simple centroid using RLE string:
            mask = decode_rle(rle, h, w)

            ys, xs = np.where(mask > 0)
            if len(xs) == 0 or len(ys) == 0:
                continue
            x1, y1, x2, y2 = xs.min(), ys.min(), xs.max(), ys.max()

            # Compute centroid of bbox
            cx = int((x1 + x2) / 2)
            cy = int((y1 + y2) / 2)
            tracks.setdefault(obj_id, []).append((frame_id, (cx, cy)))

    with open(tracks.json, "w") as f:
        json.dump(tracks, f, indent=2)


# Sort frames
image_files = sorted(
    [f for f in os.listdir(image_dir) if f.lower().endswith(".jpg")],
    key=lambda x: int(os.path.splitext(x)[0])
)
image_files = sorted(image_files, key=lambda x: int(os.path.splitext(x)[0]))


# Display frames as video
for frame_idx, img_name in enumerate(image_files, start=1):
    img_path = os.path.join(image_dir, img_name)
    frame = cv2.imread(img_path)

    # Draw trajectories up to this frame
    for obj_id, points in tracks.items():
        # Filter up to current frame
        past_points = [(x, y) for f, (x, y) in points if f <= frame_idx]
        if len(past_points) > 1:
            pts = np.array(past_points, dtype=np.int32).reshape((-1, 1, 2))
            cv2.polylines(frame, [pts], isClosed=False, color=(0, 255, 0), thickness=2)
            # Draw latest position
            cv2.circle(frame, past_points[-1], 4, (0, 0, 255), -1)
            cv2.putText(frame, f"ID {obj_id}", past_points[-1], cv2.FONT_HERSHEY_SIMPLEX, 
                        0.5, (255, 255, 255), 1)

    cv2.imshow("Tracks", frame)
    key = cv2.waitKey(30)  # ~30 fps
    if key == 27:  # ESC to quit
        break

cv2.destroyAllWindows()

