import os
import cv2
import numpy as np
from pycocotools import mask as maskUtils
from tqdm import tqdm

# path to your annotation file
anno_file = "/home/satyam/Downloads/MOTSChallenge/train/instances_txt/0002.txt"
image_dir = "/home/satyam/Downloads/MOTSChallenge/train/images/0002/"
colors = {}  # remember a color for each obj_id


tracks = []
with open(anno_file, "r") as f:
    for line in f:
        if not line.strip():
            continue
        time_frame, obj_id, class_id, h, w, rle = line.strip().split(maxsplit=5)
        if obj_id[0] == '2':
            tracks.append({
                "frame": int(time_frame),
                "obj_id": int(obj_id),
                "class_id": int(class_id),
                "height": int(h),
                "width": int(w),
                "rle": rle,
            })



def decode_rle(rle_str, height, width):
    # pycocotools expects {"size": [h,w], "counts": rle_bytes}
    rle = {
        "size": [height, width],
        "counts": rle_str.encode("utf-8")
    }
    mask = maskUtils.decode(rle)  # shape (h,w,1)
    return mask.astype(np.uint8) * 255



trajectories = {}

# Create a black canvas same size as your frames
canvas_h, canvas_w = tracks[0]["height"], tracks[0]["width"]
traj_canvas = np.zeros((canvas_h, canvas_w, 3), dtype=np.uint8)

for ann in tqdm(tracks):
    mask = decode_rle(ann["rle"], ann["height"], ann["width"])

    # Compute bounding box
    ys, xs = np.where(mask > 0)
    if len(xs) == 0 or len(ys) == 0:
        continue
    x1, y1, x2, y2 = xs.min(), ys.min(), xs.max(), ys.max()

    # Compute centroid of bbox
    cx = int((x1 + x2) / 2)
    cy = int((y1 + y2) / 2)

    # Add to trajectory dictionary
    obj_id = ann["obj_id"]
    if obj_id != 2002:
        continue
    if obj_id not in trajectories:
        trajectories[obj_id] = []
    trajectories[obj_id].append((cx, cy))

    # Draw trajectory so far for this object
    color = tuple(int(c) for c in np.random.randint(0, 255, size=3))
    pts = trajectories[obj_id]
    for i in range(1, len(pts)):
        cv2.line(traj_canvas, pts[i - 1], pts[i], color, 2)

# Show trajectory canvas
cv2.imshow("Trajectories", traj_canvas)
cv2.waitKey(0)
cv2.destroyAllWindows()
out_file = "trajectories.png"
cv2.imwrite(out_file, traj_canvas)
print(f"Saved trajectories to {out_file}")

# for ann in tracks:
#     frame_file = os.path.join(image_dir, f"{ann['frame']:06d}.jpg")
#     img = cv2.imread(frame_file)
#     if img is None:
#         continue

#     mask = decode_rle(ann["rle"], ann["height"], ann["width"])

#     # assign random color per object
#     if ann["obj_id"] not in colors:
#         colors[ann["obj_id"]] = np.random.randint(0, 255, size=3).tolist()

#     color_float = np.array(colors[ann["obj_id"]], dtype=np.float32)
#     color_bgr = tuple(int(c) for c in colors[ann["obj_id"]])

#     # overlay mask
#     img_masked = img[mask > 0].astype(np.float32)
#     img[mask > 0] = (0.7 * img_masked + 0.3 * color_float).astype(np.uint8)

#     # bounding box
#     ys, xs = np.where(mask > 0)
#     if len(xs) > 0 and len(ys) > 0:
#         x1, y1, x2, y2 = xs.min(), ys.min(), xs.max(), ys.max()
#         cv2.rectangle(img, (x1, y1), (x2, y2), color_bgr, 2)
#         cv2.putText(img, f"ID {ann['obj_id']}", (x1, y1 - 5),
#                     cv2.FONT_HERSHEY_SIMPLEX, 0.5, color_bgr, 1)

#     cv2.imshow("Tracks", img)
#     if cv2.waitKey(30) & 0xFF == ord("q"):
#         break

# cv2.destroyAllWindows()

