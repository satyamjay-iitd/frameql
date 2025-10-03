from collections import defaultdict

import glob
import os
import cv2
import numpy as np

from ultralytics import YOLO

# Load the YOLO11 model
model = YOLO("yolo11l.pt")

# Open the video file
image_dir = "/home/satyam/Downloads/MOTSChallenge/train/images/0002/"
image_paths = sorted(glob.glob(os.path.join(image_dir, "*.jpg")))

# Store the track history
track_history = defaultdict(lambda: [])

# Loop through the video frames
for img_path in image_paths:
    # Read a frame from the video
    frame = cv2.imread(img_path)

    # Run YOLO11 tracking on the frame, persisting tracks between frames
    result = model.track(frame, persist=True)[0]

    # Get the boxes and track IDs
    if result.boxes and result.boxes.is_track:
        boxes = result.boxes.xywh.cpu()
        track_ids = result.boxes.id.int().cpu().tolist()
        class_ids = result.boxes.cls.int().cpu().tolist()

        # Visualize the result on the frame
        frame = result.plot(labels=True)

        # Plot the tracks
        for box, track_id, cls_id in zip(boxes, track_ids, class_ids):
            if cls_id != 0:  
                continue
            x, y, w, h = box
            track = track_history[track_id]
            track.append((float(x), float(y)))  # x, y center point
            if len(track) > 30:  # retain 30 tracks for 30 frames
                track.pop(0)

            # Draw the tracking lines
            points = np.hstack(track).astype(np.int32).reshape((-1, 1, 2))
            cv2.polylines(frame, [points], isClosed=False, color=(230, 230, 230), thickness=10)

    # Display the annotated frame
    cv2.imshow("YOLO11 Tracking", frame)

    # Break the loop if 'q' is pressed
    if cv2.waitKey(1) & 0xFF == ord("q"):
        break

