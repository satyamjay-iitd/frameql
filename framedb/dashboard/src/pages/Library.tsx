
import VideoCard from "@/components/VideoCard"
import { useEffect, useState } from "react"

interface VideoInfo {
  id: string
  name: string
  path: string
  thumbnail_url: string
}

export default function LibraryPage() {
  const [videos, setVideos] = useState<VideoInfo[]>([])

  useEffect(() => {
    const fetchVideos = async () => {
      try {
        const res = await fetch("http://10.144.224.104:3000/video")
        if (!res.ok) throw new Error("Failed to fetch videos")
        const data: VideoInfo[] = await res.json()
        console.log(data)
        setVideos(data)
      } catch (err) {
        console.error("Error fetching videos:", err)
      }
    }
    fetchVideos()
  }, [])

  return (
    <div className="p-6">
      <div className="flex gap-4 flex-wrap">
        {videos.map((video) => (
          <VideoCard
            key={video.id}
            video={{
              id: video.id,
              title: video.name,
              thumbnailUrl: video.thumbnail_url,
            }}
          />
        ))}
      </div>
    </div>
  )
}
