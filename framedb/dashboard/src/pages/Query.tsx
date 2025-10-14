import QueryForm from "@/components/QueryForm"
import { useEffect, useState } from "react"

interface VideoInfo {
  id: string
  name: string
  path: string
  thumbnail_url: string
}

export default function QueryPage() {
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
      <h1 className="text-2xl font-bold mb-4">Query Videos</h1>
      <QueryForm videoList={videos} />
    </div>
  )
}

