import QueryForm from "@/components/QueryForm"

export default function QueryPage() {
  const videoList = ["video1.mp4", "video2.mp4"] // Fetch this from backend
  return (
    <div className="p-6">
      <h1 className="text-2xl font-bold mb-4">Query Videos</h1>
      <QueryForm videoList={videoList} />
    </div>
  )
}

