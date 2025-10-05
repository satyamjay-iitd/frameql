import VideoCard from "@/components/VideoCard"

const sampleVideos = [
  { id: "1", title: "Test Video", tags: ["test", "demo"], thumbnailUrl: "" },
]

export default function LibraryPage() {
  const handleView = (id: string) => {
    console.log("Viewing", id)
    // Open modal or navigate to detailed video view
  }

  return (
    <div className="p-6">
      <h1 className="text-2xl font-bold mb-4">Video Library</h1>
      <div className="flex gap-4 flex-wrap">
        {sampleVideos.map((video) => (
          <VideoCard key={video.id} video={video} onView={handleView} />
        ))}
      </div>
    </div>
  )
}

