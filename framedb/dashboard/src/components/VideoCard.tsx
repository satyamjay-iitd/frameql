// components/VideoCard.tsx
import { Card, CardContent, CardTitle} from "@/components/ui/card"
import { Button } from "@/components/ui/button"

interface VideoCardProps {
  video: {
    id: string
    title: string
    thumbnailUrl: string
  }
}

export default function VideoCard({ video }: VideoCardProps) {
  const handleView = () => {
    window.open(`http://10.144.224.104:3000/video/${video.id}`, "_blank")
  }
  return (
    <Card className="w-[300px]">
      {video.thumbnailUrl ? (
        <img
          src={`http://10.144.224.104:3000${video.thumbnailUrl}`}
          alt="thumbnail" className="w-full h-[160px] object-cover" />
      ) : (
        <div className="bg-gray-200 h-[160px]" />
      )}
      <CardContent className="p-4">
        <CardTitle>{video.title}</CardTitle>
        <Button onClick={handleView} className="mt-2">View</Button>
      </CardContent>
    </Card>
  )
}

