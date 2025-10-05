// components/VideoCard.tsx
import { Card, CardContent, CardTitle, CardDescription } from "@/components/ui/card"
import { Button } from "@/components/ui/button"

type Props = {
  video: {
    id: string
    title: string
    thumbnailUrl?: string
    tags: string[]
  }
  onView: (id: string) => void
}

export default function VideoCard({ video, onView }: Props) {
  return (
    <Card className="w-[300px]">
      {video.thumbnailUrl ? (
        <img src={video.thumbnailUrl} alt="thumbnail" className="w-full h-[160px] object-cover" />
      ) : (
        <div className="bg-gray-200 h-[160px]" />
      )}
      <CardContent className="p-4">
        <CardTitle>{video.title}</CardTitle>
        <CardDescription>{video.tags.join(", ")}</CardDescription>
        <Button onClick={() => onView(video.id)} className="mt-2">View</Button>
      </CardContent>
    </Card>
  )
}

