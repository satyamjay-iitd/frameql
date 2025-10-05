// components/QueryForm.tsx
import { useState } from "react"
import { Textarea } from "@/components/ui/textarea"
import { Button } from "@/components/ui/button"
import { Label } from "@/components/ui/label"
import { Select, SelectTrigger, SelectItem, SelectContent } from "@/components/ui/select"

export default function QueryForm({ videoList }: { videoList: string[] }) {
  const [selectedVideo, setSelectedVideo] = useState<string>("")
  const [query, setQuery] = useState("")
  const [resultUrl, setResultUrl] = useState<string | null>(null)

  const handleSubmit = async () => {
    // Call backend with selected video and query string
    // Example response: setResultUrl("https://...result.mp4")
  }

  return (
    <div className="space-y-4 max-w-xl">
      <Label>Select Video</Label>
      <Select onValueChange={setSelectedVideo}>
        <SelectTrigger>{selectedVideo || "Choose a video"}</SelectTrigger>
        <SelectContent>
          {videoList.map((v) => (
            <SelectItem key={v} value={v}>{v}</SelectItem>
          ))}
        </SelectContent>
      </Select>

      <Label>Query</Label>
      <Textarea value={query} onChange={(e) => setQuery(e.target.value)} rows={6} placeholder="Enter custom query here..." />

      <Button onClick={handleSubmit}>Run Query</Button>

      {resultUrl && (
        <div>
          <video src={resultUrl} controls className="mt-4 w-full rounded" />
        </div>
      )}
    </div>
  )
}

