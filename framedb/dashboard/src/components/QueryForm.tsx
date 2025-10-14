// components/QueryForm.tsx
import { useState } from "react"
import { Textarea } from "@/components/ui/textarea"
import { Button } from "@/components/ui/button"
import { Label } from "@/components/ui/label"
import { Select, SelectTrigger, SelectItem, SelectContent, SelectValue } from "@/components/ui/select"
import { z } from "zod"
import { useForm } from "react-hook-form"
import { zodResolver } from "@hookform/resolvers/zod"
import { Form, FormControl, FormField, FormItem, FormLabel, FormMessage } from "./ui/form"
import { Input } from "./ui/input"

// const formSchema = z.object({
//   video: z
//     .instanceof(FileList)
//     .refine((file) => file?.length == 1, 'Video is required.'),
//   annotations: z
//     .instanceof(FileList)
//     .refine((file) => file?.length == 1, 'Annotation is required.'),
//   ann_format: z.enum(["KPF", "COCO", "DIVE"]),
//   title: z.string().min(3),
//   desc: z.string().optional(),
// })

interface VideoInfo {
  id: string
  name: string
  path: string
  thumbnail_url: string
}

const formSchema = z.object({
  videoId: z.string().min(1, "Please select a video"),
  query: z.string().min(1, "Please enter a query string"),
})

export default function ({ videoList }: { videoList: VideoInfo[] }) {
  const [selectedVideo, setSelectedVideo] = useState<string>("")
  const [query, setQuery] = useState("")
  const [resultUrl, setResultUrl] = useState<string | null>(null)

  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      videoId: "",
      query: "Person(id) = Object(id, class_id), class_id='person'",
    },
  })

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    try {
      const { videoId, query } = values
      const url = `http://10.144.224.104:3000/query/${videoId}/${encodeURIComponent(query)}`
      const res = await fetch(url)

      if (!res.ok) throw new Error("Failed to run query")

      // Assume the backend returns a video file (binary)
      const blob = await res.blob()
      const objectUrl = URL.createObjectURL(blob)
      setResultUrl(objectUrl)
    } catch (err) {
      console.error("Error running query:", err)
      alert("Query failed — see console for details.")
    }
  }

  return (
    <div className="max-w-md mx-auto space-y-6">
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-6">
          {/* Video Selector */}
          <FormField
            control={form.control}
            name="videoId"
            render={({ field }) => (
              <FormItem>
                <FormLabel>Video</FormLabel>
                <Select
                  onValueChange={field.onChange}
                  defaultValue={field.value}
                >
                  <FormControl>
                    <SelectTrigger>
                      <SelectValue placeholder="Select a video" />
                    </SelectTrigger>
                  </FormControl>
                  <SelectContent>
                    {videoList.map((v) => (
                      <SelectItem key={v.id} value={v.id}>
                        {v.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
                <FormMessage />
              </FormItem>
            )}
          />

          {/* Query String */}
          <FormField
            control={form.control}
            name="query"
            render={({ field }) => (
              <FormItem>
                <FormLabel>Query</FormLabel>
                <FormControl>
                  <Input defaultValue="Person(id) = Object(id, class_id), class_id='person'" {...field} />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />

          <Button type="submit" className="w-full">
            Run Query
          </Button>
        </form>
      </Form>

      {/* Result section */}
      {resultUrl && (
        <div className="pt-6">
          <h3 className="text-lg font-semibold mb-2">Query Result</h3>
          <video src={resultUrl} controls width={400} className="rounded-md border" />
        </div>
      )}
    </div>
  )
}

