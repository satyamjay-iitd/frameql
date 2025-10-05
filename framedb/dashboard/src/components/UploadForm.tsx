// import { useState } from "react"
// import { Input } from "@/components/ui/input"
// import { Textarea } from "@/components/ui/textarea"
// import { Label } from "@/components/ui/label"
// import { Button } from "@/components/ui/button"

// export default function UploadForm() {
//   const [videoFile, setVideoFile] = useState<File | null>(null)
//   const [annotationFile, setAnnotationFile] = useState<File | null>(null)

//   const [metadata, setMetadata] = useState({
//     title: "",
//     description: "",
//     tags: "",
//     category: "",
//   })

//   const handleSubmit = async (e: React.FormEvent) => {
//     e.preventDefault()
//     if (!videoFile) return

//     const formData = new FormData()
//     formData.append("video", videoFile)
//     if (annotationFile) formData.append("annotation", annotationFile)

//     Object.entries(metadata).forEach(([key, value]) => {
//       formData.append(key, value)
//     })

//     // TODO: Replace with actual upload logic
//     console.log("Submitting form data:", {
//       videoFile,
//       annotationFile,
//       metadata,
//     })
//   }

//   return (
//     <form
//       onSubmit={handleSubmit}
//       className="space-y-8 p-6 rounded-lg shadow border"
//     >
//       {/* File Uploads */}
//       <fieldset className="space-y-4">
//         <legend className="text-lg font-semibold mb-2">
//           Files
//         </legend>

//         <div>
//           <Label htmlFor="video">Video File</Label>
//           <Input
//             id="video"
//             type="file"
//             accept="video/*"
//             onChange={(e) => setVideoFile(e.target.files?.[0] || null)}
//           />
//         </div>

//         <div>
//           <Label htmlFor="annotation">Annotation File</Label>
//           <Input
//             id="annotation"
//             type="file"
//             accept=".json,.xml,.txt"
//             onChange={(e) => setAnnotationFile(e.target.files?.[0] || null)}
//           />
//         </div>
//       </fieldset>

//       {/* Metadata */}
//       <fieldset className="space-y-4">
//         <legend className="text-lg font-semibold mb-2">
//           Video Metadata
//         </legend>

//         <div>
//           <Label htmlFor="title">Title</Label>
//           <Input
//             id="title"
//             value={metadata.title}
//             onChange={(e) =>
//               setMetadata({ ...metadata, title: e.target.value })
//             }
//             placeholder="Enter video title"
//           />
//         </div>

//         <div>
//           <Label htmlFor="description">Description</Label>
//           <Textarea
//             id="description"
//             value={metadata.description}
//             onChange={(e) =>
//               setMetadata({ ...metadata, description: e.target.value })
//             }
//             placeholder="Short description of the video"
//           />
//         </div>

//         <div>
//           <Label htmlFor="tags">Tags (comma-separated)</Label>
//           <Input
//             id="tags"
//             value={metadata.tags}
//             onChange={(e) =>
//               setMetadata({ ...metadata, tags: e.target.value })
//             }
//             placeholder="e.g. AI, tutorial, ML"
//           />
//         </div>

//         <div>
//           <Label htmlFor="category">Category</Label>
//           <Input
//             id="category"
//             value={metadata.category}
//             onChange={(e) =>
//               setMetadata({ ...metadata, category: e.target.value })
//             }
//             placeholder="e.g. Education, Technology"
//           />
//         </div>
//       </fieldset>

//       <Button type="submit" disabled={!videoFile}>
//         Upload
//       </Button>
//     </form>
//   )
// }

import { cn } from "@/lib/utils"
import { Button } from "@/components/ui/button"
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
// import {
//   Field,
//   FieldDescription,
//   FieldGroup,
//   FieldLabel,
//   FieldSeparator,
// } from "@/components/ui/field"
// import { Input } from "@/components/ui/input"
// import { Textarea } from "./ui/textarea"
import {
  Tags,
  TagsContent,
  TagsEmpty,
  TagsGroup,
  TagsInput,
  TagsItem,
  TagsList,
  TagsTrigger,
  TagsValue,
} from '@/components/ui/shadcn-io/tags';
import { CheckIcon, PlusIcon } from 'lucide-react';
import { useState } from 'react';
import { useForm } from "react-hook-form"
import { Form, FormControl, FormField, FormItem, FormLabel, FormMessage } from "./ui/form";
import { Input } from "./ui/input";
import { FieldSeparator } from "./ui/field";
import { Textarea } from "./ui/textarea";
import { zodResolver } from "@hookform/resolvers/zod"
import { z } from "zod"

const formSchema = z.object({
  video: z
    .instanceof(FileList)
    .refine((file) => file?.length == 1, 'Video is required.'),
  annotations: z
    .instanceof(FileList)
    .refine((file) => file?.length == 1, 'Annotation is required.'),
  title: z.string().min(3),
  desc: z.string().optional(),
})

function onSubmit(values: z.infer<typeof formSchema>) {
  const formData = new FormData()

  formData.append("video", values.video[0])
  formData.append("annotations", values.annotations[0])
  formData.append("title", values.title)
  formData.append("desc", values.desc || "")

  fetch("http://localhost:3000/upload", {
    method: "POST",
    body: formData,
  })
    .then(async (res) => {
      const text = await res.text()
      console.log("Upload response:", text)
    })
    .catch((err) => {
      console.error("Upload failed:", err)
    })
}

export default function UploadForm({
  className,
  ...props
}: React.ComponentProps<"div">) {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
  });

  const videoRef = form.register("video");
  const annRef = form.register("annotations");

  return (
    <div className={cn("flex flex-col gap-6", className)} {...props}>
      <Card>
        <CardHeader className="text-center">
          <CardTitle className="text-xl">Upload new Video</CardTitle>
        </CardHeader>
        <CardContent>
          <Form {...form}>
            <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8">
                <FieldSeparator className="*:data-[slot=field-separator-content]:bg-card">
                  Files                
                </FieldSeparator>

                <FormField
                  control={form.control}
                  name="video"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Video</FormLabel>
                      <FormControl>
                        <Input type="file" {...videoRef}/>
                      </FormControl>
                      <FormMessage />
                  </FormItem>
                  )}
                />

                <FormField
                  control={form.control}
                  name="annotations"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Annotations</FormLabel>
                      <FormControl>
                        <Input type="file" {...annRef}/>
                      </FormControl>
                      <FormMessage />
                  </FormItem>
                  )}
                />

                <FieldSeparator className="*:data-[slot=field-separator-content]:bg-card">
                  Metadata                
                </FieldSeparator>

                <FormField
                  control={form.control}
                  name="title"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Title</FormLabel>
                      <FormControl>
                        <Input type="text" placeholder="e.g. traffic.mp4" {...field} required/>
                      </FormControl>
                      <FormMessage />
                  </FormItem>
                  )}
                />

                <FormField
                  control={form.control}
                  name="desc"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Description</FormLabel>
                      <FormControl>
                        <Textarea {...field} required/>
                      </FormControl>
                      <FormMessage />
                  </FormItem>
                  )}
                />

                <Button type="submit">Upload</Button>
            </form>
          </Form>
        </CardContent>
      </Card>
    </div>
  )
}




// <FormField
//   control={form.control}
//   name="tags"
//   render={({ field }) => (
//     <FormItem>
//       <FormLabel>Description</FormLabel>
//       <FormControl>
//         <MyTag/>
//       </FormControl>
//       <FormMessage />
//   </FormItem>
//   )}
// />


const defaultTags = [
  { id: 'traffic', label: 'Traffic' },
];

const MyTag = () => {
  const [selected, setSelected] = useState<string[]>([]);
  const [newTag, setNewTag] = useState<string>('');
  const [tags, setTags] =
    useState<{ id: string; label: string }[]>(defaultTags);

  const handleRemove = (value: string) => {
    if (!selected.includes(value)) {
      return;
    }

    console.log(`removed: ${value}`);
    setSelected((prev) => prev.filter((v) => v !== value));
  };

  const handleSelect = (value: string) => {
    if (selected.includes(value)) {
      handleRemove(value);
      return;
    }

    console.log(`selected: ${value}`);
    setSelected((prev) => [...prev, value]);
  };

  const handleCreateTag = () => {
    console.log(`created: ${newTag}`);

    setTags((prev) => [
      ...prev,
      {
        id: newTag,
        label: newTag,
      },
    ]);
    setSelected((prev) => [...prev, newTag]);
    setNewTag('');
  };

  return (
    <Tags>
      <TagsTrigger>
        {selected.map((tag) => (
          <TagsValue key={tag} onRemove={() => handleRemove(tag)}>
            {tags.find((t) => t.id === tag)?.label}
          </TagsValue>
        ))}
      </TagsTrigger>
      <TagsContent>
        <TagsInput onValueChange={setNewTag} placeholder="Search tag..." />
        <TagsList>
          <TagsEmpty>
            <button
              className="mx-auto flex cursor-pointer items-center gap-2"
              onClick={handleCreateTag}
              type="button"
            >
              <PlusIcon className="text-muted-foreground" size={14} />
              Create new tag: {newTag}
            </button>
          </TagsEmpty>
          <TagsGroup>
            {tags.map((tag) => (
              <TagsItem key={tag.id} onSelect={handleSelect} value={tag.id}>
                {tag.label}
                {selected.includes(tag.id) && (
                  <CheckIcon className="text-muted-foreground" size={14} />
                )}
              </TagsItem>
            ))}
          </TagsGroup>
        </TagsList>
      </TagsContent>
    </Tags>
  );
};

