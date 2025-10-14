import { cn } from "@/lib/utils"
import { Button } from "@/components/ui/button"
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
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
import { Check, CheckIcon, ChevronsUpDown, PlusIcon } from 'lucide-react';
import { useState } from 'react';
import { useForm } from "react-hook-form"
import { Form, FormControl, FormField, FormItem, FormLabel, FormMessage } from "./ui/form";
import { Input } from "./ui/input";
import { FieldSeparator } from "./ui/field";
import { Textarea } from "./ui/textarea";
import { zodResolver } from "@hookform/resolvers/zod"
import { z } from "zod"
import { Popover, PopoverContent, PopoverTrigger } from "./ui/popover";
import { Command, CommandEmpty, CommandGroup, CommandInput, CommandItem, CommandList } from "./ui/command";


const annotation_types = [
  { label: "KPF", value: "KPF" },
  { label: "COCO", value: "COCO" },
  { label: "DIVE", value: "DIVE" },
] as const

const formSchema = z.object({
  video: z
    .instanceof(FileList)
    .refine((file) => file?.length == 1, 'Video is required.'),
  annotations: z
    .instanceof(FileList)
    .refine((file) => file?.length == 1, 'Annotation is required.'),
  ann_format: z.enum(["KPF", "COCO", "DIVE"]),
  title: z.string().min(3),
  desc: z.string().optional(),
})

function onSubmit(values: z.infer<typeof formSchema>) {
  const formData = new FormData()

  formData.append("video", values.video[0])
  formData.append("annotations", values.annotations[0])
  formData.append("ann_format", values.ann_format)
  formData.append("title", values.title)
  formData.append("desc", values.desc || "")

  fetch("http://10.144.224.104:3000/upload", {
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
                name="ann_format"
                render={({field}) => (
                <FormItem className="flex flex-col">
                  <FormLabel>Annotation Format</FormLabel>
                    <Popover>
                        <PopoverTrigger asChild>
                          <FormControl>
                            <Button
                              variant="outline"
                              role="combobox"
                              className={cn(
                                "w-[200px] justify-between",
                                !field.value && "text-muted-foreground"
                              )}
                            >
                              {field.value
                                ? annotation_types.find(
                                    (t) => t.value === field.value
                                  )?.label
                                : "Select Annotation Type"}
                              <ChevronsUpDown className="opacity-50" />
                            </Button>
                          </FormControl>
                        </PopoverTrigger>
                        <PopoverContent className="w-[200px] p-0">
                          <Command>
                            <CommandInput
                              placeholder="Search framework..."
                              className="h-9"
                            />
                            <CommandList>
                              <CommandEmpty>No framework found.</CommandEmpty>
                              <CommandGroup>
                                {annotation_types.map((ann) => (
                                  <CommandItem
                                    value={ann.label}
                                    key={ann.value}
                                    onSelect={() => {
                                      form.setValue("ann_format", ann.value)
                                    }}
                                  >
                                    {ann.label}
                                    <Check
                                      className={cn(
                                        "ml-auto",
                                        ann.value === field.value
                                          ? "opacity-100"
                                          : "opacity-0"
                                      )}
                                    />
                                  </CommandItem>
                                ))}
                              </CommandGroup>
                            </CommandList>
                          </Command>
                        </PopoverContent>
                      </Popover>
                      <FormMessage />
                    </FormItem>
                  )}
                />

                <FormField
                  control={form.control}
                  name="annotations"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Annotation File</FormLabel>
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

