import { ThemeProvider } from "@/components/theme-provider"
import { Navbar } from "./components/Navbar"
import { Routes, Route } from "react-router-dom"
import QueryPage from "./pages/Query"
import UploadPage from "./pages/Upload"
import LibraryPage from "./pages/Library"


function App() {
  return (
    <ThemeProvider defaultTheme="dark" storageKey="vite-ui-theme">
      <div>
        <Navbar />
        <Routes>
          <Route path="/" element={<LibraryPage />} />
          <Route path="/upload" element={<UploadPage />} />
          <Route path="/query" element={<QueryPage/>} />
        </Routes>
      </div>
    </ThemeProvider>
  )
}

export default App
      // <video controls>
      //   <source
      //     src="http://localhost:9996/get?duration=60.905&path=mystream&start=2025-10-03T22%3A31%3A48.735643%2B05%3A30"
      //     type="video/mp4"
      //   />
      // </video>
