import {
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
} from "@/components/ui/navigation-menu"
import { ModeToggle } from "@/components/mode-toggle"
import { Link } from "react-router-dom"
import { cn } from "@/lib/utils"

export function Navbar() {
  return (
    <nav className="flex items-center justify-between px-6 py-3 border-b bg-background">
    <NavigationMenu>
      <NavigationMenuList>
        <NavigationMenuItem>
          <Link to="/">
            <NavigationMenuLink className={cn("px-4 py-2 hover:bg-muted")}>
              Home
            </NavigationMenuLink>
          </Link>
        </NavigationMenuItem>

        <NavigationMenuItem>
          <Link to="/upload">
            <NavigationMenuLink className={cn("px-4 py-2 hover:bg-muted")}>
              Upload
            </NavigationMenuLink>
          </Link>
        </NavigationMenuItem>

        <NavigationMenuItem>
          <Link to="/query">
            <NavigationMenuLink className={cn("px-4 py-2 hover:bg-muted")}>
              Query
            </NavigationMenuLink>
          </Link>
        </NavigationMenuItem>
      </NavigationMenuList>

    </NavigationMenu>
  
    {/* Right Side: Mode Toggle */}
    <div className="ml-auto">
      <ModeToggle />
    </div>
  
    </nav>
  )
}
