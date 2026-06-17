# SDL runtime DLLs

This folder does not ship SDL binaries. Some video samples expect SDL 1.2 and/or SDL 2.x next to the executable.

## Files

| Library | DLL (Win32) | DLL (Win64) |
| :--- | :--- | :--- |
| SDL 1.2 | `SDL.dll` | `SDL.dll` |
| SDL 2.x | `SDL2.dll` | `SDL2.dll` |

Copy the DLLs for your target platform into the sample output directory or add their location to `PATH`.

## Download

- **SDL 1.2:** [libsdl.org — SDL 1.2 download](https://github.com/libsdl-org/SDL-1.2)
- **SDL 2.x:** [SDL releases on GitHub](https://github.com/libsdl-org/SDL/releases)

Use the prebuilt **Visual C++** runtime packages for Windows (`SDL2-*-win32-x86.zip` / `SDL2-*-win32-x64.zip` or equivalent from the release page).
