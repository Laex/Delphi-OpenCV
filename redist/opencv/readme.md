# OpenCV 2.4.13 runtime DLLs

This folder does not ship OpenCV binaries. Delphi-OpenCV loads shared libraries named `opencv_*2413.dll` (release) or `*2413d.dll` (debug build).

## Platforms

| Platform | Typical location after install |
| :--- | :--- |
| **Win32** | `<opencv>\build\x86\vc14\bin\` |
| **Win64** | `<opencv>\build\x64\vc14\bin\` |

Copy DLLs next to your `.exe` (`bin\Win32`, `bin\Win64`) or onto `PATH`.

**Important:** a 64-bit `.exe` must load **64-bit** OpenCV DLLs. If `PATH` contains a Win32 `bin` folder first, Windows may load 32-bit DLLs and the app fails with **0xc000007b**.

## Download

- [OpenCV 2.4.13.6 for Windows (vc14)](https://sourceforge.net/projects/opencvlibrary/files/opencv-win/2.4.13/opencv-2.4.13.6-vc14.exe/download)

After extracting, run from the repo root:

```bat
CopyOpenCVDeps.cmd "C:\opencv\build"
```

(adjust the path to your OpenCV `build` folder)

## VC++ runtime

OpenCV 2.4.13 vc14 binaries also need **Visual C++ 2015** runtime — see [redist/VC14/readme.md](../VC14/readme.md).
