# FFmpeg runtime DLLs

This folder does not ship FFmpeg binaries. Download **shared** libraries matching your [Delphi-FFMPEG](https://github.com/Laex/Delphi-FFMPEG) bindings and place them next to your executable (or on `PATH`).

## Platforms

| Platform | Typical DLL set |
| :--- | :--- |
| **Win32** | 32-bit `avcodec-*.dll`, `avutil-*.dll`, `avformat-*.dll`, … |
| **Win64** | 64-bit `avcodec-*.dll`, `avutil-*.dll`, `avformat-*.dll`, … |

DLL major versions must match `Delphi-FFMPEG/source/ffmpeg.inc` (currently FFmpeg 8.1.x).

## Download

- **Recommended:** [Delphi-FFMPEG `bin/download_ffmpeg_dll.ps1`](https://github.com/Laex/Delphi-FFMPEG/blob/main/bin/download_ffmpeg_dll.ps1)
- **Win64:** [BtbN FFmpeg-Builds releases](https://github.com/BtbN/FFmpeg-Builds/releases) — `win64-gpl-shared`
- **Win32:** [defisym/FFmpeg-Builds-Win32](https://github.com/defisym/FFmpeg-Builds-Win32) — build `win32 gpl-shared` locally (no auto-releases for x86)

See also the main [Delphi-FFMPEG](https://github.com/Laex/Delphi-FFMPEG) repository.
