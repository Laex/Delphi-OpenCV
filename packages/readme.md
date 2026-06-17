Visual and non-visual components for working with the library OpenCV
------------------------
Version: OpenCV 2.4.13

Requires installed [Visual C++ Redistributable for Visual Studio 2015 (VC14)][2]

OpenCV 2.4.13 DLLs: download from [OpenCV 2.4.13.6 release][4] and add to `PATH` or next to your `.exe`.

FFmpeg (optional): shared DLLs per [Delphi-FFMPEG](https://github.com/Laex/Delphi-FFMPEG) `source/ffmpeg.inc`. Download via [download_ffmpeg_dll.ps1][7] or [BtbN FFmpeg builds][8]. Pascal sources: `<PROJECT_ROOT>/Delphi-FFMPEG/source/` (submodule or junction — see root [README](../README.md)).

Installation
------------
1. Run `InitDelphiFFMPEG.cmd` in the repo root (`jedi` submodule + `Delphi-FFMPEG` submodule or dev junction).
2. Add OpenCV, VC++, FFmpeg, and (if needed) SDL DLLs to `PATH` or next to executables.
3. Open `<PROJECT_ROOT>/packages/<Delphi version>/OpenCV.groupproj`. Build and install: `rtpFFMPEG` → `rclVCLOpenCV` → `rclFMXOpenCV` → `dclVCLOpenCV` → `dclFMXOpenCV`.
4. Supported Delphi versions include Delphi 12 Athens and Delphi 13 Florence (`packages/Delphi 13 Florence/`).
5. Sample projects: `<PROJECT_ROOT>/samples/Components/`

[2]: https://www.microsoft.com/en-us/download/details.aspx?id=48145
[4]: https://github.com/opencv/opencv/releases/tag/2.4.13.6
[7]: https://github.com/Laex/Delphi-FFMPEG/blob/main/bin/download_ffmpeg_dll.ps1
[8]: https://github.com/BtbN/FFmpeg-Builds/releases
