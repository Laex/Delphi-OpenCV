# Delphi-OpenCV

[![Delphi Support](https://img.shields.io/badge/Delphi-2010--13-blue.svg?style=flat-square)](https://www.embarcadero.com/products/delphi)
[![OpenCV Version](https://img.shields.io/badge/OpenCV-2.4.13-green.svg?style=flat-square)](https://github.com/opencv/opencv/releases/tag/2.4.13.6)
[![FPC Support](https://img.shields.io/badge/FPC-3.0.4-orange.svg?style=flat-square)](https://www.freepascal.org/)
[![License](https://img.shields.io/badge/License-MPL_1.1-lightgrey.svg?style=flat-square)](http://www.mozilla.org/MPL/MPL-1_1Final.html)

A comprehensive port of the **OpenCV (Open Source Computer Vision Library) v2.4.13** to **Delphi** and **FreePascal (FPC)**. This library enables object pascal developers to leverage computer vision algorithms, image/video processing tools, and FFMPEG integration directly in their applications.

---

## 🌟 Key Features

- **Direct OpenCV Bindings:** Direct access to OpenCV C/C++ APIs (v2.4.13) from Delphi code.
- **FFMPEG Integration:** IP camera / RTSP streaming via [Delphi-FFMPEG](https://github.com/Laex/Delphi-FFMPEG) (`rtpFFMPEG`, `TocvFFMpegIPCamSource`) on **Win32 and Win64**.
- **Cross-Framework UI:** Components and views ready for both **VCL** and **FireMonkey (FMX)** platforms.
- **Rich Samples Collection:** Ready-to-run examples demonstrating object tracking, camera captures, face detection (Haar cascades), motion detection, and OpenGL overlays.

---

## 📋 Prerequisites & Requirements

| Dependency | Version | Description | Download |
| :--- | :--- | :--- | :--- |
| **OpenCV DLLs** | 2.4.13.6 | `opencv_*2413.dll` / `*2413d.dll` (Win32) | [OpenCV 2.4.13.6 release][4] |
| **VC++ Redistributable** | 2015 (VC14) | `msvcp140.dll`, `vcruntime140.dll`, … | [Microsoft VC++ 2015 Redistributable][2] |
| **FFmpeg DLLs** | see Delphi-FFMPEG | Shared libs for `rtpFFMPEG` / IP camera (Win32 & Win64) | [Delphi-FFMPEG script][7], [BtbN Win64][8], [defisym Win32][11] |
| **SDL** | 1.2 & 2.0 | `SDL.dll`, `SDL2.dll` for some video examples | [SDL 1.2][3], [SDL2 releases][9] |
| **JEDI** | submodule | `jedi.inc` required by `source/OpenCV.inc` | [project-jedi/jedi][10] |
| **Delphi-FFMPEG** | submodule / junction | Pascal FFmpeg bindings (`libav*.pas`) | [Laex/Delphi-FFMPEG][6] — see setup below |

> **Note:** The `redist/` folder contains only `readme.md` stubs (`ffmpeg`, `SDL`, `VC14`) with download links. Runtime DLLs are not included in the repository.

---

## 🔗 Delphi-FFMPEG: submodule or development junction

All projects reference **`Delphi-FFMPEG/source`** inside this repository (`<PROJECT_ROOT>/Delphi-FFMPEG/`).

### Standard setup (GitHub clone)

```bash
git clone --recurse-submodules https://github.com/Laex/Delphi-OpenCV.git
cd Delphi-OpenCV
```

or after a plain clone:

```bash
InitDelphiFFMPEG.cmd
```

This initializes **`jedi`** and **`Delphi-FFMPEG`** git submodules.

### Development setup (local Delphi-FFMPEG repo)

If you maintain FFmpeg bindings in a separate clone (e.g. `D:\Work\Delphi\Delphi-FFMPEG`), place it as a **sibling** of this repo or set a custom path:

```text
Delphi/
├── Delphi-FFMPEG/          ← your working clone
└── OpenCV/
    └── Delphi-OpenCV/      ← this repository
```

Run from `<PROJECT_ROOT>`:

```bash
InitDelphiFFMPEG.cmd
```

The script creates a **directory junction** `Delphi-FFMPEG` → `..\..\Delphi-FFMPEG` when that folder contains `source/`. Projects keep using `Delphi-FFMPEG/source` — no path changes needed.

**Custom path:** copy `InitDelphiFFMPEG.local.cmd.example` to `InitDelphiFFMPEG.local.cmd` and set `FFMPEG_DEV_PATH`.

**Manual junction:**

```cmd
mklink /J Delphi-FFMPEG D:\Work\Delphi\Delphi-FFMPEG
```

---

## 🛠️ Installation Guide

### Step 1: Clone and run setup

```bash
git clone https://github.com/Laex/Delphi-OpenCV.git
cd Delphi-OpenCV
InitDelphiFFMPEG.cmd
```

### Step 2: Configure Delphi Library Paths

Add in `Tools → Options → Language → Delphi → Library → Library path`:

```text
<PROJECT_ROOT>/source
<PROJECT_ROOT>/source/opengl
<PROJECT_ROOT>/source/sdl
<PROJECT_ROOT>/source/sdl2
<PROJECT_ROOT>/packages
<PROJECT_ROOT>/resource
<PROJECT_ROOT>/resource/facedetectxml
<PROJECT_ROOT>/jedi
<PROJECT_ROOT>/Delphi-FFMPEG/source
```

> **Note:** `opencv_cpp_classes` is archival only. For OpenCV C++ / 5.x use [Delphi-OpenCV5](https://github.com/Laex/Delphi-OpenCV5).

### Step 3: Set up runtime DLLs

| DLL group | Source |
| :--- | :--- |
| OpenCV 2.4.13 | [OpenCV 2.4.13.6][4] |
| VC++ 2015 | [VC++ 2015][2] |
| FFmpeg | [Delphi-FFMPEG download script][7] |
| SDL | [SDL 1.2][3], [SDL2][9] |

Copy DLLs next to your `.exe` or onto `PATH`.

### Step 4: Verify configuration

Build and run `<PROJECT_ROOT>/CheckCVDep/CheckCVDep.dproj`.

### Step 5: Install Delphi packages

Open `packages/<your Delphi version>/` and install in order:

1. **`rtpFFMPEG.dpk`**
2. **`rclVCLOpenCV.dpk`**
3. **`rclFMXOpenCV.dpk`**
4. **`dclVCLOpenCV.dpk`**
5. **`dclFMXOpenCV.dpk`**

---

## 📂 Directory Structure

```text
<PROJECT_ROOT>
 ├── Delphi-FFMPEG       # Submodule, or junction to your dev clone
 ├── jedi                # JEDI submodule
 ├── packages            # Delphi IDE packages (D2010–D13)
 ├── samples             # Demo projects
 ├── source              # OpenCV 2.4 C API bindings
 └── opencv_cpp_classes  # Archival C++ experiment (see folder README.md)
```

---

## 🚀 Examples & Demos

- **`samples/LibDemo/LibDemo.groupproj`** — basic OpenCV functions and bindings
- **`samples/MultiDemo/MultiDemo.groupproj`** — video processing, motion detection
- **`samples/VCLDemo/VCLDemo.groupproj`** — VCL visual wrappers
- **`samples/Components/ComponentsDemo.groupproj`** — IDE components
- **`Delphi-FFMPEG/examples/Examples.groupproj`** — low-level FFmpeg API samples

For OpenCV C++ / 5.x see **[Delphi-OpenCV5](https://github.com/Laex/Delphi-OpenCV5)**.

---

## 👥 Contributors

- **Laentir Valetov** (Lead Contributor) — [laex@bk.ru](mailto:laex@bk.ru)
- **Mikhail Grigorev** — [sleuthhound@gmail.com](mailto:sleuthhound@gmail.com)

---

## 📄 License

This project is licensed under the **Mozilla Public License Version 1.1 (MPL 1.1)**. See [Mozilla MPL 1.1](http://www.mozilla.org/MPL/MPL-1_1Final.html).

[2]: https://www.microsoft.com/en-us/download/details.aspx?id=48145
[3]: https://www.libsdl.org/download-1.2.php
[4]: https://github.com/opencv/opencv/releases/tag/2.4.13.6
[6]: https://github.com/Laex/Delphi-FFMPEG
[7]: https://github.com/Laex/Delphi-FFMPEG/blob/main/bin/download_ffmpeg_dll.ps1
[8]: https://github.com/BtbN/FFmpeg-Builds/releases
[9]: https://github.com/libsdl-org/SDL/releases
[10]: https://github.com/project-jedi/jedi
[11]: https://github.com/defisym/FFmpeg-Builds-Win32
