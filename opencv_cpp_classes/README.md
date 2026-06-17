# opencv_cpp_classes (archival)

> **For reference only.** This folder is kept in the repository for historical and informational purposes.

This was an **unsuccessful experiment** with wrapping the OpenCV C++ API (`cv::Mat`, `cv::VideoCapture`, `FaceRecognizer`, etc.) for Delphi 2.4.x via a custom proxy DLL (`opencv_classes`). The approach was incomplete, hard to maintain, and is **not supported**.

## Use Delphi-OpenCV5 instead

For modern OpenCV C++ bindings on Windows (OpenCV 5.0, Pascal units + C++ bridge DLL, IDE packages, samples), use:

**[Laex/Delphi-OpenCV5](https://github.com/Laex/Delphi-OpenCV5)**

## What is here

| Path | Description |
|------|-------------|
| `ocv.cls.*.pas` | Partial Delphi object wrappers |
| `opencv_classes/` | Visual C++ proxy DLL project |
| `examples/` | Demo projects (`Classes.groupproj`) |

See also `readme.txt` for the list of partially wrapped C++ headers.
