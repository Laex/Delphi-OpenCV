# Visual C++ 2015 (VC14) runtime

This folder does not ship Microsoft redistributable DLLs. OpenCV 2.4.13 Windows binaries were built with **Visual Studio 2015** and need the **VC++ 2015 Redistributable** (or equivalent runtime files) on the target machine.

## Typical runtime files

Release builds commonly need:

- `msvcp140.dll`
- `vcruntime140.dll`
- `concrt140.dll` (some configurations)
- Universal CRT (`ucrtbase.dll`) — usually installed with Windows 10+ or via the redistributable

Debug OpenCV DLLs (`*2413d.dll`) require the **debug** variants (`*140d.dll`, `ucrtbased.dll`, …) from a Visual Studio installation; do not redistribute debug runtimes with end-user apps.

## Download

- **Microsoft Visual C++ 2015 Redistributable (x86 and x64):**  
  [https://www.microsoft.com/en-us/download/details.aspx?id=48145](https://www.microsoft.com/en-us/download/details.aspx?id=48145)

Install both x86 and x64 packages if you ship 32-bit and 64-bit applications.
