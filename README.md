# Delphi-OpenCV
* OpenCV version - 2.4.13<br>
* Development environment - Delphi 2010-10.3, FPC 3.0.4<br>

<b>Contributors:</b>

 Laentir Valetov (email: laex@bk.ru)<br>
 Mikhail Grigorev (email: sleuthhound@gmail.com)

## Requirements:
* Visual C++ Redistributable for Visual Studio 2015<br>
Files: msvcp140.dll, msvcp140d.dll in "Delphi-OpenCV\redist\" or [here, but it is not exactly][2]<br>
or from the repository (Delphi-OpenCV\redist\VC14):
```
(1) 32-bit in the "\x86"
(2) 64-bit in the "\x64"
```
* Shared library FFMPEG 4.2.1 for Windows can be downloaded from [here][5]<br>
or from the repository (Delphi-OpenCV\redist\ffmpeg):
```
(3) FFmpeg 32-bit Shared "\x86"
(4) FFmpeg 64-bit Shared "\x64"
```
* Dynamic library OpenCV need to download [here][4]<br>
Files: *2413.dll and *2413d.dll<br>
After installing OpenCV:
```
(5) 32-bit in the C:\OpenCV\build\x86\vc14\bin\*.dll
(6) 64-bit in the C:\OpenCV\build\x64\vc14\bin\*.dll
```
* Some examples (FFMPEG) required [SDL 2.0 and SDL 1.2][3]<br>
or from the repository (Delphi-OpenCV\redist\SDL\1.2 and \2.0):
```
(7) 32-bit - SDL.dll and SDL2.dll "\x86"
(8) 64-bit - SDL.dll and SDL2.dll "\x64"
```
# How to install:
## 1. Delphi environment setting
Download and unzip the [archive][1] or clone repository<br>
```
git clone https://github.com/Laex/Delphi-OpenCV.git
```
Get the following directory structure<br>
```
<PROJECT_ROOT> - Directory, for example, "C:\Delphi\OpenCV\"
	<bin>		- here are the executable files of the examples
	<CheckCVDep>	- program for checking the environment
	<Delphi-FFMPEG>	- empty directory for "Delphi-FFMPEG"
	<packages>	- packages for D10-D10.3
	<redist>	- redistributable packages
	<resource>	- media for working examples and Haar-cascades
	<samples>	- examples
	<source>	- object pascal sources for OpenCV, SDL, OpenGL
	<source3>	- attempt for opencv 3
	<source4>	- blank for OpenCV 4
```
Run <b>InitDelphiFFMPEG.cmd</b> to initialize the <b>Delphi-FFMPEG</b> submodule. The <b><Delphi-FFMPEG></b> directory should be populated with sources for <b>Delphi-FFMPEG</b>.
If it didn’t work, then
```
git clone https://github.com/Laex/Delphi-FFMPEG.git
```
Add the search path for the modules of the project in Delphi IDE<br>
"Tools-Options-Delphi Options-Library-Library path" or "Tools-Options-Language-Delphi-Library"
```
<PROJECT_ROOT>\source
<PROJECT_ROOT>\source\opengl
<PROJECT_ROOT>\source\sdl
<PROJECT_ROOT>\source\sdl2
<PROJECT_ROOT>\packages
<PROJECT_ROOT>\resource
<PROJECT_ROOT>\resource\facedetectxml
<PROJECT_ROOT>\source3
<PROJECT_ROOT>\Delphi-FFMPEG\source
```
where ```<PROJECT_ROOT>``` directory, which was unzipped (or cloned) repository.<br>
## 2. Copy dynamic libraries files
<b>OS Windows 64-bit</b><br>
```
Target platform 64-bit: (2),(4),(6),(8) -> "C:\Windows\System32\"
Target platform 32-bit: (1),(3),(5),(7) -> "C:\Windows\SysWOW64\"
```
<b>OS Windows 32-bit</b><br>
```
Target platform 32-bit: (1),(3),(5),(7) -> "C:\Windows\System32\"
```
<b>Alternatively</b>, dynamic libraries can be placed next to an executable file.
## 3. Verify that the environment is configured correctly
Run from the repository
```
 Delphi-OpenCV/CheckCVDep/CheckCVDep.exe
```
The program checks the availability of dynamic libraries
```
------- Verifying Microsoft DLL -------
OK
------- OpenCV DLL -------
OK
------- Delphi-OpenCV classes DLL -------
OK
------- FFMPEG DLL -------
OK
------- SDL DLL -------
OK
```
To successfully install components and run most of the examples, the availability of FFMPEG DLL, Microsoft DLL and OpenCV DLL is sufficient
## 4. Install the components

To install the components, open and install
```
<PROJECT_ROOT>\packages\Delphi XXX\rtpFFMPEG.dpk
<PROJECT_ROOT>\packages\Delphi XXX\rclVCLOpenCV.dpk
<PROJECT_ROOT>\packages\Delphi XXX\rclFMXOpenCV.dpk
<PROJECT_ROOT>\packages\Delphi XXX\dclVCLOpenCV.dpk
<PROJECT_ROOT>\packages\Delphi XXX\dclFMXOpenCV.dpk
```
## Examples
Open in Delphi IDE and compile:<br>
Examples of the use of certain functions and procedures 
```
<PROJECT_ROOT>\samples\LibDemo\LibDemo.groupproj
```
Examples of the use of video processing algorithms
```
<PROJECT_ROOT>\samples\MultiDemo\MultiDemo.groupproj
```
Examples of the use of video processing algorithms using VCL.Forms
```
<PROJECT_ROOT>\samples\VCLDemo\VCLDemo.groupproj
```
Examples of using FFMPEG library header files are in the
```
<PROJECT_ROOT>\Delphi-FFMPEG\examples
```
Examples of use of components
```
<PROJECT_ROOT>\samples\Components\ComponentsDemo.groupproj
```
<b>Donate</b><br>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5Z5JQ7C9JCJQN">PayPal USD</a><br>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=WQYST8J8PR4K2">PayPal EUR</a><br>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=XN8D6TJMSXPFL">PayPal RUB</a><br>
<a href="https://money.yandex.ru/to/410011600173245">Yandex Money</a><br>
[![Donatecoins](http://donatecoins.org/btc/3MTXVtRgQnA22EtBxP97Nws6GS8autp38s.svg)](http://donatecoins.org/btc/3MTXVtRgQnA22EtBxP97Nws6GS8autp38s)


[1]: https://github.com/Laex/Delphi-OpenCV/archive/master.zip
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=48145
[3]: https://www.libsdl.org/index.php
[4]: https://github.com/opencv/opencv/releases/tag/2.4.13.6
[5]: http://ffmpeg.zeranoe.com/builds/
