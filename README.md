# Delphi-OpenCV
* OpenCV version - 2.4.13<br>
* Development environment - Delphi 2010-10.3<br>

<b>Contributors:</b>

 Laentir Valetov (email: laex@bk.ru)<br>
 Mikhail Grigorev (email: sleuthhound@gmail.com)

## Requirements:
* Visual C++ Redistributable for Visual Studio 2015<br>
Files: msvcp140.dll, msvcp140d.dll in "Delphi-OpenCV\redist\" or [here][2]<br>
or from the repository:
```
(1) 32-bit in the "Delphi-OpenCV\redist\VC14\x86\"
(2) 64-bit in the "Delphi-OpenCV\redist\VC14\x64\"
```
* Shared library FFMPEG 4.0.2 for Windows can be downloaded from [here][5]<br>
```
(3) FFmpeg 32-bit Shared
(4) FFmpeg 64-bit Shared
```
* Dynamic library OpenCV need to download [here][4]<br>
Files: *2413.dll and *2413d.dll
```
After installing OpenCV:
(5) 32-bit in the C:\OpenCV\build\x86\vc14\bin\*.dll
(6) 64-bit in the C:\OpenCV\build\x64\vc14\bin\*.dll
```
* Some examples (FFMPEG) required [SDL 2.0 and SDL 1.2][3]<br>
```
(7) 32-bit - SDL.dll and SDL2.dll
(8) 64-bit - SDL.dll and SDL2.dll
```

#### Copy files
<b>OS Windows 64-bit</b><br>
```
Target platform 64-bit: (2),(4),(6),(8) -> "C:\Windows\System32\"
Target platform 32-bit: (1),(3),(5),(7) -> "C:\Windows\SysWOW64\"
```
<b>OS Windows 32-bit</b><br>
```
Target platform 32-bit: (1),(3),(5),(7) -> "C:\Windows\System32\"
```

## How to install:
Download the [archive][1].<br>
Unzip it to a convenient directory, thus get the following directory structure<br>
```
<PROJECT_ROOT> - Directory, for example, "C:\Delphi\OpenCV\"
	<bin>
	<redist>
	<resource>
	<samples>
	<source>
```
Download the [FFmpeg Delphi/Pascal Headers 4.0.2][6] and extract to <PROJECT_ROOT>\source\ffmpeg<br>
Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)
```
<PROJECT_ROOT>\source
<PROJECT_ROOT>\source\classes
<PROJECT_ROOT>\source\component
<PROJECT_ROOT>\source\ffmpeg\headers
<PROJECT_ROOT>\source\opengl
<PROJECT_ROOT>\source\sdl
<PROJECT_ROOT>\source\sdl2
<PROJECT_ROOT>\source\utils
<PROJECT_ROOT>\resource\facedetectxml
```
where ```<PROJECT_ROOT>``` directory, which was unzipped project.<br>

To install the components, open and install
```
<PROJECT_ROOT>\source\component\DelphiXX\dclCommonOpenCVXXX.dpk
<PROJECT_ROOT>\source\component\DelphiXX\dclFFMSourceXXX.dpk
<PROJECT_ROOT>\source\component\DelphiXX\dclFMXOpenCVXXX.dpk
<PROJECT_ROOT>\source\component\DelphiXX\dclVCLOpenCVXXX.dpk
```
## Verify that the environment is configured correctly
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
<PROJECT_ROOT>\source\ffmpeg\examples
```
Examples of use of components
```
<PROJECT_ROOT>\samples\Components\ComponentsDemo.groupproj
```
<b>Donate</b><br>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5Z5JQ7C9JCJQN">PayPal USD</a><br>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=WQYST8J8PR4K2">PayPal EUR</a><br>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=XN8D6TJMSXPFL">PayPal RUB</a><br>
<a href="https://money.yandex.ru/to/410011600173245">Yandex Money</a>


[1]: https://github.com/Laex/Delphi-OpenCV/archive/master.zip
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=48145
[3]: https://www.libsdl.org/index.php
[4]: https://github.com/opencv/opencv/releases/tag/2.4.13.3
[5]: http://ffmpeg.zeranoe.com/builds/
[6]: http://www.delphiffmpeg.com/headers/