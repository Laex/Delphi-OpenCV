#Delphi-OpenCV
* OpenCV version - 2.4.13.3<br>
* Development environment - Delphi 2010-10.2<br>

<b>Contributors:</b>
```
 Laentir Valetov (email: laex@bk.ru)
 Mikhail Grigorev (email: sleuthhound@gmail.com)
```
##Requirements:
* Visual C++ Redistributable for Visual Studio 2015<br>
Files: msvcp140.dll, msvcp140d.dll in "Delphi-OpenCV\redist\" or [here][2]
```
(1) 32-bit in the "Delphi-OpenCV\redist\VC14x86\"
(2) 64-bit in the "Delphi-OpenCV\redist\VC14x64\"
```
* Shared library FFMPEG 3.3.3 for Windows can be downloaded from [here][5]<br>
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
(7) SDL.dll and SDL2.dll
```

####Copy files
<b>OS Windows 64-bit</b><br>
```
Target platform 64-bit: (2),(4),(6) -> "C:\Windows\System32\"
Target platform 32-bit: (1),(3),(5),(7) -> "C:\Windows\SysWOW64\"
```
<b>OS Windows 32-bit</b><br>
```
Target platform 32-bit: (1),(3),(5),(7) -> "C:\Windows\System32\"
```

##How to install:
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
Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)
```
<PROJECT_ROOT>\source
<PROJECT_ROOT>\source\utils
<PROJECT_ROOT>\source\component
<PROJECT_ROOT>\source\sdl
<PROJECT_ROOT>\source\opengl
<PROJECT_ROOT>\source\ffmpeg
<PROJECT_ROOT>\resource\facedetectxml
```
where ```<PROJECT_ROOT>``` directory, which was unzipped project.<br>

To install the components, open and install
```
<PROJECT_ROOT>\source\component\DelphiXX\OpenCVXXX.dpk
<PROJECT_ROOT>\source\component\DelphiXX\dclCommonOpenCVXXX.dpk
<PROJECT_ROOT>\source\component\DelphiXX\dclVCLOpenCVXXX.dpk
<PROJECT_ROOT>\source\component\DelphiXX\dclFMXOpenCVXXX.dpk
```
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
<PROJECT_ROOT>\samples\FFMpeg\FFMPEG.groupproj
```
Examples of use of components
```
<PROJECT_ROOT>\samples\Components\ComponentsDemo.groupproj
```
<br><a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5Z5JQ7C9JCJQN">Donate (PayPal USD)</a>
<br><a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=WQYST8J8PR4K2">Donate (PayPal EUR)</a>
<br><a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=XN8D6TJMSXPFL">Donate (PayPal RUB)</a>
<br>Yandex Money: 410012802258318

[1]: https://github.com/Laex/Delphi-OpenCV/archive/master.zip
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=48145
[3]: https://www.libsdl.org/index.php
[4]: https://github.com/opencv/opencv/releases/tag/2.4.13.2
[5]: http://ffmpeg.zeranoe.com/builds/