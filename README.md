###Delphi-OpenCV
* OpenCV version - 2.4.9<br>
* Development environment - Delphi 2010-XE7<br>

####Contributors:
```
 Laentir Valetov (email: laex@bk.ru)
 Mikhail Grigorev (email: sleuthhound@gmail.com)
```

####Requirements:
* Requires installed [Visual C++ redistributable for Visual Studio 2013][2]<br>
* Some examples (FFMPEG) required [SDL 2.0 and SDL 1.2][3]<br>
* <b>Warning! Dynamic OpenCV library is not included in the repository!</b><br>
Dynamic library OpenCV need to download [here] [4]
* Shared library FFMPEG for Windows can be downloaded from [link] [5]

####How to install:
Download the [archive][1]<br>
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
Add to your PATH variable path to the library "opencv_*.dll" and FFMPEG dll.
* for Win32 ```<PROJECT_ROOT>\bin\Win32```
* for Win64 ```<PROJECT_ROOT>\bin\Win64```<br>
<b>OR</b><br>For 64-bit
```
1. Copy the dll files from the <PROJECT_ROOT>\redist\VC2013x64\ to C:\Windows\System32\
2. Copy all the dll from <PROJECT_ROOT>\bin\Win64 in the C:\Windows\System32\
3. Copy the dll files from the <PROJECT_ROOT>\redist\VC2013x86\ to C:\Windows\SysWOW64\
4. Copy all the dll from <PROJECT_ROOT>\bin\Win32 in the C:\Windows\SysWOW64\
```
For 32-bit
```
1. Copy the dll files from the <PROJECT_ROOT>\redist\VC2013x86\ to C:\Windows\System32\
2. Copy all the dll from <PROJECT_ROOT>\bin\Win32 in the C:\Windows\System32\
```
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
[1]: https://github.com/Laex/Delphi-OpenCV/archive/master.zip
[2]: http://www.microsoft.com/ru-ru/download/details.aspx?id=40784
[3]: https://www.libsdl.org/index.php
[4]: http://opencv.org/downloads.html
[5]: http://ffmpeg.zeranoe.com/builds/
