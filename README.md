###Delphi-OpenCV
* OpenCV version - 2.4.9<br>
* Development environment - Delphi XE2-XE6<br>

Requires installed [Visual C++ redistributable for Visual Studio 2013][2]<br>

#####Contributors:
```
 Laentir Valetov (email: laex@bk.ru)
 Mikhail Grigorev (email: sleuthhound@gmail.com)
```
####How to install:
Download the archive [link][1].<br>
Unzip it to a convenient directory, thus get the following directory structure.<br>
```
<PROJECT_ROOT> - Directory, such as "C:\OpenCV\"
		<3rdParty>
		<bin>
		<component>
		<include>
		<opencv_classes>
		<samples>
```
Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)
```
<PROJECT_ROOT>\source
<PROJECT_ROOT>\source\utils
<PROJECT_ROOT>\source\component
<PROJECT_ROOT>\resource\facedetectxml
```
where ```<PROJECT_ROOT>``` directory, which was unzipped project.<br>
Additionally, you can specify the path to the library header files FFMPEG
```
<PROJECT_ROOT>\source\ffmpeg
<PROJECT_ROOT>\source\sdl
<PROJECT_ROOT>\source\opengl
```
Examples of using FFMPEG library header files are in the
```
<PROJECT_ROOT>\samples\FFMpeg\FFMPEG.groupproj
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
Examples of use of components.<br>
To install the components, open and install
```
<PROJECT_ROOT>\source\component\Delphi20\RAD Studio XE6.groupproj
```
When installing the components in your PATH variable must be available should the library "opencv_*.dll". 
Must also be installed visual C++ redistributable for Visual Studio 2013 is available on the [link][2]<br>
Examples of the use of components - open:
```
<PROJECT_ROOT>\samples\Components\cCameraCapture\cCameraCapture.dproj
<PROJECT_ROOT>\samples\Components\cMatchTemplate\cMatchTemplate.dproj

```
[1]: https://github.com/Laex/Delphi-OpenCV/archive/master.zip
[2]: http://www.microsoft.com/ru-ru/download/details.aspx?id=40784
