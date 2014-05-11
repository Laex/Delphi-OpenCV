###Delphi-OpenCV
----------
* OpenCV version - 2.4.9<br>
* Development environment - Delphi XE2-XE6<br>

----------
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
		<bin>
		<component>
		<include>
		<opencv_classes>
		<samples>
```
Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)
```
<PROJECT_ROOT>\include
<PROJECT_ROOT>\include\calib3d
<PROJECT_ROOT>\include\contrib
<PROJECT_ROOT>\include\core
<PROJECT_ROOT>\include\features2d
<PROJECT_ROOT>\include\ffmpeg
<PROJECT_ROOT>\include\highgui
<PROJECT_ROOT>\include\imgproc
<PROJECT_ROOT>\include\legacy
<PROJECT_ROOT>\include\ml
<PROJECT_ROOT>\include\nonfree
<PROJECT_ROOT>\include\objdetect
<PROJECT_ROOT>\include\stitching
<PROJECT_ROOT>\include\video
<PROJECT_ROOT>\component
```
where ```<PROJECT_ROOT>``` directory, which was unzipped project.<br>
Additionally, you can specify the path to the library header files FFMPEG
```
<PROJECT_ROOT>\include\ffmpeg
<PROJECT_ROOT>\include\ffmpeg\ctypes
<PROJECT_ROOT>\include\ffmpeg\libavcodec
<PROJECT_ROOT>\include\ffmpeg\libavfilter
<PROJECT_ROOT>\include\ffmpeg\libavformat
<PROJECT_ROOT>\include\ffmpeg\libavutil
<PROJECT_ROOT>\include\ffmpeg\libswscale
```
Examples of using FFMPEG library header files are in the
```
<PROJECT_ROOT>\include\ffmpeg\examples
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
<PROJECT_ROOT>\include\component\OpenCV.dpk
```
When installing the components in your PATH variable must be available should the library "opencv_*.dll". 
Must also be installed visual C + + redistributable for Visual Studio 2012 is available on the [link][2]<br>
Examples of the use of components - open:
```
<PROJECT_ROOT>\samples\Components\cCameraCapture\cCameraCapture.dproj
```
[1]: https://github.com/Laex/Delphi-OpenCV/archive/master.zip
[2]: http://www.microsoft.com/en-US/download/details.aspx?id=30679