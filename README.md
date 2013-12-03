###Delphi-OpenCV (master)
* OpenCV version - 2.4.7<br>
* Development environment - Delphi XE2-XE5<br>

#####Contributors:
```
Laentir Valetov
email: laex@bk.ru

Mikhail Grigorev
email: sleuthhound@gmail.com
```
####How to install:
1.Download the archive ```Delphi-OpenCV-master.zip```<br>
2.Unzip it to a convenient directory, thus get the following directory structure

```
<Directory, such as 'C:\OpenCV\' - <PROJECT_ROOT>>
		<bin>
		<component>
		<include>
		<opencv_classes>
		<samples>
```
3.Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)
```
<PROJECT_ROOT>\Include
<PROJECT_ROOT>\Include\core
<PROJECT_ROOT>\Include\highgui
<PROJECT_ROOT>\Include\imgproc
<PROJECT_ROOT>\Include\legacy
<PROJECT_ROOT>\Include\nonfree
<PROJECT_ROOT>\Include\objdetect
<PROJECT_ROOT>\Include\calib3d
<PROJECT_ROOT>\Include\contrib
<PROJECT_ROOT>\Include\video
<PROJECT_ROOT>\Include\ml
<PROJECT_ROOT>\Include\stitching
<PROJECT_ROOT>\Include\component
```
where ```<PROJECT_ROOT>``` directory, which was unzipped project.

Additionally, you can specify the path to the library header files FFMPEG
```
<PROJECT_ROOT>\Include\ffmpeg
<PROJECT_ROOT>\Include\ffmpeg\ctypes
<PROJECT_ROOT>\Include\ffmpeg\libavcodec
<PROJECT_ROOT>\Include\ffmpeg\libavfilter
<PROJECT_ROOT>\Include\ffmpeg\libavformat
<PROJECT_ROOT>\Include\ffmpeg\libavutil
<PROJECT_ROOT>\Include\ffmpeg\libswscale
```
Examples of using FFMPEG library header files are in the
```
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\ffmpeg\examples
```

4.Open in Delphi IDE and compile:<br>
Examples of the use of certain functions and procedures 
```
<PROJECT_ROOT>\samples\LibDemo.groupproj
```
Examples of the use of video processing algorithms
```
<PROJECT_ROOT>\samples\MultiDemo.groupproj
```
Examples of the use of video processing algorithms using VCL.Forms
```
<PROJECT_ROOT>\samples\VCLDemo.groupproj
```
Examples of use of components.<br>
To install the components, open and install
```
<PROJECT_ROOT>\Include\component\OpenCV.dpk
<PROJECT_ROOT>\samples\ComponentDemo.groupproj
```