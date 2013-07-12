###Delphi-OpenCV
* OpenCV version - 2.4.6<br>
* Development environment - Delphi XE2-XE4<br>

#####Contributors:
```
Laentir Valetov
email:laex@bk.ru

Mikhail Grigorev
email: sleuthhound@gmail.com
```
####How to install:
1.Download the archive ```Delphi-OpenCV-master.zip```<br>
2.Unzip it to a convenient directory, thus get the following directory structure

```
<Directory, such as 'C:\OpenCV' or just 'C:\' - <PROJECT_ROOT>>
	<Delphi-OpenCV-master>
		<bin>
		<component>
		<include>
		<opencv_classes>
		<samples>
```
3.Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)
```
<PROJECT_ROOT>\Delphi-OpenCV-master\Include
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\core
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\highgui
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\imgproc
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\legacy
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\nonfree
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\objdetect
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\calib3d
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\contrib
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\video
<PROJECT_ROOT>\Delphi-OpenCV-master\component
```
where ```<PROJECT_ROOT>``` directory, which was unzipped project.

4.You can unzip files subdirectory of the project without <Delphi-OpenCV-master>.<br>
Then the directory structure of the project is as follows:
```
<Directory, such as 'C:\OpenCV' - <PROJECT_ROOT>>
	<bin>
	<component>
	<include>
	<opencv_classes>
	<samples>
```
and path
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
<PROJECT_ROOT>\component
```
5.Open in Delphi IDE and compile:<br> 
```
<PROJECT_ROOT>\samples\LibTest.groupproj
<PROJECT_ROOT>\samples\MultiDemo.groupproj
<PROJECT_ROOT>\samples\ClassDemo.groupproj
```
for use OCV component - install ```<PROJECT_ROOT>\component\OpenCV.dpk```
and then open in Delphi IDE and compile ```<PROJECT_ROOT>\samples\ComponentDemo.groupproj```