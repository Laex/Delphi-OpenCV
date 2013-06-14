Delphi-OpenCV
===============
* OpenCV version - 2.4.5<br>
* Development environment - Delphi XE2-XE4<br>

Install:
----------------

* Download the archive <a href="https://github.com/Laex/Delphi-OpenCV/archive/master.zip" title="Title">Delphi-OpenCV</a><br>
* Unzip it to a convenient directory, thus get the following directory structure

```
<Directory, such as 'C:\OpenCV' or just 'C:\' - <PROJECT_ROOT>>
	<Delphi-OpenCV-master>
		<Bin>
		<include>
		<opencv_classes>
		<samples>
```
* Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)

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
```
where ```<PROJECT_ROOT>``` directory, which was unzipped project.

* You can unzip files subdirectory of the project without <Delphi-OpenCV-master>.<br>
Then the directory structure of the project is as follows:

```
<Directory, such as 'C:\OpenCV' - <PROJECT_ROOT>>
	<Bin>
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
<PROJECT_ROOT>\component\
```

* Open ```<PROJECT_ROOT>\samples\LibTest.groupproj``` in Delphi IDE and compile the ```LibTest``` programs.<br>
  Open ```<PROJECT_ROOT>\samples\MultiDemo.groupproj``` in Delphi IDE and compile the ```MultiDemo``` programs.<br>
  Open ```<PROJECT_ROOT>\samples\ClassDemo.groupproj``` in Delphi IDE and compile the ```ClassDemo``` programs.<br>

* Open ```<PROJECT_ROOT>\samples\OpenCV.dproj``` in Delphi IDE and install OCV components<br>
  Open ```<PROJECT_ROOT>\ComponentDemo.groupproj``` in Delphi IDE and compile the ```ComponentDemo``` programs.
