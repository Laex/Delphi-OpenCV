Delphi-OpenCV
-------------
* OpenCV version - 2.4.5<br>
* Development environment - Delphi XE3<br>

How to install:
---------------

##1. Download the archive Delphi-OpenCV-master.zip<br>
##2. Unzip it to a convenient directory, thus get the following directory structure<br>

<Directory, such as "C:\OpenCV" or just "C:\" - <PROJECT_ROOT>><br>
	<Delphi-OpenCV-master><br>
		<Bin><br>
		<include><br>
		<opencv_classes><br>
		<samples><br>

##3. Add the search path for the modules of the project in Delphi IDE (Tools-Options-Delphi Options-Library-Library path)

<PROJECT_ROOT>\Delphi-OpenCV-master\Include<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\core<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\highgui<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\imgproc<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\legacy<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\nonfree<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\objdetect<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\calib3d<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\contrib<br>
<PROJECT_ROOT>\Delphi-OpenCV-master\Include\video<br>

where <PROJECT_ROOT> directory, which was unzipped project.

##4. You can unzip files subdirectory of the project without <Delphi-OpenCV-master>.<br>
Then the directory structure of the project is as follows:

<Directory, such as "C:\OpenCV" - <PROJECT_ROOT>><br>
	<Bin><br>
	<include><br>
	<opencv_classes><br>
	<samples><br>

and path

<PROJECT_ROOT>\Include<br>
<PROJECT_ROOT>\Include\core<br>
<PROJECT_ROOT>\Include\highgui<br>
<PROJECT_ROOT>\Include\imgproc<br>
<PROJECT_ROOT>\Include\legacy<br>
<PROJECT_ROOT>\Include\nonfree<br>
<PROJECT_ROOT>\Include\objdetect<br>
<PROJECT_ROOT>\Include\calib3d<br>
<PROJECT_ROOT>\Include\contrib<br>
<PROJECT_ROOT>\Include\video<br>

##5. Open "<PROJECT_ROOT>\samples\Samples.groupproj" in Delphi IDE and compile the sample programs.