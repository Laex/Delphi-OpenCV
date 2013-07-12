###Delphi-OpenCV
* Версия OpenCV - 2.4.6<br>
* Среда разработки - Delphi XE2-XE4<br>

#####Разработчики:
```
Laentir Valetov
email:laex@bk.ru

Mikhail Grigorev
email: sleuthhound@gmail.com
```
####Установка:
1.Скачайте архив ```Delphi-OpenCV-master.zip```<br>
2.Разархивируйте в выбранное Вами место, должна получиться следующая структура каталогов

```
<Корневая директория 'C:\OpenCV' или 'C:\', далее обозначена <PROJECT_ROOT>>
	<Delphi-OpenCV-master>
		<bin>
		<component>
		<include>
		<opencv_classes>
		<samples>
```
3.Добавьте пути к файлам библиотеки в Delphi IDE (Tools-Options-Delphi Options-Library-Library path)
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
где ```<PROJECT_ROOT>``` каталог куда разархивированы файлы.

4.Откройте в Delphi IDE и откомпилируйте:<br> 
```
<PROJECT_ROOT>\samples\LibTest.groupproj - базовые примеры использования OpenCV
<PROJECT_ROOT>\samples\MultiDemo.groupproj - примеры комплексного использования функций
<PROJECT_ROOT>\samples\ClassDemo.groupproj - примеры использования интерфейсов к классам OpenCV
```
для использования OCV компонент - инсталируйте ```<PROJECT_ROOT>\component\OpenCV.dpk```
и затем откройте в Delphi IDE и откомпилируйте ```<PROJECT_ROOT>\samples\ComponentDemo.groupproj```