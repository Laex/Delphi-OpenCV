Заголовочные файлы OpenCV 2.4.6 для Delphi XE2-XE4
==================================================

Разработчики:
-------------

Лаентир Валетов
Email: laex@bk.ru

Михаил Григорьев
Email: sleuthhound@gmail.com


Инструкция по установке:
------------------------

1. Скачайте архив Delphi-OpenCV-master.zip
2. Разархивируйте его в удобный для Вас каталог, при этом получится следующая структура каталогов

<Ваш каталог, например "C:\OpenCV" или просто "C:\" - <PROJECT_ROOT>>
	<Delphi-OpenCV-master>
		<Bin>
		<include>
		<opencv_classes>
		<samples>

3. Добавьте пути поиска к модулям проекта в Delphi IDE (Tools-Options-Delphi Options-Library-Library path)

<PROJECT_ROOT>\Delphi-OpenCV-master\include
<PROJECT_ROOT>\Delphi-OpenCV-master\include\core
<PROJECT_ROOT>\Delphi-OpenCV-master\include\highgui
<PROJECT_ROOT>\Delphi-OpenCV-master\include\imgproc
<PROJECT_ROOT>\Delphi-OpenCV-master\include\legacy
<PROJECT_ROOT>\Delphi-OpenCV-master\include\nonfree
<PROJECT_ROOT>\Delphi-OpenCV-master\include\objdetect
<PROJECT_ROOT>\Delphi-OpenCV-master\include\calib3d
<PROJECT_ROOT>\Delphi-OpenCV-master\include\contrib
<PROJECT_ROOT>\Delphi-OpenCV-master\include\video

где <PROJECT_ROOT> каталог, в который был разархивирован проект.

4. Вы можете разархивировать фалы проекта без подкаталога <Delphi-OpenCV-master>.
Тогда структура каталогов проекта будет следующая:

<Ваш каталог, например "C:\OpenCV" - <PROJECT_ROOT>>
	<Bin>
	<include>
	<opencv_classes>
	<samples>

и пути

<PROJECT_ROOT>\include
<PROJECT_ROOT>\include\core
<PROJECT_ROOT>\include\highgui
<PROJECT_ROOT>\include\imgproc
<PROJECT_ROOT>\include\legacy
<PROJECT_ROOT>\include\nonfree
<PROJECT_ROOT>\include\objdetect
<PROJECT_ROOT>\include\calib3d
<PROJECT_ROOT>\include\contrib
<PROJECT_ROOT>\include\video
	
5. Откройте "<PROJECT_ROOT>\samples\LibTest.groupproj" или
   "<PROJECT_ROOT>\samples\MultiDemo.groupproj" в Delphi IDE и скомпилируйте
   примеры программ.
