ARCHIVAL — for reference only. Unsuccessful experiment; use Delphi-OpenCV5 instead:
  https://github.com/Laex/Delphi-OpenCV5
  See README.md in this folder.

OpenCV C++ API support for Delphi-OpenCV
========================================

Pascal bindings (ocv.cls.*.pas) and the native proxy DLL (opencv_classes/)
for calling OpenCV C++ classes from Delphi via stdcall exports.

Layout:
  ocv.cls.*.pas       - Delphi object wrappers
  opencv_classes/     - Visual C++ DLL project (build opencv_classes.dll)
  examples/           - Sample projects (open examples/Classes.groupproj)

Requires OpenCV 2.4.x C++ libraries and the opencv_classes DLL next to
sample executables (see source/ocv.lib.pas, opencv_classes_lib).

Wrapped API (partial):

objdetect.hpp
	class CascadeClassifier - not fully
highgui.hpp
	class VideoCapture
	procedure namedWindow
	procedure destroyWindow
	procedure destroyAllWindows
	function startWindowThread
	function waitKey
	procedure imshow
	procedure resizeWindow
	procedure moveWindow
	procedure setWindowProperty
	function getWindowProperty
	function createTrackbar
	function imread
core.hpp
	class Mat
	function setBreakOnError
	function redirectError
	procedure setNumThreads
	function getNumThreads
	function getThreadNum
	function getTickCount
	function getTickFrequency
	function getCPUTickCount
	function checkHardwareSupport
	function getNumberOfCPUs
	function fastMalloc
	procedure fastFree
	procedure setUseOptimized
	function useOptimized
contrib.hpp
	class FaceRecognizer