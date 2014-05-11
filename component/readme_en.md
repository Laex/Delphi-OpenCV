Delphi-OpenCV Components
------------------------

Visual and non-visual components for working with the library OpenCV.
Version: OpenCV 2.4.9
Package: OpenCV.dpk

Components:

TocvView
--------
Showcases the work of a visual component to display video camera images

TocvCamera
----------

Showcases the work of non-visual component connection to the camera and image transmission receivers TOpenCVCamera

TocvImageOperation
------------------
Component that handles images

Installation
------------
1. Add to system variable PATH path to DLL libraries OpenCV.
Also may need to specify a path to  msvcp120d.dll, msvcp120.dll, msvcr100d.dll, msvcr100.dll ([link][1])
(if they are not in the same directory). 
2. To install, open <PROJECT_ROOT>\component\OpenCV.dpk. Install package.
3. In the panel component will be part OpenCV.
4. Open the sample
> <PROJECT_ROOT>\Samples\Components\cCameraCapture\cCameraCapture.dpr

Run the sample.
*TODO:*
*Wanted beautiful icons for the components.*
[1]: http://www.microsoft.com/en-US/download/details.aspx?id=30679