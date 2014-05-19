Visual and non-visual components for working with the library OpenCV
------------------------
Version: OpenCV 2.4.9

Requires installed [Visual C++ redistributable for Visual Studio 2013][1]<br>

Components:

TocvView
--------
Showcases the work of a visual component to display video camera images

TocvCamera, TocvFileSource, TocvIPCamSource
----------

Showcases the work of non-visual component connection to the video source 

TocvImageOperation
------------------
Component that handles images

> TocvNoneOperation,  TocvGrayScaleOperation,  TovcCannyOperation, 
> TovcSmoothOperation,  TovcErodeOperation, TovcDilateOperation, 
> TocvLaplaceOperation,  TovcSobelOperation,  TocvThresholdOperation, 
> TocvAdaptiveThresholdOperation,  TocvContoursOperation, 
> TocvRotateOperation,  TocvAbsDiff,  TocvHaarCascade, 
> TocvMatchTemplate, TocvMotionDetect

Installation
------------
1. Add to system variable PATH path to DLL libraries OpenCV.
2. To install, open <PROJECT_ROOT>\component\Delphi20\RAD Studio XE6.groupproj. Install package.
3. In the panel component will be part OpenCV.
4. Open the sample<br>
```
<PROJECT_ROOT>\samples\Components\cCameraCapture\cCameraCapture.dproj
<PROJECT_ROOT>\samples\Components\cMatchTemplate\cMatchTemplate.dproj
```

Run the sample.

[1]: http://www.microsoft.com/ru-ru/download/details.aspx?id=40784