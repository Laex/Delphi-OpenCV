*****************************************************************
*                       Delphi-OpenCV Demo                      *
*               Copyright (C) 2013 Project Delphi-OpenCV        *
*****************************************************************

Visual and non-visual components for working with the library OpenCV.
Version: OpenCV 2.4.9
Package: OpenCV.dpk

The demo version.

Components:

----- TocvView -------
Showcases the work of a visual component to display
video camera images

----- TocvCamera -----
Showcases the work of non-visual component connection
to the camera and image transmission receivers TOpenCVCamera

----- TocvImageOperation -----
Component that handles images

----- TocvSplitter -----
Transmits the image to multiple receivers

Attention! Components written in a very non-optimal and require
significant improvement. The use of real-world projects
is not recommended.
Further components will be refined and improved.

----- Installation -------
1. Add to system variable PATH path to DLL libraries OpenCV.
Also may need to specify a path to  msvcp100d.dll and
msvcr100d.dll (if they are not in the same directory)

2. To install, open <PROJECT_ROOT>\component\OpenCV.dpk.
Install package.

3. In the panel component will be part OpenCV.

4. Open the sample
<PROJECT_ROOT>\Samples\Components\cCameraCapture\cCameraCapture.dpr
Run the sample.

TODO:
Wanted beautiful icons for the components.