// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// ****************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// ****************************************************************
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the 'License'); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *******************************************************************

program cv_DFT;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.utils;

const
  CAMERA_INDEX = CV_CAP_ANY;

  // Rearrange the quadrants of Fourier image so that the origin is at
  // the image center
  // src & dst arrays of equal size & type
procedure cvShiftDFT(src_arr: pCvArr; dst_arr: pCvArr);
Var
  tmp: pCvMat;
  q1stub, q2stub: TCvMat;
  q3stub, q4stub: TCvMat;
  d1stub, d2stub: TCvMat;
  d3stub, d4stub: TCvMat;
  q1, q2, q3, q4: pCvMat;
  d1, d2, d3, d4: pCvMat;

  size: TCvSize;
  dst_size: TCvSize;
  cx, cy: Integer;
begin
  tmp := nil;
  size := cvGetSize(src_arr);
  dst_size := cvGetSize(dst_arr);

  if (dst_size.width <> size.width) or (dst_size.height <> size.height) then
  begin
    cvError(CV_StsUnmatchedSizes, 'cvShiftDFT', 'Source and Destination arrays must have equal sizes', 'cv_DFT', 65);
  end;

  if (src_arr = dst_arr) then
    tmp := cvCreateMat(size.height div 2, size.width div 2, cvGetElemType(src_arr));

  cx := size.width div 2;
  cy := size.height div 2; // image center

  q1 := cvGetSubRect(src_arr, @q1stub, cvRect(0, 0, cx, cy));
  q2 := cvGetSubRect(src_arr, @q2stub, cvRect(cx, 0, cx, cy));
  q3 := cvGetSubRect(src_arr, @q3stub, cvRect(cx, cy, cx, cy));
  q4 := cvGetSubRect(src_arr, @q4stub, cvRect(0, cy, cx, cy));
  d1 := cvGetSubRect(src_arr, @d1stub, cvRect(0, 0, cx, cy));
  d2 := cvGetSubRect(src_arr, @d2stub, cvRect(cx, 0, cx, cy));
  d3 := cvGetSubRect(src_arr, @d3stub, cvRect(cx, cy, cx, cy));
  d4 := cvGetSubRect(src_arr, @d4stub, cvRect(0, cy, cx, cy));

  if (src_arr <> dst_arr) then
  begin
    if not CV_ARE_TYPES_EQ(q1, d1) then
    begin
      cvError(CV_StsUnmatchedFormats, 'cvShiftDFT', 'Source and Destination arrays must have the same format', 'cv_DFT', 120);
    end;
    cvCopy(q3, d1);
    cvCopy(q4, d2);
    cvCopy(q1, d3);
    cvCopy(q2, d4);
  end
  else
  begin
    cvCopy(q3, tmp);
    cvCopy(q1, q3);
    cvCopy(tmp, q1);
    cvCopy(q4, tmp);
    cvCopy(q2, q4);
    cvCopy(tmp, q2);

    cvReleaseMat(tmp);
  end;
end;

Var
  img: pIplImage = nil; // image object
  capture: pCvCapture = nil; // capture object

  originalName: pCvChar = 'Original Image (grayscale)'; // window name
  magnitudeName: pCvChar = 'Magnitude Image (log transformed)'; // window name

  keepProcessing: Boolean = true; // loop control flag
  key: Integer; // user input
  EVENT_LOOP_DELAY: Integer = 40; // delay for GUI window
  // 40 ms equates to 1000ms/25fps := 40ms per frame

  realInput: pIplImage;
  imaginaryInput: pIplImage;
  complexInput: pIplImage;

  dft_M, dft_N: Integer;

  dft_A: pCvMat;
  tmp: TCvMat;
  image_Re: pIplImage;
  image_Im: pIplImage;
  grayImg: pIplImage;
  _m, _mm: Double;

begin
  try
    // if command line arguments are provided try to read image/video_name
    // otherwise default to capture from attached H/W camera
    if ParamCount = 1 then
    begin
      img := cvLoadImage(c_str(ParamStr(1)), CV_LOAD_IMAGE_UNCHANGED);
      if not Assigned(img) then
        capture := cvCreateFileCapture(c_str(ParamStr(1)));
    end;

    if (not Assigned(img)) and (not Assigned(capture)) then
      capture := cvCreateCameraCapture(CAMERA_INDEX);
    if (not Assigned(capture)) and (not Assigned(img)) then
      Halt(1);

    // create window objects (use flag:=0 to allow resize, 1 to auto fix size)

    cvNamedWindow(originalName, 0);
    cvNamedWindow(magnitudeName, 0);

    // define required floating point images for DFT processing
    // (if using a capture object we need to get a frame first to get the size)

    if Assigned(capture) then
    begin
      // cvQueryFrame s just a combination of cvGrabFrame
      // and cvRetrieveFrame in one call.
      img := cvQueryFrame(capture);
      if not Assigned(img) then
      begin
        if (ParamCount = 1) then
          WriteLn('End of video file reached')
        else
          WriteLn('ERROR: cannot get next fram from camera');

        cvReleaseCapture(capture);
        Halt(1);
      end;
    end;
    realInput := cvCreateImage(cvGetSize(img), IPL_DEPTH_64F, 1);
    imaginaryInput := cvCreateImage(cvGetSize(img), IPL_DEPTH_64F, 1);
    complexInput := cvCreateImage(cvGetSize(img), IPL_DEPTH_64F, 2);

    dft_M := cvGetOptimalDFTSize(img^.height - 1);
    dft_N := cvGetOptimalDFTSize(img^.width - 1);

    dft_A := cvCreateMat(dft_M, dft_N, CV_64FC2);
    image_Re := cvCreateImage(cvSize(dft_N, dft_M), IPL_DEPTH_64F, 1);
    image_Im := cvCreateImage(cvSize(dft_N, dft_M), IPL_DEPTH_64F, 1);

    // define grayscale image
    grayImg := cvCreateImage(cvSize(img^.width, img^.height), img^.depth, 1);
    grayImg^.origin := img^.origin;

    // start main loop

    while true do
    begin
      // if capture object in use (i.e. video/camera)
      // get image from capture object
      if Assigned(capture) then
      begin
        // cvQueryFrame s just a combination of cvGrabFrame
        // and cvRetrieveFrame in one call.
        img := cvQueryFrame(capture);
        if not Assigned(img) then
        begin
          if (ParamCount = 1) then
            WriteLn('End of video file reached')
          else
            WriteLn('ERROR: cannot get next fram from camera\n');
          Halt(1);
        end;
      end
      else
        // if not a capture object set event delay to zero so it waits
        // indefinitely (as single image file, no need to loop)
        EVENT_LOOP_DELAY := 0;

      // *** Fourier processing

      // if input is not already grayscale, convert to grayscale

      if (img^.nChannels > 1) then
        cvCvtColor(img, grayImg, CV_BGR2GRAY)
      else
        grayImg := img;

      cvScale(grayImg, realInput, 1.0, 0.0);
      cvZero(imaginaryInput);
      cvMerge(realInput, imaginaryInput, nil, nil, complexInput);

      // copy A to dft_A and pad dft_A with zeros
      cvGetSubRect(dft_A, @tmp, cvRect(0, 0, grayImg^.width, grayImg^.height));
      cvCopy(complexInput, @tmp, nil);
      cvGetSubRect(dft_A, @tmp, cvRect(img^.width, 0, dft_A^.cols - grayImg^.width, grayImg^.height));
      if ((dft_A^.cols - grayImg^.width) > 0) then
        cvZero(@tmp);

      // no need to pad bottom part of dft_A with zeros because of
      // use nonzero_rows parameter in cvDFT() call below

      cvDFT(dft_A, dft_A, CV_DXT_FORWARD, complexInput^.height);

      // Split Fourier in real and imaginary parts
      cvSplit(dft_A, image_Re, image_Im, nil, nil);

      // Compute the magnitude of the spectrum Mag := sqrt(Re^2 + Im^2)
      cvPow(image_Re, image_Re, 2.0);
      cvPow(image_Im, image_Im, 2.0);
      cvAdd(image_Re, image_Im, image_Re, nil);
      cvPow(image_Re, image_Re, 0.5);

      // Compute log(1 + Mag)
      cvAddS(image_Re, cvScalarAll(1.0), image_Re, nil); // 1 + Mag
      cvLog(image_Re, image_Re); // log(1 + Mag)

      // Rearrange the quadrants of Fourier image so that the origin is at
      // the image center
      cvShiftDFT(image_Re, image_Re);

      // scale image for display
      cvMinMaxLoc(image_Re, @_m, @_mm, nil, nil, nil);
      cvScale(image_Re, image_Re, 1.0 / (_mm - _m), 1.0 * (-_m) / (_mm - _m));

      // ***

      // display image in window

      cvShowImage(originalName, grayImg);
      cvShowImage(magnitudeName, image_Re);

      // start event processing loop (very important,in fact essential for GUI)
      // 4 ms roughly equates to 100ms/25fps := 4ms per frame

      key := cvWaitKey(EVENT_LOOP_DELAY);

      if key = 27 then
      begin

        // if user presses 'x' then exit

        WriteLn('Keyboard exit requested : exiting now - bye!\n');
        Break;
      end;
    end;

    // destroy window objects
    // (triggered by event loop *only* window is closed)
    cvDestroyAllWindows();

    // destroy image object (if it does not originate from a capture object)

    if not Assigned(capture) then
      cvReleaseImage(img);


    // release other images

    cvReleaseMat(dft_A);
    if Assigned(grayImg) then
      cvReleaseImage(grayImg);

    cvReleaseImage(realInput);
    cvReleaseImage(imaginaryInput);
    cvReleaseImage(complexInput);
    cvReleaseImage(image_Re);
    cvReleaseImage(image_Im);

    if Assigned(capture) then
      cvReleaseCapture(capture);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
