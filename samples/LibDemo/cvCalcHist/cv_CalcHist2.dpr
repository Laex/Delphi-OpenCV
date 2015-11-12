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
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1_1Final.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
// *****************************************************************
// Original: https://github.com/joshdoe/opencv-clahe/blob/master/adapthistequal.cpp
// ****************************************************************
// Example : adaptive histogram equalise grayscale image
// usage: prog {<image_name> | <video_name>}
// ****************************************************************

program cv_CalcHist2;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.utils;

Const
  CAMERA_INDEX = CV_CAP_ANY;

  // function that takes a gray scale image and draws a histogram
  // image for it in a pre-allocated image

procedure create_histogram_image(grayImg: pIplImage; histogramImage: pIplImage);
Var
  hist     : pCvHistogram; // pointer to histogram object
  max_value: Float;        // max value in histogram
  hist_size: Integer;      // size of histogram (number of bins)
  bin_w    : Integer;      // initial width to draw bars
  range_0  : array [0 .. 1] of Float;
  ranges   : pFloat;
  i        : Integer;
begin
  hist       := Nil; // pointer to histogram object
  max_value  := 0;   // max value in histogram
  hist_size  := 256; // size of histogram (number of bins)
  bin_w      := 0;   // initial width to draw bars
  range_0[0] := 0;
  range_0[1] := 256;
  ranges     := @range_0;

  hist := cvCreateHist(
    1,
    @hist_size,
    CV_HIST_ARRAY,
    @ranges,
    1);

  cvCalcHist(
    grayImg,
    hist,
    0,
    nil);
  cvGetMinMaxHistValue(
    hist,
    0,
    @max_value);
  cvScale(
    hist^.bins,
    hist^.bins,
    histogramImage^.height / max_value,
    0);
  cvSet(
    histogramImage,
    cvScalarAll(255),
    0);
  bin_w := cvRound(histogramImage^.width / hist_size);

  for i := 0 to hist_size - 1 do
  begin
    cvRectangle(
      histogramImage,
      cvPoint(i * bin_w, histogramImage^.height),
      cvPoint((i + 1) * bin_w, histogramImage^.height - cvRound(cvGetReal1D(hist^.bins, i))),
      cvScalarAll(0),
      -1,
      8,
      0);
  end;

  cvReleaseHist(hist);
end;

Const
  windowName1  = 'Grayscale';                          // window name
  windowNameH1 = 'Adaptive Equalised Histogram';       // window name
  windowNameH2 = 'Original Histogram';                 // window name

Var
  img    : pIplImage  = nil; // image object
  capture: pCvCapture = nil; // capture object

  eqHistogramImage  : pIplImage = nil; // histogram images
  grayHistogramImage: pIplImage = nil;

  keepProcessing  : Boolean = true; // loop control flag
  key             : Integer;        // user input
  EVENT_LOOP_DELAY: Integer = 40;   // delay for GUI window
  // 40 ms equates to 1000ms/25fps:=40ms per frame

  xdivs        : Integer = 2;
  ydivs        : Integer = 2;
  bins         : Integer = 256;
  limit_counter: Integer = 14;

  grayImg    : pIplImage = nil;
  eqImg      : pIplImage = nil;
  histEqImage: pIplImage = nil;

begin
  try
    // if command line arguments are provided try to read image/video_name
    // otherwise default to capture from attached H/W camera
    if ParamCount = 1 then
    begin
      img := cvLoadImage(
        c_str(ParamStr(1)),
        1);
      if not Assigned(img) then
        capture := cvCreateFileCapture(c_str(ParamStr(1)));
    end;
    if (not Assigned(img)) and (not Assigned(capture)) then
      capture := cvCreateCameraCapture(CAMERA_INDEX);

    if (not Assigned(img)) and (not Assigned(capture)) then
      Halt(1);

    // create window object (use flag=0 to allow resize, 1 to auto fix size)

//    cvNamedWindow(
//      windowName,
//      1); // flag set to 1 by Shervin Emami, 17Nov2010.
    cvNamedWindow(
      windowName1,
      1); // flag set to 1 by Shervin Emami, 17Nov2010.
    cvNamedWindow(
      windowNameH1,
      1); // flag set to 1 by Shervin Emami, 17Nov2010.
    cvNamedWindow(
      windowNameH2,
      1); // flag set to 1 by Shervin Emami, 17Nov2010.

    cvNamedWindow(
      'Simple Histogram Equalization',
      1); // Added by Shervin Emami, 17Nov2010.

//    cvCreateTrackbar(
//      'X cells',
//      windowName,
//      @xdivs,
//      16,
//      nil);
//    cvCreateTrackbar(
//      'Y cells',
//      windowName,
//      @ydivs,
//      16,
//      nil);
//    cvCreateTrackbar(
//      'bins',
//      windowName,
//      @bins,
//      256,
//      nil);
//    cvCreateTrackbar(
//      'limit (x 0.1)',
//      windowName,
//      @limit_counter,
//      30,
//      nil);

    // define required images for intermediate processing
    // (if using a capture object we need to get a frame first to get the size)

    if Assigned(capture) then
    begin

      // cvQueryFrame s just a combination of cvGrabFrame
      // and cvRetrieveFrame in one call.

      img := cvQueryFrame(capture);
      if not Assigned(img) then
      begin
        if ParamCount = 1 then
        begin
          WriteLn('End of video file reached');
        end
        else
        begin
          WriteLn('ERROR: cannot get next fram from camera');
        end;
        Halt(0);
      end;

    end;

    grayImg := cvCreateImage(
      cvSize(img^.width, img^.height),
      img^.depth,
      1);
    grayImg^.origin := img^.origin;
    eqImg           := cvCreateImage(
      cvSize(img^.width, img^.height),
      img^.depth,
      1);
    eqImg^.origin := img^.origin;

    eqHistogramImage := cvCreateImage(
      cvSize(255, 200),
      8,
      1);
    grayHistogramImage := cvCreateImage(
      cvSize(255, 200),
      8,
      1);

    // start main loop

    while (keepProcessing) do
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
          if ParamCount = 1 then
          begin
            WriteLn('End of video file reached');
          end
          else
          begin
            WriteLn('ERROR: cannot get next fram from camera');
          end;
          break;
        end;

      end
      else
      begin

        // if not a capture object set event delay to zero so it waits
        // indefinitely (as single image file, no need to loop)

        EVENT_LOOP_DELAY := 0;
      end;

      // *** Histogram Equalisation Processing

      // if input is not already grayscale, convert to grayscale

      if (img^.nChannels > 1) then
      begin
        cvCvtColor(
          img,
          grayImg,
          CV_BGR2GRAY);
      end
      else
      begin
        grayImg := img;
      end;

       // ***

      // *** draw histograms

      create_histogram_image(
        grayImg,
        grayHistogramImage);
      create_histogram_image(
        eqImg,
        eqHistogramImage);

      // display image in window

//      cvShowImage(
//        windowName,
//        eqImg);
      cvShowImage(
        windowName1,
        grayImg);

      cvShowImage(
        windowNameH1,
        eqHistogramImage);
      cvShowImage(
        windowNameH2,
        grayHistogramImage);

      // Simple Histogram Equalization. Added by Shervin Emami, 17Nov2010.
      histEqImage := cvCreateImage(
        cvGetSize(grayImg),
        grayImg^.depth,
        grayImg^.nChannels);
      cvEqualizeHist(
        grayImg,
        histEqImage);
      cvShowImage(
        'Simple Histogram Equalization',
        histEqImage);
      cvReleaseImage(&histEqImage);

      // start event processing loop (very important,in fact essential for GUI)
      // 4 ms roughly equates to 100ms/25fps:=4ms per frame

      key := cvWaitKey(EVENT_LOOP_DELAY);

      if (key = 27) then
      begin // 'Esc' key added by Shervin Emami, 17Nov2010.

        // if user presses 'x' or 'Esc' then exit

        WriteLn('Keyboard exit requested : exiting now - bye!');
        keepProcessing := false;
      end;
    end;

    // destroy window objects
    // (triggered by event loop *only* window is closed)

    cvDestroyAllWindows();

    // destroy image object (if it does not originate from a capture object)

    if not Assigned(capture) then
    begin
      cvReleaseImage(img);
    end;
    cvReleaseImage(grayImg);
    cvReleaseImage(eqImg);

    if Assigned(capture) then
      cvReleaseCapture(capture);

    // release histogram images

    cvReleaseImage(eqHistogramImage);
    cvReleaseImage(grayHistogramImage);

  except
    on E: Exception do
      WriteLn(
        E.ClassName,
        ': ',
        E.Message);
  end;

end.
