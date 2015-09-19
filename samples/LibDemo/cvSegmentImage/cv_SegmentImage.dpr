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

program cv_SegmentImage;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc.types_c,
  ocv.imgproc_c,
  ocv.legacy,
  ocv.utils;

const
  CAMERA_INDEX = CV_CAP_ANY;

Var
  img    : pIplImage  = nil; // image object
  capture: pCvCapture = nil; // capture object

  windowName: pCvChar = 'Edge Based Segmentation'; // window name

  keepProcessing  : Boolean = true; // loop control flag
  key             : Integer;        // user input
  EVENT_LOOP_DELAY: Integer = 40;   // delay for GUI window
  // 40 ms equates to 1000ms/25fps := 40ms per frame

  canny    : Integer = 200; // canny input parameter
  floodFill: Integer = 50;  // flood fill parameter
  segmented: pIplImage;

begin
  try
    // if command line arguments are provided try to read image/video_name
    // otherwise default to capture from attached H/W camera
    if ParamCount = 1 then
    begin
      img := cvLoadImage(
        c_str(ParamStr(1)),
        CV_LOAD_IMAGE_UNCHANGED);
      if not Assigned(img) then
        capture := cvCreateFileCapture(c_str(ParamStr(1)));
    end;

    if (not Assigned(img)) and (not Assigned(capture)) then
      capture := cvCreateCameraCapture(CAMERA_INDEX);
    if (not Assigned(capture)) and (not Assigned(img)) then
      Halt(1);

    // create window objects (use flag:=0 to allow resize, 1 to auto fix size)

    cvNamedWindow(
      windowName,
      0);

    cvCreateTrackbar(
      'Canny',
      windowName,
      @canny,
      255,
      nil);
    cvCreateTrackbar(
      'Fill',
      windowName,
      @floodFill,
      255,
      nil);

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

    segmented := cvCreateImage(
      cvSize(img^.width, img^.height),
      img^.depth,
      img^.nChannels);
    segmented^.origin := img^.origin;


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

      cvSegmentImage(
        img,
        segmented,
        canny,
        floodFill,
        nil);

      // display image in window

      cvShowImage(
        windowName,
        segmented);

      // start event processing loop (very important,in fact essential for GUI)
      // 4 ms roughly equates to 100ms/25fps := 4ms per frame
      key := cvWaitKey(EVENT_LOOP_DELAY);

      if key = 27 then
      begin
        // if user presses 'ESC' then exit
        WriteLn('Keyboard exit requested : exiting now - bye!');
        Break;
      end;
    end;

    // destroy window objects
    // (triggered by event loop *only* window is closed)
    cvDestroyAllWindows();
    // destroy image object (if it does not originate from a capture object)
    if not Assigned(capture) then
      cvReleaseImage(img);
    cvReleaseImage(segmented);
    if Assigned(capture) then
      cvReleaseCapture(capture);

  except
    on E: Exception do
      WriteLn(
        E.ClassName,
        ': ',
        E.Message);
  end;

end.
