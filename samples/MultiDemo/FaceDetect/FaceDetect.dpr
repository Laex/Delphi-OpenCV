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
// *******************************************************************
// Original: http://public.cranfield.ac.uk/c5354/teaching/ml/examples/c/haar_cascade/haar_cascade.cc
// *******************************************************************
//
// Example : run haar cascade classifier on image / video / camera
// usage: prog {<image_name> | <video_name>}

program FaceDetect;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.Character,
  System.SysUtils,
  System.Math,
  ocv.core.types_c,
  ocv.core_c,
  ocv.highgui_c,
  ocv.objdetect_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.utils,
  uResourcePaths;

Const
  // ******************************************************************************/
  // setup the cameras properly based on OS platform
  CAMERA_INDEX = CV_CAP_ANY;
  // ******************************************************************************/
  windowName = 'Haar Cascade Detection'; // window name
  cascade_name: pCVChar = cResourceFaceDetect + 'haarcascade_frontalface_alt.xml'; // cascade file

Var
  img: pIplImage = nil; // image object
  frame: pIplImage = nil;
  capture: pCvCapture = nil; // capture object
  detected_objects: pCvSeq = nil; // list of detected items
  key: Integer; // user input
  EVENT_LOOP_DELAY: Integer = 40; // delay for GUI window  40 ms equates to 1000ms/25fps=40ms per frame
  cascade: pCvHaarClassifierCascade;
  storage: pCvMemStorage;
  gray: pIplImage;
  imgcopy: pIplImage;
  i: Integer;
  r: pCvRect;
  isCapture: Boolean = false;

begin
  try
    // if command line arguments are provided try to read image/video_name
    // otherwise default to capture from attached H/W camera
    if ParamCount = 1 then
    begin
      img := cvLoadImage(c_str(ParamStr(1)), CV_LOAD_IMAGE_UNCHANGED);
      if not Assigned(img) then
      begin
        capture := cvCreateFileCapture(c_str(ParamStr(1)));
        isCapture := True;
      end;
    end;

    if (not Assigned(img)) and (not Assigned(capture)) then
    begin
      capture := cvCreateCameraCapture(CAMERA_INDEX);
      isCapture := True;
    end;
    if not Assigned(capture) then
      Halt(1);

    // create window object (use flag:=0 to allow resize, 1 to auto fix size)
    cvNamedWindow(windowName, 0);

    // load the trained haar cascade classifier from file
    // and create storage required for detections
    cascade := cvLoad(cascade_name, nil, nil, nil);
    if Assigned(cascade) then
      Writeln('LOADED : ', cascade_name)
    else
    begin
      Writeln('ERROR: Could not load classifier cascade : ', cascade_name);
      Halt(1);
    end;
    try
      storage := cvCreateMemStorage(0);

      // if capture object in use (i.e. video/camera)
      // get initial image from capture object
      if Assigned(capture) then
      begin
        // cvQueryFrame is just a combination of cvGrabFrame
        // and cvRetrieveFrame in one call.
        img := cvQueryFrame(capture);
        if not Assigned(img) then
        begin
          if ParamCount = 1 then
            Writeln('End of video file reached')
          else
            Writeln('ERROR: cannot get next fram from camera');
          Halt(0);
        end;
      end;

      // create a greyscale image upon which to run the classifier
      gray := cvCreateImage(cvSize(img^.width, img^.height), img^.depth, 1);

      // create a copy of the image upon which to do detection and box drawing
      imgcopy := cvCloneImage(img);

      // start main loop
      while True do
      begin
        // if capture object in use (i.e. video/camera)
        // get image from capture object
        if Assigned(capture) then
        begin
          // cvQueryFrame is just a combination of cvGrabFrame
          // and cvRetrieveFrame in one call.
          frame := cvQueryFrame(capture);
          if not Assigned(frame) then
          begin
            if ParamCount = 1 then
              Writeln('End of video file reached')
            else
              Writeln('ERROR: cannot get next fram from camera');
            Halt(0);
          end
        end
        else
        begin
          // if not a capture object set event delay to zero so it waits
          // indefinitely (as single image file, no need to loop)
          EVENT_LOOP_DELAY := 0;
        end;

        // N.B. as haar features are orientation dependent (and
        // the haar function operate directly on the pixel
        // array in memory) we'll just check the image
        // is using Top-Left origin in actual memory
        // and if not flip it (use a copy which is not the
        // capture object buffer)
        if (img^.origin = IPL_ORIGIN_TL) then
        begin
          cvCopy(frame, imgcopy);
        end
        else
        begin
          cvFlip(frame, imgcopy);
          imgcopy^.origin := IPL_ORIGIN_TL;
        end;
        gray^.origin := imgcopy^.origin;

        // convert input image to grayscale
        cvCvtColor(imgcopy, gray, CV_BGR2GRAY);

        // histogram equalize it also to maximize the region differences
        cvEqualizeHist(gray, gray);

        // run the haar cascade detection
        // with parameters scale:=1.2, neighbours := 4 and with Canny pruning
        // turned on with minimum detection scale 30x30 pixels
        detected_objects := cvHaarDetectObjects(gray, cascade, storage, 1.2, 4, CV_HAAR_DO_CANNY_PRUNING, cvSize(30, 30), cvSize(0, 0));

        // draw a red rectangle around any detected objects
        i := 0;
        While i < ifthen(Assigned(detected_objects), detected_objects^.total, 0) do
        begin
          r := pCvRect(cvGetSeqElem(detected_objects, i));
          cvRectangle(imgcopy, cvPoint(r^.x, r^.y), cvPoint((r^.x) + (r^.width), (r^.y) + (r^.height)), CV_RGB(255, 0, 0), 2, 8, 0);
          Inc(i);
        end;
        // if Assigned(detected_objects) then
        // cvClearSeq(detected_objects);
        // cvClearMemStorage(storage);

        // display image in window
        cvShowImage(windowName, imgcopy);

        // start event processing loop (very important,in fact essential for GUI)
        // 40 ms roughly equates to 1000ms/25fps := 4ms per frame
        key := cvWaitKey(EVENT_LOOP_DELAY);
        if key = 27 then
        begin
          // if user presses 'ESC' then exit
          Writeln('Keyboard exit requested : exiting now - bye!');
          Break;
        end;
      end;

      // destroy window objects
      // (triggered by event loop *only* window is closed)
      cvDestroyAllWindows();
      cvReleaseImage(gray);
      cvReleaseImage(imgcopy);
    finally
      if Assigned(img) and (not isCapture) then
        cvReleaseImage(img);
      // destroy image objects (if it does not originate from a capture object)
      if Assigned(capture) then
        cvReleaseCapture(capture);
      if Assigned(storage) then
        cvReleaseMemStorage(storage);
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
