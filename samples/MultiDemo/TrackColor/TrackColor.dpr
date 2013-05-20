(* /*****************************************************************
  //                       Delphi-OpenCV Demo
  //               Copyright (C) 2013 Project Delphi-OpenCV
  // ****************************************************************
  // Contributor:
  // laentir Valetov
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
  ******************************************************************* *)
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program TrackColor;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Math,
  uLibName in '..\..\..\include\uLibName.pas',
  highgui_c in '..\..\..\include\highgui\highgui_c.pas',
  core_c in '..\..\..\include\core\core_c.pas',
  Core.types_c in '..\..\..\include\core\Core.types_c.pas',
  imgproc.types_c in '..\..\..\include\imgproc\imgproc.types_c.pas',
  imgproc_c in '..\..\..\include\imgproc\imgproc_c.pas',
  Core in '..\..\..\include\core\core.pas',
  cvUtils in '..\..\..\include\cvUtils.pas',
  Mat in '..\..\..\include\core\Mat.pas';

function GetThresholdedImage(img: pIplImage): pIplImage;
Var
  imgHSV: pIplImage;
  imgThreshed: pIplImage;
begin
  // Convert the image into an HSV image
  imgHSV := cvCreateImage(cvGetSize(img), 8, 3);
  cvCvtColor(img, imgHSV, CV_BGR2HSV);
  imgThreshed := cvCreateImage(cvGetSize(img), 8, 1);
  // Values 20,100,100 to 30,255,255 working perfect for yellow at around 6pm
  cvInRangeS(imgHSV, cvScalar(112, 100, 100), cvScalar(124, 255, 255), imgThreshed);
  cvReleaseImage(imgHSV);
  Result := imgThreshed;
end;

Var
  capture: pCvCapture = nil;
  imgScribble: pIplImage;
  frame: pIplImage;
  imgYellowThresh: pIplImage;
  moments: pCvMoments;
  moment10: double;
  moment01: double;
  area: double;

  posX: Integer = 0;
  posY: Integer = 0;

  lastX: Integer;
  lastY: Integer;
  c: Integer;

begin
  try
    // Initialize capturing live feed from the camera
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    if not Boolean(capture) then
    begin
      Writeln('Could not initialize capturing...');
      Halt(1);
    end;

    // The two windows we'll be using
    cvNamedWindow('svideo');
    cvNamedWindow('video');
    cvNamedWindow('thresh');

    // This image holds the 'scribble' data...
    // the tracked positions of the ball
    imgScribble := Nil;

    // An infinite loop
    while (true) do
    begin
      // Will hold a frame captured from the camera
      frame := cvQueryFrame(capture);

      // If we couldn't grab a frame... quit
      if not Boolean(frame) then
        break;

      cvShowImage('svideo', frame);

      // If this is the first frame, we need to initialize it
      if (imgScribble = NiL) then
        imgScribble := cvCreateImage(cvGetSize(frame), 8, 3);

      // Holds the yellow thresholded image (yellow := white, rest := black)
      imgYellowThresh := GetThresholdedImage(frame);

      // Calculate the moments to estimate the position of the ball
      moments := pCvMoments(AllocMem(sizeof(TCvMoments)));
      cvMoments(imgYellowThresh, moments, 1);

      // The actual moment values
      moment10 := cvGetSpatialMoment(moments, 1, 0);
      moment01 := cvGetSpatialMoment(moments, 0, 1);
      area := cvGetCentralMoment(moments, 0, 0);

      // Holding the last and current ball positions
      lastX := posX;
      lastY := posY;

      posX := Trunc(moment10 / area);
      posY := Trunc(moment01 / area);

      // Print it out for debugging purposes
      Writeln(Format('position (%d,%d)', [posX, posY]));

      // We want to draw a line only if its a valid position
      if (lastX > 0) and (lastY > 0) and (posX > 0) and (posY > 0) then
      begin
        // Draw a yellow line from the previous point to the current point
        cvLine(imgScribble, cvPoint(posX, posY), cvPoint(lastX, lastY), cvScalar(0, 255, 255), 5);
      end;

      // Add the scribbling image and the frame... and we get a combination of the two
      cvAdd(frame, imgScribble, frame);
      cvShowImage('thresh', imgYellowThresh);
      cvShowImage('video', frame);
      // Release the thresholded image... we need no memory leaks.. please
      cvReleaseImage(imgYellowThresh);
      FreeMem(moments);
      // Wait for a keypress
      c := cvWaitKey(10);
      if (c = 27) then
      begin
        // If pressed, break out of the loop
        break;
      end;
    end;
    cvDestroyAllWindows;
    // We're done using the camera. Other applications can now use it
    cvReleaseCapture(capture);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
