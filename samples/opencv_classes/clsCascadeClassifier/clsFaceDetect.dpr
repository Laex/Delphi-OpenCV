(*
  *****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  *******************************************************************
*)
program clsFaceDetect;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.Character,
  System.SysUtils,

  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.objdetect_c,

  ocv.objdetect,
  ocv.mat,
  ocv.cclasses,

  uResourcePaths;

var
  // Create memory for calculations
  storage: pCvMemStorage = nil;
  // Create a new Haar classifier
  cascade: pCvHaarClassifierCascade = nil;
  // Create a string that contains the cascade name
  cascade_name: String = cResourceFaceDetect + 'haarcascade_frontalface_alt2.xml';
  CC: TccvCascadeClassifier;

  // Function prototype for detecting and drawing an object from an image
procedure detect_and_draw(image: pIplImage);
var
  gray: pIplImage;
  pt1, pt2: TCvPoint;
  i: Integer;
  r: TCRect;
  M: TccvMat;
  objects: TCVectorRect;
begin
  gray := cvCreateImage(cvSize(image^.width, image^.height), IPL_DEPTH_8U, 1);
  cvCvtColor(image, gray, CV_BGR2GRAY);
  M := TccvMat.Create(gray);
  objects := TCVectorRect.Create;
  CC.detectMultiScale(M, objects, 1.1, 3, CV_HAAR_FIND_BIGGEST_OBJECT, cvSize(0, 0), cvSize(0, 0));
  for i := 0 to objects.size - 1 do
  begin
    r := objects.at(i)^;
    pt1.x := r.x;
    pt2.x := r.x + r.width;
    pt1.y := r.y;
    pt2.y := r.y + r.height;
    // Draw the rectangle in the input image
    cvRectangle(image, pt1, pt2, CV_RGB(255, 0, 0), 3, 8, 0);
  end;
  // Show the image in the window named "result"
  cvShowImage('result', image);
  cvReleaseImage(gray);
  M.Free;
  objects.Free;
end;

Var
  capture: pCvCapture = nil;
  frame: pIplImage      = nil;
  frame_copy: pIplImage = nil;
begin
  try
    CC := TccvCascadeClassifier.Create(cascade_name);
    WriteLn('isOldFormatCascade: ', CC.isOldFormatCascade);
    WriteLn('empty             : ', CC.empty);
    capture := cvCreateCameraCapture(CV_CAP_ANY);
    cvNamedWindow('result', 1);
    if Assigned(capture) then
    begin
      // Capture from the camera.
      While true do
      begin
        // Capture the frame and load it in IplImage
        frame := cvQueryFrame(capture);
        if not Assigned(frame) then
          Break;
        // Allocate framecopy as the same size of the frame
        if not Assigned(frame_copy) then
          frame_copy := cvCreateImage(cvSize(frame^.width, frame^.height), IPL_DEPTH_8U, frame^.nChannels);
        // Check the origin of image. If top left, copy the image frame to frame_copy.
        if (frame^.origin = IPL_ORIGIN_TL) then
          cvCopyImage(frame, frame_copy, nil)
          // Else flip and copy the image
        else
          cvFlip(frame, frame_copy, 0);

        // Call the function to detect and draw the face
        detect_and_draw(frame_copy);
        // Wait for a while before proceeding to the next frame
        if (cvWaitKey(1) >= 0) then
          Break;

      end;
      // Release the images, and capture memory
      cvReleaseImage(frame_copy);
      cvReleaseCapture(capture);
      CC.Free;
    end;
    // Destroy the window previously created with filename: "result"
    cvDestroyWindow('result');
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
