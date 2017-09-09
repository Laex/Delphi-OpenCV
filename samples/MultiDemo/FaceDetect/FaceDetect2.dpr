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
program FaceDetect;

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
  uResourcePaths;

var
  // Create memory for calculations
  storage: pCvMemStorage = nil;
  // Create a new Haar classifier
  cascade: pCvHaarClassifierCascade = nil;
  // Create a string that contains the cascade name
  cascade_name: AnsiString = cResourceFaceDetect + 'haarcascade_eye.xml';
  // Input file name for avi or image file.
  input_name: AnsiString = '0';

  // Function prototype for detecting and drawing an object from an image
procedure detect_and_draw(image: pIplImage);
var
  scale: Integer;
  temp: pIplImage;
  // two points to represent the face locations
  pt1, pt2: TCvPoint;
  i: Integer;
  faces: pCvSeq;
  r: pCvRect;
begin
  scale := 1;
  // Create a new image based on the input image
  temp := cvCreateImage(cvSize(image^.width div scale, image^.height div scale), 8, 3);
  // Clear the memory storage which was used before
  cvClearMemStorage(storage);

  // Find whether the cascade is loaded, to find the faces. If yes, then:
  if Assigned(cascade) then
  begin
    // There can be more than one face in an image. So create a growable sequence of faces.
    // Detect the objects and store them in the sequence
    faces := cvHaarDetectObjects(image, cascade, storage, 1.1, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(40, 40), cvSize(0, 0));

    // Loop the number of faces found.
    for i := 1 to faces^.total do
    begin
      // Create a new rectangle for drawing the face
      r := pCvRect(cvGetSeqElem(faces, i));
      // Find the dimensions of the face,and scale it if necessary
      pt1.x := r^.x * scale;
      pt2.x := (r^.x + r^.width) * scale;
      pt1.y := r^.y * scale;
      pt2.y := (r^.y + r^.height) * scale;

      // Draw the rectangle in the input image
      cvRectangle(image, pt1, pt2, CV_RGB(255, 0, 0), 3, 8, 0);
    end;
  end;

  // Show the image in the window named "result"
  cvShowImage('result', image);

  // Release the temp image created.
  cvReleaseImage(temp);
end;

Var
  // Structure for getting video from camera or avi
  capture: pCvCapture = nil;
  // Images to capture the frame from video or camera or from file
  frame: pIplImage = nil;
  frame_copy: pIplImage = nil;

const
  opt = '--cascade=';

begin
  try
    // Check for the correct usage of the command line
    if (ParamCount > 1) and (Pos(opt, ParamStr(1)) <> 0) then
    begin
      cascade_name := Copy(ParamStr(1), Length(opt) + 1, Length(ParamStr(1)));
      input_name := ParamStr(2);
    end
    else
    if not FileExists(cascade_name) then
    begin
      Writeln('Usage: facedetect --cascade=<cascade_path> [filename|camera_index]');
      Halt(1);
    end;

    // Load the HaarClassifierCascade
    cascade := cvLoad(pCVChar(@cascade_name[1]), 0, 0, 0);

    // Check whether the cascade has loaded successfully. Else report and error and quit
    if not Assigned(cascade) then
    begin
      Writeln('ERROR: Could not load classifier cascade');
      Halt(1);
    end;

    // Allocate the memory storage
    storage := cvCreateMemStorage(0);

    // Find whether to detect the object from file or from camera.
    if isDigit(input_name, 1) and (Length(input_name) = 1) then
      capture := cvCreateCameraCapture(StrToInt(input_name))
    else
      capture := cvCreateFileCapture(pCVChar(@input_name[1]));
    // Create a new named window with title: result
    cvNamedWindow('result', 1);

    // Find if the capture is loaded successfully or not.

    // If loaded succesfully, then:
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
          cvCopy(frame, frame_copy)
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
    end
    else
    // If the capture is not loaded succesfully, then:
    begin // Assume the image to be lena.jpg, or the input_name specified
      input_name := cResourceMedia + 'lena.jpg';

      // Load the image from that filename
      frame := cvLoadImage(pCVChar(@input_name[1]), 1);

      // If Image is loaded succesfully, then:
      if Assigned(frame) then
      begin
        // Detect and draw the face
        detect_and_draw(frame);
        //
        // Wait for user input
        cvWaitKey(0);

        // Release the image memory
        cvReleaseImage(frame);
      end;
    end;
    // Destroy the window previously created with filename: "result"
    cvDestroyWindow('result');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
