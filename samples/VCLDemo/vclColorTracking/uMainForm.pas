// **************************************************************************************************
// Project Delphi-OpenCV
// **************************************************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
// **************************************************************************************************
// You may retrieve the latest version of this file at the GitHub,
// located at git://github.com/Laex/Delphi-OpenCV.git
// **************************************************************************************************
// License:
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU Lesser General Public License (the  "LGPL License"), in which case the
// provisions of the LGPL License are applicable instead of those above.
// If you wish to allow use of your version of this file only under the terms
// of the LGPL License and not to allow others to use your version of this file
// under the MPL, indicate your decision by deleting  the provisions above and
// replace  them with the notice and other provisions required by the LGPL
// License.  If you do not delete the provisions above, a recipient may use
// your version of this file under either the MPL or the LGPL License.
//
// For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
// **************************************************************************************************
// The Initial Developer of the Original Code:
// OpenCV: open source computer vision library
// Homepage:    http://ocv.org
// Online docs: http://docs.ocv.org
// Q&A forum:   http://answers.ocv.org
// Dev zone:    http://code.ocv.org
// **************************************************************************************************

unit uMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  ocv.highgui_c,
  ocv.core.types_c,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    pb1: TPaintBox;
    pb2: TPaintBox;
    rb1: TRadioButton;
    rb2: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    capture: pCvCapture;
    lastX, lastY: Integer;
    imgTracking: pIplImage;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure DetectingRedObjects(img: pIplImage);
    procedure TrackingRedObjects(img: pIplImage);
    procedure trackObject(imgThresh: pIplImage);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

Uses
  ocv.core_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c,
  ocv.utils;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(capture) then
    cvReleaseCapture(capture);
  if Assigned(imgTracking) then
    cvReleaseImage(imgTracking);
end;

// This function threshold the HSV image and create a binary image
function GetThresholdedImage(imgHSV: pIplImage): pIplImage;
begin
  Result := cvCreateImage(cvGetSize(imgHSV), IPL_DEPTH_8U, 1);

  // Convet a video into a binary image based on the red color.
  // Red color area of the video is assined to '1' and other area is assigned to '0' in the binary image
  cvInRangeS(imgHSV, cvScalar(170, 160, 60), cvScalar(180, 256, 256), Result);
end;

procedure TMainForm.DetectingRedObjects(img: pIplImage);
Var
  imgHSV: pIplImage;
  imgThresh: pIplImage;
  frame: pIplImage;
begin
  frame := cvCloneImage(img);

  // smooth the original image using Gaussian kernel
  cvSmooth(frame, frame, CV_GAUSSIAN, 3, 3);

  imgHSV := cvCreateImage(cvGetSize(frame), IPL_DEPTH_8U, 3);

  // Change the color format from BGR to HSV
  cvCvtColor(frame, imgHSV, CV_BGR2HSV);

  imgThresh := GetThresholdedImage(imgHSV);

  // smooth the binary image using Gaussian kernel
  cvSmooth(imgThresh, imgThresh, CV_GAUSSIAN, 3, 3);

  ipDraw(pb1.Canvas.Handle, frame, pb1.ClientRect);

  ipDraw(pb2.Canvas.Handle, imgThresh, pb1.ClientRect);

  // Clean up used images
  cvReleaseImage(imgHSV);
  cvReleaseImage(imgThresh);
  cvReleaseImage(frame);
end;

procedure TMainForm.trackObject(imgThresh: pIplImage);
Var
  moments: pCvMoments;
  moment10, moment01, area: Double;
  posX, posY: Integer;
begin
  // Calculate the moments of 'imgThresh'
  moments := allocMem(sizeof(TCvMoments));
  cvMoments(imgThresh, moments, 1);
  moment10 := cvGetSpatialMoment(moments, 1, 0);
  moment01 := cvGetSpatialMoment(moments, 0, 1);
  area := cvGetCentralMoment(moments, 0, 0);

  // if the area<1000, I consider that the there are no object in the image and it's because of the noise, the area is not zero
  if (area > 1000) then
  begin
    // calculate the position of the ball
    posX := Trunc(moment10 / area);
    posY := Trunc(moment01 / area);

    if (lastX >= 0) and (lastY >= 0) and (posX >= 0) and (posY >= 0) then
      // Draw a yellow line from the previous point to the current point
      cvLine(imgTracking, cvPoint(posX, posY), cvPoint(lastX, lastY), cvScalar(0, 0, 255), 4);

    lastX := posX;
    lastY := posY;

  end;
  freemem(moments);
end;

procedure TMainForm.TrackingRedObjects(img: pIplImage);
Var
  imgHSV: pIplImage;
  imgThresh: pIplImage;
  frame: pIplImage;
begin
  frame := cvCloneImage(img);

  cvSmooth(frame, frame, CV_GAUSSIAN, 3, 3); // smooth the original image using Gaussian kernel

  imgHSV := cvCreateImage(cvGetSize(frame), IPL_DEPTH_8U, 3);
  cvCvtColor(frame, imgHSV, CV_BGR2HSV); // Change the color format from BGR to HSV
  imgThresh := GetThresholdedImage(imgHSV);

  cvSmooth(imgThresh, imgThresh, CV_GAUSSIAN, 3, 3); // smooth the binary image using Gaussian kernel

  // track the possition of the ball
  trackObject(imgThresh);

  // Add the tracking image and the frame
  cvAdd(frame, imgTracking, frame);

  ipDraw(pb1.Canvas.Handle, frame, pb1.ClientRect);

  ipDraw(pb2.Canvas.Handle, imgThresh, pb1.ClientRect);

  // Clean up used images
  cvReleaseImage(imgHSV);
  cvReleaseImage(imgThresh);
  cvReleaseImage(frame);
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);
Var
  frame: pIplImage;
begin
  if Assigned(capture) then
  begin
    frame := cvQueryFrame(capture);
    if Assigned(frame) then
    begin
      if rb2.Checked then
        DetectingRedObjects(frame)
      else if rb1.Checked then
      begin
        if not Assigned(imgTracking) then
        begin
          // create a blank image and assigned to 'imgTracking' which has the same size of original video
          imgTracking := cvCreateImage(cvGetSize(frame), IPL_DEPTH_8U, 3);
          cvZero(imgTracking); // covert the image, 'imgTracking' to black
        end;
        TrackingRedObjects(frame);
      end;
    end;
  end;
  Done := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  lastX := -1;
  lastY := -1;
  capture := cvCreateCameraCapture(CV_CAP_ANY);
  if Assigned(capture) then
    Application.OnIdle := OnIdle;
end;

end.
