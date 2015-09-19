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
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    pb2: TPaintBox;
    chk1: TCheckBox;
    chk2: TCheckBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    trckbr_H_min: TTrackBar;
    trckbr_S_min: TTrackBar;
    trckbr_V_min: TTrackBar;
    trckbr_H_max: TTrackBar;
    trckbr_S_max: TTrackBar;
    trckbr_V_max: TTrackBar;
    edt1: TEdit;
    Edit1: TEdit;
    edt2: TEdit;
    edt3: TEdit;
    edt4: TEdit;
    edt5: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure trckbr_H_minChange(Sender: TObject);
    procedure trckbr_S_minChange(Sender: TObject);
    procedure trckbr_V_minChange(Sender: TObject);
    procedure trckbr_H_maxChange(Sender: TObject);
    procedure trckbr_S_maxChange(Sender: TObject);
    procedure trckbr_V_maxChange(Sender: TObject);
  private
    capture: pCvCapture;
    HSV: pIplImage;
    threshold: pIplImage;
    lastX, lastY: Integer;
    imgTracking: pIplImage;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure morphOps(thresh: pIplImage);
    procedure trackFilteredObject(var x, y: Integer; threshold: pIplImage; var cameraFeed: pIplImage);
    procedure drawObject(x, y: Integer; frame: pIplImage);
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

const
  FRAME_WIDTH = 640;
  FRAME_HEIGHT = 480;
  // max number of objects to be detected in frame
  MAX_NUM_OBJECTS = 50;
  // minimum and maximum object area
  MIN_OBJECT_AREA = 20 * 20;
  MAX_OBJECT_AREA = FRAME_HEIGHT * FRAME_WIDTH / 1.5;

  H_MIN: Integer = 170; // 0;
  H_MAX: Integer = 180;
  CH_MAX: Integer = 256;
  S_MIN: Integer = 160; // 0;
  S_MAX: Integer = 256;
  CS_MAX: Integer = 256;
  V_MIN: Integer = 60; // 0;
  V_MAX: Integer = 256;
  CV_MAX: Integer = 256;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(capture) then
    cvReleaseCapture(capture);
  if Assigned(imgTracking) then
    cvReleaseImage(imgTracking);
  if Assigned(HSV) then
    cvReleaseImage(HSV);
  if Assigned(threshold) then
    cvReleaseImage(threshold);
end;

procedure TMainForm.morphOps(thresh: pIplImage);
Var
  erodeElement, dilateElement: pIplConvKernel;
begin
  // create structuring element that will be used to "dilate" and "erode" image.
  // the element chosen here is a 3px by 3px rectangle
  erodeElement := cvCreateStructuringElementEx(3, 3, 1, 1, CV_SHAPE_RECT);
  // dilate with larger element so make sure object is nicely visible
  dilateElement := cvCreateStructuringElementEx(8, 8, 3, 3, CV_SHAPE_RECT);
  cvErode(thresh, thresh, erodeElement, 2);
  cvDilate(thresh, thresh, dilateElement, 2);
  cvReleaseStructuringElement(erodeElement);
  cvReleaseStructuringElement(dilateElement);
end;

procedure TMainForm.drawObject(x: Integer; y: Integer; frame: pIplImage);
Var
  font: TCvFont;
begin
  // use some of the openCV drawing functions to draw crosshairs
  // on your tracked image!
  // added 'if' and 'else' statements to prevent
  // memory errors from writing off the screen (ie. (-25,-25) is not within the window!)
  cvCircle(frame, cvPoint(x, y), 20, cvScalar(0, 255, 0), 2);
  if (y - 25) > 0 then
    cvLine(frame, cvPoint(x, y), cvPoint(x, y - 25), cvScalar(0, 255, 0), 2)
  else
    cvLine(frame, cvPoint(x, y), cvPoint(x, 0), cvScalar(0, 255, 0), 2);
  if (y + 25) < FRAME_HEIGHT then
    cvLine(frame, cvPoint(x, y), cvPoint(x, y + 25), cvScalar(0, 255, 0), 2)
  else
    cvLine(frame, cvPoint(x, y), cvPoint(x, FRAME_HEIGHT), cvScalar(0, 255, 0), 2);
  if (x - 25) > 0 then
    cvLine(frame, cvPoint(x, y), cvPoint(x - 25, y), cvScalar(0, 255, 0), 2)
  else
    cvLine(frame, cvPoint(x, y), cvPoint(0, y), cvScalar(0, 255, 0), 2);
  if (x + 25) < FRAME_WIDTH then
    cvLine(frame, cvPoint(x, y), cvPoint(x + 25, y), cvScalar(0, 255, 0), 2)
  else
    cvLine(frame, cvPoint(x, y), cvPoint(FRAME_WIDTH, y), cvScalar(0, 255, 0), 2);

  cvInitFont(@font, CV_FONT_HERSHEY_COMPLEX, 1.0, 1.0, 0, 1, CV_AA);

  cvPutText(frame, c_str(intToStr(x) + ',' + intToStr(y)), cvPoint(x, y + 30), @font, cvScalar(0, 255, 0));
end;

procedure TMainForm.trackFilteredObject(Var x, y: Integer; threshold: pIplImage; var cameraFeed: pIplImage);
Var
  refArea: Double;
  objectFound: Boolean;
  area: Double;
  _hierarchy: pCvMemStorage;
  _contours: pCvSeq;
  dst_th: pIplImage;
  contoursCont: Integer;
  moments: TCvMoments;
  moment10: Double;
  moment01: Double;
  font: TCvFont;
begin
  // find contours of filtered image using openCV findContours function
  _hierarchy := cvCreateMemStorage(0);
  dst_th := cvCreateImage(cvGetSize(threshold), IPL_DEPTH_8U, 1);

  cvThreshold(threshold, dst_th, 128, 255, CV_THRESH_BINARY);

  cvSmooth(dst_th, // function input
    dst_th, // function output
    CV_GAUSSIAN, // use Gaussian filter (average nearby pixels, with closest pixels weighted more)
    9, // smoothing filter window width
    9); // smoothing filter window height

  _contours := nil;

  cvClearMemStorage(_hierarchy);
  contoursCont := cvFindContours(dst_th, _hierarchy, @_contours, sizeof(TCvContour), CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE,
    cvPoint(0, 0));

  cvInitFont(@font, CV_FONT_HERSHEY_COMPLEX, 1.0, 1.0, 0, 1, CV_AA);

  if (contoursCont > 0) and (contoursCont < MAX_NUM_OBJECTS) then
  begin
    refArea := 0;
    objectFound := false;
    While Assigned(_contours) do
      if CV_IS_SEQ_CLOSED(_contours) then
      begin
        // cvDrawContours(cameraFeed, _contours, CV_RGB(52, 201, 36), CV_RGB(36, 201, 197), -1, 2, // CV_FILLED,
        // CV_AA, cvPoint(0, 0)); // рисуем контур

        // if number of objects greater than MAX_NUM_OBJECTS we have a noisy filter
        cvZero(dst_th);
        cvDrawContours(dst_th, _contours, CV_RGB(52, 201, 36), CV_RGB(36, 201, 197), -1, 2, // CV_FILLED,
          CV_AA, cvPoint(0, 0)); // рисуем контур
        cvMoments(dst_th, @moments, 1);
        // The actual moment values
        moment10 := cvGetSpatialMoment(@moments, 1, 0);
        moment01 := cvGetSpatialMoment(@moments, 0, 1);
        area := cvGetCentralMoment(@moments, 0, 0);

        if (area > MIN_OBJECT_AREA) and (area < MAX_OBJECT_AREA) and (area > refArea) then
        begin
          x := Trunc(moment10 / area);
          y := Trunc(moment01 / area);
          objectFound := true;
        end
        else
          objectFound := false;

        _contours := _contours^.h_next;
      end;
    // let user know you found an object
    if objectFound then
    begin
      cvPutText(cameraFeed, 'Tracking Object', cvPoint(0, 50), @font, cvScalar(0, 255, 0));
      // draw object location on screen
      drawObject(x, y, cameraFeed);
    end;
  end
  else
    cvPutText(cameraFeed, 'TOO MUCH NOISE! ADJUST FILTER', cvPoint(0, 50), @font, cvScalar(0, 0, 255));
  cvReleaseMemStorage(_hierarchy);
end;

procedure TMainForm.trckbr_H_maxChange(Sender: TObject);
begin
  edt3.Text := intToStr(trckbr_H_max.Position);
end;

procedure TMainForm.trckbr_H_minChange(Sender: TObject);
begin
  edt1.Text := intToStr(trckbr_H_min.Position);
end;

procedure TMainForm.trckbr_S_maxChange(Sender: TObject);
begin
  edt4.Text := intToStr(trckbr_S_max.Position);
end;

procedure TMainForm.trckbr_S_minChange(Sender: TObject);
begin
  Edit1.Text := intToStr(trckbr_S_min.Position);
end;

procedure TMainForm.trckbr_V_maxChange(Sender: TObject);
begin
  edt5.Text := intToStr(trckbr_V_max.Position);
end;

procedure TMainForm.trckbr_V_minChange(Sender: TObject);
begin
  edt2.Text := intToStr(trckbr_V_min.Position);
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);

Const
  PreSize = 100;

Var
  frame: pIplImage;
begin
  if Assigned(capture) then
  begin
    frame := cvQueryFrame(capture);
    if Assigned(frame) then
    begin
      if not Assigned(HSV) then
        HSV := cvCreateImage(cvGetSize(frame), 8, 3);
      cvCvtColor(frame, HSV, CV_BGR2HSV);
      if not Assigned(threshold) then
        threshold := cvCreateImage(cvGetSize(frame), 8, 1);

      cvInRangeS(HSV,
        {} cvScalar(trckbr_H_min.Position, trckbr_S_min.Position, trckbr_V_min.Position),
        {} cvScalar(trckbr_H_max.Position, trckbr_S_max.Position, trckbr_V_max.Position), threshold);
      if chk1.Checked then
        morphOps(threshold);

      if chk2.Checked then
        trackFilteredObject(lastX, lastY, threshold, frame);

      if chk2.Checked then
      begin
        ipDraw(pb2.Canvas.Handle, frame, pb2.ClientRect, true);
        ipDraw(pb2.Canvas.Handle, threshold, Rect(0, pb2.Height - PreSize, PreSize, pb2.Height), true);
      end
      else
      begin
        ipDraw(pb2.Canvas.Handle, threshold, pb2.ClientRect, true);
        ipDraw(pb2.Canvas.Handle, frame, Rect(0, pb2.Height - PreSize, PreSize, pb2.Height), true);
      end;
      ipDraw(pb2.Canvas.Handle, HSV, Rect(pb2.Width - PreSize, pb2.Height - PreSize, pb2.Width, pb2.Height), true);
    end;
  end;
  Done := false;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  lastX := -1;
  lastY := -1;
  trckbr_H_min.Position := H_MIN;
  trckbr_S_min.Position := S_MIN;
  trckbr_V_min.Position := V_MIN;
  trckbr_H_max.Position := H_MAX;
  trckbr_S_max.Position := S_MAX;
  trckbr_V_max.Position := V_MAX;
  capture := cvCreateCameraCapture(CV_CAP_ANY);
  if Assigned(capture) then
  begin
    // set height and width of capture frame
    cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH, FRAME_WIDTH);
    cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT, FRAME_HEIGHT);
    Application.OnIdle := OnIdle;
  end;
end;

end.
