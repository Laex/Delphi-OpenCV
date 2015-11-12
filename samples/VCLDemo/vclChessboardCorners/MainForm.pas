// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Samuele Trentin
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

unit MainForm;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    ImageOut: TImage;
    Panel1: TPanel;
    ButtonClose: TButton;
    ButtonAR: TButton;
    ImageCaptured: TImage;
    ButtonShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonARClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
  private
    {Private declarations}
    FCamCapture: pCvCapture;
    FFrameBitmap: TBitmap;
    FOverlaySize: TCvSize;
    FbAR2D: boolean;
    FCorner: array [0 .. 100] of TCvPoint2D32f;
    FOverlayImage: pIplImage;
    procedure OnIdle(Sender: TObject; var Done: boolean);
  public
    {Public declarations}
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  ocv.core_c,
  ocv.utils,
  ocv.calib3d_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

const
  nWidthGrid = 9;
  nHeightGrid = 6;

procedure TFormMain.ButtonCloseClick(Sender: TObject);
begin
  Application.OnIdle := nil;
  FbAR2D := False;
  Close;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ButtonShowClick(Self);

  // Initialization
  FbAR2D := False;
  FOverlaySize.height := nHeightGrid - 1;
  FOverlaySize.width := nWidthGrid - 1;
  FOverlayImage := cvLoadImage('Resource\baboon.jpg');

  // Link to the first camera available
  FCamCapture := cvCreateCameraCapture(CV_CAP_ANY);
  if Assigned(FCamCapture) then
  begin
    // Structure for treating the images captured by camera
    FFrameBitmap := TBitmap.Create;
    FFrameBitmap.PixelFormat := pf24bit;

    // Show frame captured..
    Application.OnIdle := OnIdle;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Free camera and structure..
  if Assigned(FCamCapture) then
    cvReleaseCapture(FCamCapture);
  if Assigned(FFrameBitmap) then
    FFrameBitmap.Free;
  cvReleaseImage(FOverlayImage);
end;

procedure TFormMain.OnIdle(Sender: TObject; var Done: boolean);
Var
  frame: pIplImage;
  nResult: Integer;
  resizeImg, blankImg, negImg, copyImg, greyImg: pIplImage;
  p, q: pCvPoint2D32f;
  warp_matrix: pCvMat;
begin
  // Retrieve frame
  frame := cvQueryFrame(FCamCapture);
  if Assigned(frame) then
  begin
    // Show input
    if ImageCaptured.Visible then
    begin
      IplImage2Bitmap(frame, FFrameBitmap);
      ImageCaptured.Picture.Bitmap.Assign(FFrameBitmap);
    end;

    // Overlay image..
    if FbAR2D then
    begin
      try
        // Create gray image from source
        greyImg := cvCreateImage(cvGetSize(frame), IPL_DEPTH_8U, 1);
        cvCvtColor(frame, greyImg, CV_BGR2GRAY);
        nResult := cvCheckChessboard(greyImg, CvSize(nWidthGrid, nHeightGrid));
        if nResult > 0 then
        begin
          nResult := cvFindChessboardCorners(greyImg, FOverlaySize, @FCorner);
          if nResult > 0 then
          begin
            try
              // Identifies the pattern from the gray image and saves the valid group of corners
              cvFindCornerSubPix(greyImg, @FCorner, 40, CvSize(11, 11), CvSize(-1, -1),
                CvTermCriteria(CV_TERMCRIT_EPS + CV_TERMCRIT_ITER, 40, 0.1));

              // Set the source points
              q := AllocMem(SizeOf(TCvPoint2D32f) * 4);
              // Src Top left
              q[0].x := 0;
              q[0].y := 0;
              // Src Top right
              q[1].x := FOverlayImage.width - 1;
              q[1].y := 0;
              // Src Bottom left
              q[2].x := 0;
              q[2].y := FOverlayImage.height - 1;
              // Src Bot right
              q[3].x := FOverlayImage.width - 1;
              q[3].y := FOverlayImage.height - 1;

              // Set the destination points
              p := AllocMem(SizeOf(TCvPoint2D32f) * 4);
              // Src Top right
              p[0].x := FCorner[0].x;
              p[0].y := FCorner[0].y;
              // Src Top left
              p[1].x := FCorner[nWidthGrid - 2].x;
              p[1].y := FCorner[nWidthGrid - 2].y;
              // Src Bottom left
              p[2].x := FCorner[(nWidthGrid - 1) * (nHeightGrid - 2)].x;
              p[2].y := FCorner[(nWidthGrid - 1) * (nHeightGrid - 2)].y;
              // Src Bot right
              p[3].x := FCorner[(nWidthGrid - 1) * (nHeightGrid - 1) - 1].x;
              p[3].y := FCorner[(nWidthGrid - 1) * (nHeightGrid - 1) - 1].y;

              // Create the transformation matrix
              warp_matrix := cvCreateMat(3, 3, CV_32FC1);
              cvGetPerspectiveTransform(q, p, warp_matrix);

              // Support structures
              blankImg := cvCreateImage(cvGetSize(FOverlayImage), FOverlayImage.depth, FOverlayImage.nChannels);
              negImg := cvCreateImage(cvGetSize(frame), frame.depth, frame.nChannels);
              copyImg := cvCreateImage(cvGetSize(frame), frame.depth, frame.nChannels);

              // Transform overlay image
              cvWarpPerspective(FOverlayImage, negImg, warp_matrix, CV_INTER_LINEAR or CV_WARP_FILL_OUTLIERS, cvScalarAll(0));

              // Set to white
              cvSet(blankImg, cvScalarAll(255));
              // Transform blank image
              cvWarpPerspective(blankImg, copyImg, warp_matrix, CV_INTER_LINEAR or CV_WARP_FILL_OUTLIERS, cvScalarAll(0));

              // Invert image
              cvNot(copyImg, copyImg);

              // Join Frame and overlay image
              cvAnd(copyImg, frame, copyImg);
              cvOr(copyImg, negImg, frame);

              // Show output
              IplImage2Bitmap(frame, FFrameBitmap);
              ImageOut.Picture.Bitmap.Assign(FFrameBitmap);
            finally
              FreeMem(q);
              FreeMem(p);
              cvReleaseImage(blankImg);
              cvReleaseImage(negImg);
              cvReleaseImage(copyImg);
            end;
          end;
        end;
      except
        on E: Exception do
      end;
    end;
    Done := False;
  end
  else
    Application.OnIdle := nil;
end;

procedure TFormMain.ButtonARClick(Sender: TObject);
begin
  // Start / Stop
  if not FbAR2D then
    ButtonAR.Caption := 'Disable'
  else
    ButtonAR.Caption := 'Active';
  FbAR2D := not FbAR2D;
end;

procedure TFormMain.ButtonShowClick(Sender: TObject);
begin
  ImageCaptured.Visible := not ImageCaptured.Visible;
  // Show / hide captured frame..
  if ImageCaptured.Visible then
  begin
    ButtonShow.Caption := '<< Hide cap.';
    Self.width := 1384;
  end
  else
  begin
    ButtonShow.Caption := 'Show cap. >>';
    Self.width := 746;
  end;
end;

end.
