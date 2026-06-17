unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  ocv.core.types_c, ocv.core_c, ocv.highgui_c, ocv.imgproc.types_c, ocv.imgproc_c, ocv.utils;

const
  cResourceMedia = '..\..\resource\media\';

type
  pTDetectCtx = ^TDetectCtx;
  TDetectCtx = record
    Capture: pCvCapture;
    Frame: pIplImage;
    Template: pIplImage;
    MatchMap: pIplImage;
    MatchScore: Double;
    ObjectDetected: Boolean;
    StableDetected: Boolean;
    DetectStreak: Integer;
  end;

  TMainForm = class(TForm)
    pbFrame: TPaintBox;
    gbSettings: TGroupBox;
    lblFlagCaption: TLabel;
    lblFlag: TLabel;
    lblScoreCaption: TLabel;
    lblScore: TLabel;
    lblThresholdCaption: TLabel;
    seThreshold: TSpinEdit;
    lblStreakCaption: TLabel;
    seStreak: TSpinEdit;
    rgTimerMode: TRadioGroup;
    seInterval: TSpinEdit;
    lblMs: TLabel;
    btnStartStop: TButton;
    btnLoadTemplate: TButton;
    btnCaptureTemplate: TButton;
    tmrFrame: TTimer;
    dlgOpenTemplate: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
    procedure btnLoadTemplateClick(Sender: TObject);
    procedure btnCaptureTemplateClick(Sender: TObject);
    procedure rgTimerModeClick(Sender: TObject);
    procedure seIntervalChange(Sender: TObject);
    procedure tmrFrameTimer(Sender: TObject);
    procedure pbFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbFramePaint(Sender: TObject);
  private
    FCtx: pTDetectCtx;
    FFrameBitmap: TBitmap;
    FSnapFrame: pIplImage;
    FSelectingTemplate: Boolean;
    FMouseDown: Boolean;
    FSelX, FSelY, FSelX1, FSelY1: Integer;
    FImageWidth, FImageHeight: Integer;
    procedure StartCapture;
    procedure StopCapture;
    procedure UpdateTimerMode;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure ProcessFrame;
    procedure DetectTemplate;
    procedure ReleaseMatchMap;
    procedure EnsureMatchMap;
    procedure SetTemplateImage(const AImage: pIplImage);
    procedure UpdateFlagLabels;
    function PaintToImageX(const X: Integer): Integer;
    function PaintToImageY(const Y: Integer): Integer;
    function ThresholdValue: Double;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'Template detect (camera)';
  dlgOpenTemplate.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + cResourceMedia;
  dlgOpenTemplate.Filter := 'Images|*.bmp;*.jpg;*.jpeg;*.png|All files|*.*';
  seThreshold.Value := 80;
  seStreak.Value := 3;
  seInterval.Value := 100;
  lblFlag.Caption := 'NO';
  lblScore.Caption := '0.00';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopCapture;
end;

function TMainForm.ThresholdValue: Double;
begin
  Result := seThreshold.Value / 100.0;
end;

function TMainForm.PaintToImageX(const X: Integer): Integer;
begin
  if pbFrame.Width <= 0 then
    Exit(0);
  Result := MulDiv(X, FImageWidth, pbFrame.Width);
end;

function TMainForm.PaintToImageY(const Y: Integer): Integer;
begin
  if pbFrame.Height <= 0 then
    Exit(0);
  Result := MulDiv(Y, FImageHeight, pbFrame.Height);
end;

procedure TMainForm.StartCapture;
begin
  if Assigned(FCtx) then
    Exit;

  FCtx := AllocMem(SizeOf(TDetectCtx));
  FCtx.Capture := cvCreateCameraCapture(CV_CAP_ANY);
  if not Assigned(FCtx.Capture) then
  begin
    ShowMessage('Cannot open camera.');
    FreeMem(FCtx);
    FCtx := nil;
    Exit;
  end;

  FImageWidth := Trunc(cvGetCaptureProperty(FCtx.Capture, CV_CAP_PROP_FRAME_WIDTH));
  FImageHeight := Trunc(cvGetCaptureProperty(FCtx.Capture, CV_CAP_PROP_FRAME_HEIGHT));
  FFrameBitmap := TBitmap.Create;
  FFrameBitmap.PixelFormat := pf24bit;

  btnStartStop.Caption := 'Stop';
  btnCaptureTemplate.Enabled := True;
  UpdateTimerMode;
end;

procedure TMainForm.StopCapture;
begin
  Application.OnIdle := nil;
  tmrFrame.Enabled := False;
  FSelectingTemplate := False;
  FMouseDown := False;

  if Assigned(FSnapFrame) then
  begin
    cvReleaseImage(FSnapFrame);
    FSnapFrame := nil;
  end;

  if Assigned(FCtx) then
  begin
    ReleaseMatchMap;
    if Assigned(FCtx.Template) then
      cvReleaseImage(FCtx.Template);
    if Assigned(FCtx.Capture) then
      cvReleaseCapture(FCtx.Capture);
    FreeMem(FCtx);
    FCtx := nil;
  end;

  if Assigned(FFrameBitmap) then
    FreeAndNil(FFrameBitmap);

  btnStartStop.Caption := 'Start';
  btnCaptureTemplate.Enabled := False;
  lblFlag.Caption := 'NO';
  lblScore.Caption := '0.00';
end;

procedure TMainForm.UpdateTimerMode;
begin
  if not Assigned(FCtx) then
    Exit;
  seInterval.Enabled := rgTimerMode.ItemIndex = 1;
  lblMs.Enabled := rgTimerMode.ItemIndex = 1;
  case rgTimerMode.ItemIndex of
    0:
      begin
        tmrFrame.Enabled := False;
        Application.OnIdle := OnIdle;
      end;
    1:
      begin
        Application.OnIdle := nil;
        tmrFrame.Enabled := True;
      end;
  end;
end;

procedure TMainForm.btnStartStopClick(Sender: TObject);
begin
  if Assigned(FCtx) then
    StopCapture
  else
    StartCapture;
end;

procedure TMainForm.rgTimerModeClick(Sender: TObject);
begin
  UpdateTimerMode;
end;

procedure TMainForm.seIntervalChange(Sender: TObject);
begin
  tmrFrame.Interval := seInterval.Value;
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
  ProcessFrame;
  Done := Assigned(FCtx);
end;

procedure TMainForm.tmrFrameTimer(Sender: TObject);
begin
  ProcessFrame;
end;

procedure TMainForm.ReleaseMatchMap;
begin
  if Assigned(FCtx) and Assigned(FCtx.MatchMap) then
  begin
    cvReleaseImage(FCtx.MatchMap);
    FCtx.MatchMap := nil;
  end;
end;

procedure TMainForm.EnsureMatchMap;
var
  MapW, MapH: Integer;
begin
  if not Assigned(FCtx) or not Assigned(FCtx.Frame) or not Assigned(FCtx.Template) then
    Exit;

  MapW := FCtx.Frame^.width - FCtx.Template^.width + 1;
  MapH := FCtx.Frame^.height - FCtx.Template^.height + 1;
  if (MapW < 1) or (MapH < 1) then
  begin
    ReleaseMatchMap;
    Exit;
  end;

  if not Assigned(FCtx.MatchMap) or
     (FCtx.MatchMap^.width <> MapW) or (FCtx.MatchMap^.height <> MapH) then
  begin
    ReleaseMatchMap;
    FCtx.MatchMap := cvCreateImage(cvSize(MapW, MapH), IPL_DEPTH_32F, 1);
  end;
end;

procedure TMainForm.SetTemplateImage(const AImage: pIplImage);
begin
  if not Assigned(FCtx) then
    Exit;
  if Assigned(FCtx.Template) then
    cvReleaseImage(FCtx.Template);
  FCtx.Template := cvCloneImage(AImage);
  ReleaseMatchMap;
end;

procedure TMainForm.btnLoadTemplateClick(Sender: TObject);
var
  Loaded: pIplImage;
begin
  if not Assigned(FCtx) then
  begin
    ShowMessage('Start camera first.');
    Exit;
  end;
  if not dlgOpenTemplate.Execute then
    Exit;

  Loaded := cvLoadImage(PAnsiChar(AnsiString(dlgOpenTemplate.FileName)));
  if not Assigned(Loaded) then
  begin
    ShowMessage('Cannot load template image.');
    Exit;
  end;

  SetTemplateImage(Loaded);
  cvReleaseImage(Loaded);
end;

procedure TMainForm.btnCaptureTemplateClick(Sender: TObject);
begin
  if not Assigned(FCtx) then
    Exit;
  if not Assigned(FCtx.Frame) then
  begin
    ShowMessage('Wait for the first camera frame.');
    Exit;
  end;

  if Assigned(FSnapFrame) then
    cvReleaseImage(FSnapFrame);
  FSnapFrame := cvCloneImage(FCtx.Frame);
  FSelectingTemplate := True;
  FMouseDown := False;
  FSelX := 0;
  FSelY := 0;
  FSelX1 := 0;
  FSelY1 := 0;
  pbFrame.Invalidate;
end;

procedure TMainForm.pbFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FSelectingTemplate then
    Exit;
  FMouseDown := True;
  FSelX := X;
  FSelY := Y;
  FSelX1 := X;
  FSelY1 := Y;
end;

procedure TMainForm.pbFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FSelectingTemplate and FMouseDown then
  begin
    FSelX1 := X;
    FSelY1 := Y;
    pbFrame.Invalidate;
  end;
end;

procedure TMainForm.pbFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  X1, Y1, X2, Y2, W, H: Integer;
  Roi: TCvRect;
  Cropped: pIplImage;
begin
  if not FSelectingTemplate or not FMouseDown then
    Exit;

  FMouseDown := False;
  FSelX1 := X;
  FSelY1 := Y;

  X1 := Min(FSelX, FSelX1);
  Y1 := Min(FSelY, FSelY1);
  X2 := Max(FSelX, FSelX1);
  Y2 := Max(FSelY, FSelY1);
  if (X2 - X1 < 4) or (Y2 - Y1 < 4) then
    Exit;

  X1 := PaintToImageX(X1);
  Y1 := PaintToImageY(Y1);
  X2 := PaintToImageX(X2);
  Y2 := PaintToImageY(Y2);
  W := X2 - X1;
  H := Y2 - Y1;
  if (W < 4) or (H < 4) then
    Exit;

  Roi := cvRect(X1, Y1, W, H);
  Cropped := CropIplImage(FSnapFrame, Roi);
  if Assigned(Cropped) then
  begin
    SetTemplateImage(Cropped);
    cvReleaseImage(Cropped);
    FSelectingTemplate := False;
    pbFrame.Invalidate;
  end;
end;

procedure TMainForm.pbFramePaint(Sender: TObject);
begin
  if FSelectingTemplate and ((FSelX <> FSelX1) or (FSelY <> FSelY1)) then
  begin
    pbFrame.Canvas.Brush.Style := bsClear;
    pbFrame.Canvas.Pen.Color := clYellow;
    pbFrame.Canvas.Pen.Width := 2;
    pbFrame.Canvas.Rectangle(FSelX, FSelY, FSelX1, FSelY1);
  end;
end;

procedure TMainForm.DetectTemplate;
var
  MinVal, MaxVal: Double;
  BestPt, Pt2: TCvPoint;
  WasDetected: Boolean;
begin
  FCtx.ObjectDetected := False;
  FCtx.MatchScore := 0;
  FCtx.StableDetected := False;

  if not Assigned(FCtx.Template) or not Assigned(FCtx.MatchMap) then
  begin
    FCtx.DetectStreak := 0;
    Exit;
  end;

  cvMatchTemplate(FCtx.Frame, FCtx.Template, FCtx.MatchMap, CV_TM_CCOEFF_NORMED);
  cvMinMaxLoc(FCtx.MatchMap, @MinVal, @MaxVal, nil, @BestPt, nil);

  FCtx.MatchScore := MaxVal;
  WasDetected := MaxVal >= ThresholdValue;
  FCtx.ObjectDetected := WasDetected;

  if WasDetected then
  begin
    Inc(FCtx.DetectStreak);
    Pt2.X := BestPt.X + FCtx.Template^.width - 1;
    Pt2.Y := BestPt.Y + FCtx.Template^.height - 1;
    cvRectangle(FCtx.Frame, BestPt, Pt2, CV_RGB(0, 255, 0), 2, 8, 0);
  end
  else
    FCtx.DetectStreak := 0;

  FCtx.StableDetected := FCtx.DetectStreak >= seStreak.Value;
end;

procedure TMainForm.UpdateFlagLabels;
begin
  if FCtx.ObjectDetected then
    lblFlag.Caption := 'YES'
  else
    lblFlag.Caption := 'NO';

  if FCtx.StableDetected then
    lblFlag.Font.Color := clGreen
  else if FCtx.ObjectDetected then
    lblFlag.Font.Color := clOlive
  else
    lblFlag.Font.Color := clMaroon;

  lblScore.Caption := Format('%.3f', [FCtx.MatchScore]);
end;

procedure TMainForm.ProcessFrame;
begin
  if not Assigned(FCtx) then
    Exit;

  FCtx.Frame := cvQueryFrame(FCtx.Capture);
  if not Assigned(FCtx.Frame) then
  begin
    Application.OnIdle := nil;
    tmrFrame.Enabled := False;
    Exit;
  end;

  EnsureMatchMap;
  DetectTemplate;
  UpdateFlagLabels;

  if Assigned(FFrameBitmap) then
  begin
    IplImage2Bitmap(FCtx.Frame, FFrameBitmap);
    pbFrame.Canvas.StretchDraw(pbFrame.ClientRect, FFrameBitmap);
  end;

  if FSelectingTemplate then
    pbFrame.Invalidate;
end;

end.
