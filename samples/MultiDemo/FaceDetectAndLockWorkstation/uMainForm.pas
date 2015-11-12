unit uMainForm;

{$R images.res}

interface

uses
  ocv.core.types_c, ocv.core_c, ocv.highgui_c, ocv.objdetect_c, ocv.utils,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.Menus, Vcl.ImgList, uResourcePaths;

const
  WM_NOFACE = WM_USER+1;
  WM_NOCAMERA = WM_NOFACE+1;

type
  pCtx = ^TCtx;
  TCtx = record
    MyCapture: pCvCapture;       // Capture handle
    MyInputImage: pIplImage;     // Input image
    MyStorage: pCvMemStorage;    // Memory storage
    TotalFaceDetect: Integer;    // Total face detect
  end;
  TMainForm = class(TForm)
    LWFrameOutput: TPaintBox;
    LWTimer: TTimer;
    LWPopupMenu: TPopupMenu;
    LWShow: TMenuItem;
    LWExit: TMenuItem;
    LWImageList: TImageList;
    LWGBFaceDetectSettings: TGroupBox;
    LTotalFaceDetect: TLabel;
    LTotalFace: TLabel;
    LWTimerRadioGroup: TRadioGroup;
    LWSpinEdit: TSpinEdit;
    LWLTimerMS: TLabel;
    LWLWaitTime: TLabel;
    LWSpinEditWaitTime: TSpinEdit;
    LWLWaitTimeMS: TLabel;
    LWStopLockTimer: TCheckBox;
    LWLTotalPCLock: TLabel;
    LWButtonStartStop: TButton;
    LWButtonAbout: TButton;
    tmrLWThreadTimer: TTimer;
    trycn1: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LWTimerRadioGroupClick(Sender: TObject);
    procedure LWSpinEditChange(Sender: TObject);
    procedure LWTimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LWExitClick(Sender: TObject);
    procedure LWCoolTrayIconDblClick(Sender: TObject);
    procedure LWCoolTrayIconStartup(Sender: TObject; var ShowMainForm: Boolean);
    procedure LWThreadTimerTimer(Sender: TObject);
    procedure LWSpinEditWaitTimeChange(Sender: TObject);
    procedure LWButtonStartStopClick(Sender: TObject);
    procedure LWButtonAboutClick(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
  private
    MyCtx: pCtx;
    FrameBitmap: TBitmap;
    SessionEnding: Boolean;
    LWMainFormHidden: Boolean;
    FintLockedCount: Integer;
    FrameHeight, FrameWidth: Double;
    procedure StartCapture;
    procedure StopCapture;
    procedure UpdateGetImage;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    procedure NoFaceRedraw(var mes : TMessage); message WM_NOFACE;
    procedure NoCameraRedraw(var mes : TMessage); message WM_NOCAMERA;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure DetectAndDraw(const Ctx: pCtx);
    procedure WndProc(var Message: TMessage); override;
    procedure AddImageFromResourceToPaintBox(ResID: Integer);
    function ShowMessageDlgEx(const AText, ACaption: string; const ResID: Integer; Style: Cardinal = MB_OK): Cardinal; overload;
    function ShowMessageDlgEx(const AText, ACaption: string; const ResName: pChar; Style: Cardinal = MB_OK): Cardinal; overload;
  public
  end;

var
  MainForm: TMainForm;
  HaarCascade: pCvHaarClassifierCascade = nil;
  Cascade_Name: AnsiString = cResourceFaceDetect + 'haarcascade_frontalface_alt.xml';

// WTSRegisterSessionNotification
// http://msdn.microsoft.com/en-us/library/aa383828%28VS.85%29.aspx
// http://msdn.microsoft.com/en-us/library/aa383841%28VS.85%29.aspx
function WTSRegisterSessionNotification(hWnd: HWND; dwFlags: DWORD): BOOL; stdcall;
function WTSUnRegisterSessionNotification(hWND: HWND): BOOL; stdcall;

const
  ProgramsName = 'FaceDetect and LockWorkstation';
  ProgramsVer = '1.0';
  // WTSRegisterSessionNotification
  NOTIFY_FOR_ALL_SESSIONS  = 1;
  NOTIFY_FOR_THIS_SESSIONS = 0;
  // Res images type
  NOFACE = 1;
  NOCAMERA = 2;

implementation

procedure BlockInput(ABlockInput: boolean); stdcall; external 'USER32.DLL';
// WTSRegisterSessionNotification
function WTSRegisterSessionNotification; external 'wtsapi32.dll' Name 'WTSRegisterSessionNotification';
function WTSUnRegisterSessionNotification; external 'wtsapi32.dll' Name 'WTSUnRegisterSessionNotification';

{$R *.dfm}

procedure TMainForm.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_WTSSESSION_CHANGE:
      begin
        if Message.wParam = WTS_SESSION_LOCK then
        begin
          Inc(FintLockedCount);
          StopCapture;
          AddImageFromResourceToPaintBox(NOFACE);
        end;
        if Message.wParam = WTS_SESSION_UNLOCK then
        begin
          LWLTotalPCLock.Caption := 'The computer was locked ' + IntToStr(FintLockedCount) + ' times.';
          StartCapture;
        end;
      end;
  end;
  inherited;
end;

procedure TMainForm.WMQueryEndSession(var Message: TMessage);
begin
  SessionEnding := True;
  Message.Result := 1;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ((LWMainFormHidden) or SessionEnding);
  if not CanClose then
  begin
    Application.Minimize;
    Application.MainFormOnTaskBar:=False;
    LWMainFormHidden := True;
    LWPopupMenu.Items[0].Caption := 'Show';
  end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Application.Minimize;
    Application.MainFormOnTaskBar:=False;
    LWMainFormHidden := True;
    LWPopupMenu.Items[0].Caption := 'Show';
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := ProgramsName;
  // WTSRegisterSessionNotification
  WTSRegisterSessionNotification(Handle, NOTIFY_FOR_ALL_SESSIONS);
  FintLockedCount := 0;
  // End
  trycn1.Hint := ProgramsName;
  trycn1.IconIndex := 1;
  tmrLWThreadTimer.Interval := LWSpinEditWaitTime.Value*1000;
  // Запуск захвата
  StartCapture;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopCapture;
end;

{ Запуск поиска лица }
procedure TMainForm.StartCapture;
begin
  if not Assigned(MyCtx) then
  begin
    MyCtx := nil;
    MyCtx := AllocMem(SizeOf(TCtx));
    try
      MyCtx.MyCapture := cvCreateCameraCapture(CV_CAP_ANY);
    except
      on E: Exception do
      begin
        ShowMessage('Exception in procedure StartCapture.' + #13 + E.ClassName + ': '+ E.Message);
        MyCtx := nil;
        FreeMem(MyCtx, SizeOf(TCtx));
        AddImageFromResourceToPaintBox(NOCAMERA);
        Exit;
      end;
    end;
    // Узнаем ширину и высоту кадра
    FrameWidth := cvGetCaptureProperty(MyCtx.MyCapture, CV_CAP_PROP_FRAME_WIDTH);
    FrameHeight := cvGetCaptureProperty(MyCtx.MyCapture, CV_CAP_PROP_FRAME_HEIGHT);
    //cvSetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_WIDTH, VideoCamWidth);
    //cvSetCaptureProperty(Ctx.MyCapture, CV_CAP_PROP_FRAME_HEIGHT, VideoCamHeight);
    // Load the HaarClassifierCascade
    HaarCascade := cvLoad(pCVChar(@cascade_name[1]), 0, 0, 0);
    // Check whether the cascade has loaded successfully. Else report and error and quit
    if not Assigned(HaarCascade) then
    begin
      ShowMessage('ERROR: Could not load haar classifier cascade.');
      LWExitClick(LWExit);
    end;
    // Allocate the memory storage
    MyCtx.MyStorage := cvCreateMemStorage(0);
    if Assigned(MyCtx.MyCapture) then
    begin
      LWButtonStartStop.Caption := 'Stop face detect';
      LWTimerRadioGroup.Enabled := True;
      LWSpinEditWaitTime.Enabled := True;
      LWStopLockTimer.Enabled := True;
      LWLWaitTime.Enabled := True;
      LWLWaitTimeMS.Enabled := True;
      LWSpinEdit.Enabled := LWTimerRadioGroup.ItemIndex = 1;
      LWLTimerMS.Enabled := LWTimerRadioGroup.ItemIndex = 1;
      FrameBitmap := TBitmap.Create;
      FrameBitmap.PixelFormat := pf24bit;
      UpdateGetImage;
    end
    else
    begin
      MyCtx := nil;
      FreeMem(MyCtx, SizeOf(TCtx));
      AddImageFromResourceToPaintBox(NOCAMERA);
      Exit;
    end;
  end;
end;

{ Остановка поиска лица }
procedure TMainForm.StopCapture;
begin
  try
    if Assigned(MyCtx) then
    begin
      LWTimer.Enabled := False;
      tmrLWThreadTimer.Enabled := False;
      LWTimerRadioGroup.Enabled := False;
      LWSpinEdit.Enabled := False;
      LWLTimerMS.Enabled := False;
      LWSpinEditWaitTime.Enabled := False;
      LWStopLockTimer.Enabled := False;
      LWLWaitTime.Enabled := False;
      LWLWaitTimeMS.Enabled := False;
      Application.OnIdle := nil;
      if Assigned(MyCtx.MyCapture) then
        cvReleaseCapture(MyCtx.MyCapture);
      if Assigned(FrameBitmap) then
        FrameBitmap.Free;
      MyCtx := nil;
      FreeMem(MyCtx, SizeOf(TCtx));
      LWButtonStartStop.Caption := 'Start face detect';
    end;
  except
    on E: Exception do
      ShowMessage('Exception in procedure StopCapture.' + #13 + E.ClassName + ': '+ E.Message);
  end;
end;

procedure TMainForm.tmr1Timer(Sender: TObject);
begin
  if MyCtx.TotalFaceDetect = 0 then
    LockWorkStation();
  {  BlockInput(True)   // Отключить средства ввода (клавиатуру и мышь)
  else
    BlockInput(False); // Включить средства ввода (клавиатуру и мышь)
  }
end;

procedure TMainForm.LWButtonAboutClick(Sender: TObject);
begin
  ShowMessageDlgEx(ProgramsName+#13+'Version: '+ProgramsVer+#13+'Copyright © 2013 by Mikhail Grigorev'+#13+'www.im-history.ru'+#13+'sleuthhound@gmail.com', ProgramsName+' - About', 'ABOUT');
end;

{ Запуск\остановка поиска лица }
procedure TMainForm.LWButtonStartStopClick(Sender: TObject);
begin
  if Assigned(MyCtx) then
  begin
    StopCapture;
    AddImageFromResourceToPaintBox(NOFACE);
  end
  else
    StartCapture;
end;

{ Клик по пункту Скрыть/Показать контекстного меню в трее }
procedure TMainForm.LWCoolTrayIconDblClick(Sender: TObject);
begin
  if LWMainFormHidden then
  begin
    Application.Restore;
    Application.MainFormOnTaskBar:=True;
    LWMainFormHidden := False;
    LWPopupMenu.Items[0].Caption := 'Hide';
  end
  else
  begin
    Application.Minimize;
    Application.MainFormOnTaskBar:=False;
    LWMainFormHidden := True;
    LWPopupMenu.Items[0].Caption := 'Show';
  end;
end;

procedure TMainForm.LWCoolTrayIconStartup(Sender: TObject; var ShowMainForm: Boolean);
begin
  ShowMainForm := False;
  LWMainFormHidden := True;
end;

{ Выход из программы }
procedure TMainForm.LWExitClick(Sender: TObject);
begin
  LWMainFormHidden := True;
  Close;
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
  MyCtx.MyInputImage := cvQueryFrame(MyCtx.MyCapture);
  if Assigned(MyCtx.MyInputImage) then
  begin
    DetectAndDraw(MyCtx);
    if LWStopLockTimer.Checked then
    begin
      if MyCtx.TotalFaceDetect = 0 then
        tmrLWThreadTimer.Enabled := True
      else
        tmrLWThreadTimer.Enabled := False;
    end
    else
      tmrLWThreadTimer.Enabled := True;
    trycn1.Hint := Format('%s (Total face: %s)', [ProgramsName, IntToStr(MyCtx.TotalFaceDetect)]);
    LTotalFace.Caption := IntToStr(MyCtx.TotalFaceDetect);
    if not LWMainFormHidden then
    begin
      IplImage2Bitmap(MyCtx.MyInputImage, FrameBitmap);
      LWFrameOutput.Canvas.StretchDraw(LWFrameOutput.ClientRect, FrameBitmap);
    end;
    Done := False;
  end
  else
  begin
    Application.OnIdle := nil;
    tmrLWThreadTimer.Enabled := False;
  end;
end;

procedure TMainForm.LWThreadTimerTimer(Sender: TObject);
begin
  if MyCtx.TotalFaceDetect = 0 then
    LockWorkStation();
  {  BlockInput(True)   // Отключить средства ввода (клавиатуру и мышь)
  else
    BlockInput(False); // Включить средства ввода (клавиатуру и мышь)
  }
end;

procedure TMainForm.LWTimerRadioGroupClick(Sender: TObject);
begin
  if Assigned(MyCtx) then
  begin
    UpdateGetImage;
    LWSpinEdit.Enabled := LWTimerRadioGroup.ItemIndex = 1;
    LWLTimerMS.Enabled := LWTimerRadioGroup.ItemIndex = 1;
  end;
end;

procedure TMainForm.LWSpinEditChange(Sender: TObject);
begin
  LWTimer.Interval := LWSpinEdit.Value;
end;

procedure TMainForm.LWSpinEditWaitTimeChange(Sender: TObject);
begin
  tmrLWThreadTimer.Enabled := False;
  tmrLWThreadTimer.Interval := LWSpinEditWaitTime.Value*1000;
  tmrLWThreadTimer.Enabled := True;
end;

procedure TMainForm.LWTimerTimer(Sender: TObject);
var
  Done: Boolean;
begin
  OnIdle(nil, Done);
end;

procedure TMainForm.UpdateGetImage;
begin
  if Assigned(MyCtx.MyCapture) then
    case LWTimerRadioGroup.ItemIndex of
      0:
        begin
          LWTimer.Enabled := False;
          Application.OnIdle := OnIdle;
        end;
      1:
        begin
          Application.OnIdle := nil;
          LWTimer.Enabled := True;
        end;
    end;
end;

procedure TMainForm.DetectAndDraw(const Ctx: pCtx);
var
  Scale: Integer;
  Temp: pIplImage;
  Pt1, Pt2: TCvPoint;
  I: Integer;
  Faces: pCvSeq;
  R: pCvRect;
begin
  Scale := 1;
  Temp := cvCreateImage(cvSize(Ctx.MyInputImage^.width div Scale, Ctx.MyInputImage^.height div Scale), 8, 3);
  cvClearMemStorage(Ctx.MyStorage);
  if Assigned(HaarCascade) then
  begin
    Faces := cvHaarDetectObjects(Ctx.MyInputImage, HaarCascade, Ctx.MyStorage, 1.1, 2, CV_HAAR_DO_CANNY_PRUNING, cvSize(40, 40), cvSize(0, 0));
    Ctx.TotalFaceDetect := Faces^.total;
    for I := 1 to Faces^.total do
    begin
      R := pCvRect(cvGetSeqElem(Faces, I));
      Pt1.x := R^.x * Scale;
      Pt2.x := (R^.x + R^.width) * Scale;
      Pt1.y := R^.y * Scale;
      Pt2.y := (R^.y + R^.height) * Scale;
      cvRectangle(Ctx.MyInputImage, Pt1, Pt2, CV_RGB(255, 0, 0), 3, 8, 0);
    end;
  end;
  cvReleaseImage(Temp);
end;

{ Добавление картинки из Resource в PaintBox }
procedure TMainForm.AddImageFromResourceToPaintBox(ResID: Integer);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    if ResID = NOFACE then
      Bitmap.LoadFromResourceName(HInstance, 'NOFACE')
    else if ResID = NOCAMERA then
      Bitmap.LoadFromResourceName(HInstance, 'NOCAMERA');
    LWFrameOutput.Canvas.StretchDraw(LWFrameOutput.ClientRect, Bitmap);
    LWFrameOutput.Tag := ResID;
  finally
    Bitmap.Free;
  end;
end;

procedure TMainForm.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
begin
  BeginPaint(Handle, PS);
  if not LWMainFormHidden then
  begin
    if LWFrameOutput.Tag = NOFACE then
      PostMessage(Handle, WM_NOFACE, 0, 0)
    else if LWFrameOutput.Tag = NOCAMERA then
      PostMessage(Handle, WM_NOCAMERA, 0, 0);
  end;
  EndPaint(Handle, PS);
end;

procedure TMainForm.NoFaceRedraw(var Mes: TMessage);
begin
  AddImageFromResourceToPaintBox(NOFACE);
end;

procedure TMainForm.NoCameraRedraw(var Mes: TMessage);
begin
  AddImageFromResourceToPaintBox(NOCAMERA);
end;

{ Показываем свое диалоговое окно }
function TMainForm.ShowMessageDlgEx(const AText, ACaption: string; const ResID: Integer; Style: Cardinal = MB_OK): Cardinal;
var
   lpMsgBoxParams : MsgBoxParams;
begin
   with lpMsgBoxParams do
   begin
     cbSize             := SizeOf(lpMsgBoxParams);
     hwndOwner          := Application.Handle;
     hInstance          := SysInit.hInstance;
     lpszText           := PChar(AText);
     lpszCaption        := PChar(ACaption);
     dwStyle            := MB_USERICON or MB_TOPMOST or Style;
     lpszIcon           := MAKEINTRESOURCE(ResID);
     dwContextHelpID    := 0;
     lpfnMsgBoxCallback := nil;
     dwLanguageId       := LANG_ENGLISH;
   end;
   Result := Cardinal(MessageBoxIndirect(lpMsgBoxParams));
end;

{ Показываем свое диалоговое окно }
function TMainForm.ShowMessageDlgEx(const AText, ACaption: string; const ResName: pChar; Style: Cardinal = MB_OK): Cardinal;
var
   lpMsgBoxParams : MsgBoxParams;
begin
   with lpMsgBoxParams do
   begin
     cbSize             := SizeOf(lpMsgBoxParams);
     hwndOwner          := Application.Handle;
     hInstance          := SysInit.hInstance;
     lpszText           := PChar(AText);
     lpszCaption        := PChar(ACaption);
     dwStyle            := MB_USERICON or MB_TOPMOST or Style;
     lpszIcon           := ResName;
     dwContextHelpID    := 0;
     lpfnMsgBoxCallback := nil;
     dwLanguageId       := LANG_ENGLISH;
   end;
   Result := Cardinal(MessageBoxIndirect(lpMsgBoxParams));
end;

end.
