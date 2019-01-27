unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    mmo1: TMemo;
    btn1: TButton;
    pb1: TProgressBar;
    chk1: TCheckBox;
    procedure btn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure Verifying_OpenCV_Dependencies;
    function CheckLoadDLL(const DLLFileName: String; Var ErrorCode: Cardinal; var ErrorString: string): Boolean;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}
{$I libversion.inc}

const

  MSDLL_Core: array of string = ['concrt140', 'msvcp140', 'ucrtbase', 'vcruntime140'];

  CVDLL_Core: array of string = ['opencv_calib3d2413', 'opencv_contrib2413', 'opencv_core2413', 'opencv_features2d2413', 'opencv_flann2413', 'opencv_gpu2413', 'opencv_highgui2413',
    'opencv_imgproc2413', 'opencv_legacy2413', 'opencv_ml2413', 'opencv_nonfree2413', 'opencv_objdetect2413', 'opencv_ocl2413', 'opencv_photo2413', 'opencv_stitching2413',
    'opencv_superres2413', 'opencv_video2413', 'opencv_videostab2413', 'opencv_ffmpeg2413'];

  CVDLL_Classes: array of string = ['opencv_classes2413'];

  FFMPEGDLL: array of string = [ //
    AVCODEC_LIBNAME, AVDEVICE_LIBNAME, AVFILTER_LIBNAME, AVFORMAT_LIBNAME, AVUTIL_LIBNAME, SWRESAMPLE_LIBNAME, SWSCALE_LIBNAME];

  SDLDLL: array of string = ['SDL', 'SDL2'];

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Verifying_OpenCV_Dependencies;
end;

procedure TMainForm.btn1Click(Sender: TObject);
begin
  Verifying_OpenCV_Dependencies;
end;

function TMainForm.CheckLoadDLL(const DLLFileName: String; Var ErrorCode: Cardinal; var ErrorString: string): Boolean;
Var
  DLL: HMODULE;
begin
  DLL := LoadLibraryEx(PChar(DLLFileName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if DLL = 0 then
  begin
    ErrorCode := GetLastError;
    ErrorString := SysErrorMessage(ErrorCode);
    Result := False;
  end
  else
  begin
    FreeLibrary(DLL);
    Result := True;
  end;
end;

procedure TMainForm.Verifying_OpenCV_Dependencies;
Var
  i: Integer;
  ErrorCode: Cardinal;
  ErrorString: string;
  R: Boolean;
begin
  mmo1.Lines.Clear;
  pb1.Position := 0;
  btn1.Enabled := False;
  pb1.Max := Length(MSDLL_Core) + Length(CVDLL_Core) + Length(CVDLL_Classes) + Length(FFMPEGDLL) + Length(SDLDLL);
  Application.ProcessMessages;
  try
    mmo1.Lines.Add('------- Verifying Microsoft DLL -------');
    R := True;
    for i := 0 to High(MSDLL_Core) do
    begin
      if not CheckLoadDLL(MSDLL_Core[i] + '.dll', ErrorCode, ErrorString) then
      begin
        mmo1.Lines.Add('Verifying ' + MSDLL_Core[i] + '.dll');
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(MSDLL_Core[i] + '.dll - ok');

      if not CheckLoadDLL(MSDLL_Core[i] + 'd.dll', ErrorCode, ErrorString) then
      begin
        mmo1.Lines.Add('Verifying ' + MSDLL_Core[i] + 'd.dll');
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(MSDLL_Core[i] + 'd.dll - ok');

      pb1.Position := pb1.Position + 1;
      Application.ProcessMessages;
    end;
    if R then
      mmo1.Lines.Add('OK');

    mmo1.Lines.Add('------- OpenCV DLL -------');
    R := True;
    for i := 0 to High(CVDLL_Core) do
    begin
      if not CheckLoadDLL(CVDLL_Core[i] + '.dll', ErrorCode, ErrorString) then
      begin
        mmo1.Lines.Add('Verifying ' + CVDLL_Core[i] + '.dll');
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(CVDLL_Core[i] + '.dll - ok');
      if (i < High(CVDLL_Core)) and (not CheckLoadDLL(CVDLL_Core[i] + 'd.dll', ErrorCode, ErrorString)) then
      begin
        mmo1.Lines.Add('Verifying ' + CVDLL_Core[i] + 'd.dll');
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(CVDLL_Core[i] + 'd.dll - ok');
      pb1.Position := pb1.Position + 1;
      Application.ProcessMessages;
    end;
    if R then
      mmo1.Lines.Add('OK');

    mmo1.Lines.Add('------- Delphi-OpenCV classes DLL -------');
    R := True;
    for i := 0 to High(CVDLL_Classes) do
    begin
      if not CheckLoadDLL(CVDLL_Classes[i] + '.dll', ErrorCode, ErrorString) then
      begin
        mmo1.Lines.Add('Verifying ' + CVDLL_Classes[i] + '.dll');
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(CVDLL_Classes[i] + '.dll - ok');
      if not CheckLoadDLL(CVDLL_Classes[i] + 'd.dll', ErrorCode, ErrorString) then
      begin
        mmo1.Lines.Add('Verifying ' + CVDLL_Classes[i] + 'd.dll');
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(CVDLL_Classes[i] + 'd.dll - ok');
      pb1.Position := pb1.Position + 1;
      Application.ProcessMessages;
    end;
    if R then
      mmo1.Lines.Add('OK');

    mmo1.Lines.Add('------- FFMPEG DLL -------');
    R := True;
    for i := 0 to High(FFMPEGDLL) do
    begin
      if not CheckLoadDLL(FFMPEGDLL[i], ErrorCode, ErrorString) then
      begin
        mmo1.Lines.Add('Verifying ' + FFMPEGDLL[i]);
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(FFMPEGDLL[i] + '.dll - ok');
      pb1.Position := pb1.Position + 1;
      Application.ProcessMessages;
    end;
    if R then
      mmo1.Lines.Add('OK');

    mmo1.Lines.Add('------- SDL DLL -------');
    R := True;
    for i := 0 to High(SDLDLL) do
    begin
      if not CheckLoadDLL(SDLDLL[i] + '.dll', ErrorCode, ErrorString) then
      begin
        mmo1.Lines.Add('Verifying ' + SDLDLL[i] + '.dll');
        mmo1.Lines.Add('   Error code: ' + ErrorCode.ToString + ' - ' + ErrorString);
        R := False;
      end
      else if chk1.Checked then
        mmo1.Lines.Add(SDLDLL[i] + '.dll - ok');
      pb1.Position := pb1.Position + 1;
      Application.ProcessMessages;
    end;
    if R then
      mmo1.Lines.Add('OK');
  finally
    pb1.Position := 0;
    btn1.Enabled := True;
  end;
end;

end.
