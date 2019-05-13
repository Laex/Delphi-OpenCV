(* *************************************************************************************************
  Project Delphi-OpenCV
  **************************************************************************************************
  Contributor:
  Laentir Valetov (laex@bk.ru)
  Mikhail Grigorev (sleuthhound@gmail.com)
  **************************************************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  **************************************************************************************************
  License:
  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied. See the License for the specific language governing rights
  and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  **************************************************************************************************
  Warning: Using Delphi XE3 syntax!
  **************************************************************************************************
  The Initial Developer of the Original Code:
  OpenCV: open source computer vision library
  Homepage:    http://ocv.org
  Online docs: http://docs.ocv.org
  Q&A forum:   http://answers.ocv.org
  Dev zone:    http://code.ocv.org
  ************************************************************************************************** *)

unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  ocv.highgui_c, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TMainForm = class(TForm)
    img1: TImage;
    rb1: TRadioButton;
    pb1: TPaintBox;
    rb2: TRadioButton;
    chk1: TCheckBox;
    rg1: TRadioGroup;
    se1: TSpinEdit;
    tmr1: TTimer;
    Label1: TLabel;
    RTSPCapture: TEdit;
    ButtonStartCapture: TButton;
    lbl1: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure rg1Click(Sender: TObject);
    procedure se1Change(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure ButtonStartCaptureClick(Sender: TObject);
  private
    capture: pCvCapture;
    framebitmap: TBitmap;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure UpdateGetImage;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses ocv.core.types_c, ocv.utils;

procedure TMainForm.ButtonStartCaptureClick(Sender: TObject);
var
  rtsp: AnsiString;
begin
  rtsp := AnsiString(RTSPCapture.Text);
  capture := cvCreateFileCapture(pAnsiChar(rtsp));
  if Assigned(capture) then
  begin
    framebitmap := TBitmap.Create;
    framebitmap.PixelFormat := pf24bit;
    se1.Value := 100;
    UpdateGetImage;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(capture) then
    cvReleaseCapture(capture);
  if Assigned(framebitmap) then
    framebitmap.Free;
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);
Var
  frame: pIplImage;
begin
  frame := cvQueryFrame(capture);
  if Assigned(frame) then
  begin
    IplImage2Bitmap(frame, framebitmap);
    if rb1.Checked then
    begin
      if chk1.Checked then
        framebitmap.Canvas.TextOut(2, 2, 'Text demo');
      img1.Picture.Assign(framebitmap);
    end
    else if rb2.Checked then
    begin
      pb1.Canvas.StretchDraw(pb1.ClientRect, framebitmap);
      if chk1.Checked then
        pb1.Canvas.TextOut(2, 2, 'Text demo');
    end;
    Done := False;
  end
  else
    Application.OnIdle := nil;
end;

procedure TMainForm.rg1Click(Sender: TObject);
begin
  UpdateGetImage;
  se1.Enabled := rg1.ItemIndex = 1;
  lbl1.Enabled := rg1.ItemIndex = 1;
end;

procedure TMainForm.se1Change(Sender: TObject);
begin
  tmr1.Interval := se1.Value;
end;

procedure TMainForm.tmr1Timer(Sender: TObject);
var
  Done: Boolean;
begin
  OnIdle(nil, Done);
end;

procedure TMainForm.UpdateGetImage;
begin
  if Assigned(capture) then
    case rg1.ItemIndex of
      0:
        begin
          tmr1.Enabled := False;
          Application.OnIdle := OnIdle;
        end;
      1:
        begin
          Application.OnIdle := Nil;
          tmr1.Enabled := True;
        end;
    end;
end;

end.
