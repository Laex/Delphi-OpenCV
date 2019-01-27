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
******************************************************************
*)

unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ocv.comp.Types,
  ocv.comp.ImageOperation, ocv.comp.Source, ocv.comp.View;

type
  TMainForm = class(TForm)
    ocvw1: TocvView;
    ocvw2: TocvView;
    ocvcmrsrc1: TocvCameraSource;
    ocvmgprtn1: TocvImageOperation;
    btn1: TButton;
    btn2: TButton;
    lbl1: TLabel;
    procedure btn1Click(Sender: TObject);
    procedure ocvw2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ocvw2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ocvw2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btn2Click(Sender: TObject);
    procedure ocvw2AfterPaint(Sender: TObject; const IplImage: IocvImage);
  private
    SnapImage: IocvImage;
    mX, mY: Integer;
    mX1, mY1: Integer;
    mDown: Boolean;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ocv.core_c,
  ocv.core.types_c, ocv.utils;

procedure TMainForm.btn1Click(Sender: TObject);
begin
  SnapImage := ocvcmrsrc1.Image;
  mX := 0;
  mY := 0;
  mX1 := 0;
  mY1 := 0;
  ocvw2.DrawImage(SnapImage);
  lbl1.Visible := True;
end;

procedure TMainForm.btn2Click(Sender: TObject);
begin
  (ocvmgprtn1.Operation as TocvMatchTemplate).IPLTemplate := CropIplImage(SnapImage.IpImage, CvRect(
    {} Trunc(ocvcmrsrc1.ImageWidth * mX / ocvw2.Width),
    {} Trunc(ocvcmrsrc1.ImageHeight * mY / ocvw2.Height),
    {} Trunc(ocvcmrsrc1.ImageWidth * (mX1 - mX) / ocvw2.Width),
    {} Trunc(ocvcmrsrc1.ImageHeight * (mY1 - mY) / ocvw2.Height)));
end;

procedure TMainForm.ocvw2AfterPaint(Sender: TObject; const IplImage: IocvImage);
begin
  // if mDown then
  ocvw2.Canvas.Brush.Style := bsClear;
  ocvw2.Canvas.Pen.Color := clRed;
  ocvw2.Canvas.Pen.Width := 2;
  ocvw2.Canvas.Rectangle(mX, mY, mX1, mY1);
end;

procedure TMainForm.ocvw2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mDown := True;
  mX := X;
  mY := Y;
  mX1 := X;
  mY1 := Y;
  btn2.Enabled := False;
end;

procedure TMainForm.ocvw2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if mDown then
  begin
    mX1 := X;
    mY1 := Y;
    ocvw2.Invalidate;
  end;
end;

procedure TMainForm.ocvw2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  t: Integer;
begin
  if mX1 < mX then
  begin
    t := mX;
    mX := mX1;
    mX1 := t;
  end;
  if mY1 < mY then
  begin
    t := mY;
    mY := mY1;
    mY1 := t;
  end;
  mDown := False;
  ocvw2.Invalidate;
  btn2.Enabled := (mX <> mX1) and (mY <> mY1);
end;

end.
