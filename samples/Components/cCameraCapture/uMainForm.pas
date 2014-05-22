// *****************************************************************
// Delphi-OpenCV Demo
// Copyright (C) 2013 Project Delphi-OpenCV
// ****************************************************************
// Contributor:
// Laentir Valetov
// email:laex@bk.ru
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

unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uOCVTypes, uOCVImageOperation,
  opencv.core.types_c, uOCVSource, uOCVView, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    lbl1: TLabel;
    cbb1: TComboBox;
    chk1: TCheckBox;
    ocvw1: TocvView;
    ocvmgprtn1: TocvImageOperation;
    ocvw2: TocvView;
    ocvflsrc1: TocvFileSource;
    ocvw3: TocvView;
    ocvcmrsrc1: TocvCameraSource;
    ocvw4: TocvView;
    ocvpcmsrc1: TocvIPCamSource;
    chk2: TCheckBox;
    chk3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure chk1Click(Sender: TObject);
    procedure chk2Click(Sender: TObject);
    procedure chk3Click(Sender: TObject);
    procedure ocvmgprtn1AfterEachOperation(PrevOperation, Operation, NextOperation: TObject; const IplImage: IocvImage;
      var ContinueTransform: Boolean);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.cbb1Change(Sender: TObject);
begin
  ocvmgprtn1.OperationClass := TocvImageOperationClass(cbb1.Items.Objects[cbb1.ItemIndex]);
end;

procedure TMainForm.chk1Click(Sender: TObject);
begin
  ocvcmrsrc1.Enabled := chk1.Checked;
end;

procedure TMainForm.chk2Click(Sender: TObject);
begin
  ocvpcmsrc1.Enabled := chk2.Checked;
end;

procedure TMainForm.chk3Click(Sender: TObject);
begin
  ocvflsrc1.Enabled := chk3.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cbb1.Items.Assign(GetRegisteredImageOperations);
  cbb1.ItemIndex := cbb1.Items.IndexOf(GetRegisteredImageOperations.GetNameByClass(ocvmgprtn1.OperationClass));
  chk3.Checked := ocvflsrc1.Enabled;
  chk2.Checked := ocvpcmsrc1.Enabled;
  chk1.Checked := ocvcmrsrc1.Enabled;
end;

procedure TMainForm.ocvmgprtn1AfterEachOperation(PrevOperation, Operation, NextOperation: TObject; const IplImage: IocvImage;
  var ContinueTransform: Boolean);
Var
  H: TocvRects;
begin
  if (Operation is TocvHaarCascade) and (NextOperation is TovcCropOperation) then
  begin
    H := (Operation as TocvHaarCascade).HaarRects;
    if Length(H) > 0 then
      (NextOperation as TovcCropOperation).CropRect.ocvRect := H[0];
  end;
end;

end.
