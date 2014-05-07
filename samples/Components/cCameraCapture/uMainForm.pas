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
  core.types_c, uOCVCamera, uOCVView, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    lbl1: TLabel;
    cbb1: TComboBox;
    chk1: TCheckBox;
    ocvw1: TocvView;
    ocvcmr1: TocvCamera;
    ocvmgprtn1: TocvImageOperation;
    ocvw2: TocvView;
    procedure FormCreate(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure chk1Click(Sender: TObject);
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
  ocvcmr1.Enabled := chk1.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cbb1.Items.Assign(GetRegisteredImageOperations);
//  cbb1.ItemIndex := 0;
//  ocvmgprtn1.OperationClass := TocvImageOperationClass(cbb1.Items.Objects[cbb1.ItemIndex]);
  ocvcmr1.Enabled := True;
end;

end.
