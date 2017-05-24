unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, ocv.comp.ViewFMX, ocv.comp.Types, ocv.comp.Source,
  ocv.comp.ImageOperation, ocv.comp.FFMSource, FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    ocvcmrsrc1: TocvCameraSource;
    ocvwfmx1: TocvViewFMX;
    chk1: TCheckBox;
    ocvpcmsrc1: TocvIPCamSource;
    ocvfmpgpcmsrc1: TocvFFMpegIPCamSource;
    ocvflsrc1: TocvFileSource;
    ocvmgprtn1: TocvImageOperation;
    ocvwfmx2: TocvViewFMX;
    ocvwfmx3: TocvViewFMX;
    ocvwfmx4: TocvViewFMX;
    chk2: TCheckBox;
    chk3: TCheckBox;
    chk4: TCheckBox;
    chk5: TCheckBox;
    procedure chk1Change(Sender: TObject);
    procedure chk5Change(Sender: TObject);
    procedure chk2Change(Sender: TObject);
    procedure chk3Change(Sender: TObject);
    procedure chk4Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.chk1Change(Sender: TObject);
begin
  ocvcmrsrc1.Enabled := chk1.IsChecked;
end;

procedure TMainForm.chk2Change(Sender: TObject);
begin
  ocvpcmsrc1.Enabled := chk2.IsChecked;
end;

procedure TMainForm.chk3Change(Sender: TObject);
begin
  ocvflsrc1.Enabled := chk3.IsChecked;
end;

procedure TMainForm.chk4Change(Sender: TObject);
begin
  ocvfmpgpcmsrc1.Enabled := chk4.IsChecked;
end;

procedure TMainForm.chk5Change(Sender: TObject);
begin
  ocvmgprtn1.Enabled := chk5.IsChecked;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  ocvcmrsrc1.Enabled := chk1.IsChecked;
  ocvpcmsrc1.Enabled := chk2.IsChecked;
  ocvflsrc1.Enabled := chk3.IsChecked;
  ocvfmpgpcmsrc1.Enabled := chk4.IsChecked;
end;

end.
