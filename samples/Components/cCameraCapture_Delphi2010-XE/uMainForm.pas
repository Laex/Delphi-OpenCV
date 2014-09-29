unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ocv.comp.Types, ocv.comp.ImageOperation,
  ocv.core.types_c, ocv.comp.Source, ocv.comp.View, StdCtrls, ExtCtrls;

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
    procedure ocvmgprtn1OperationBeforeTransform(Sender: TObject; const IplImage: IocvImage; var ContinueTransform: Boolean);
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
  if (Operation is TocvHaarCascade) and (NextOperation is TocvCropOperation) then
  begin
    H := (Operation as TocvHaarCascade).HaarRects;
    if Length(H) > 0 then
      (NextOperation as TocvCropOperation).CropRect.ocvRect := H[0];
  end;
end;

procedure TMainForm.ocvmgprtn1OperationBeforeTransform(Sender: TObject; const IplImage: IocvImage;
  var ContinueTransform: Boolean);
begin
  With Sender as TocvWarpPerspective do
  begin
    DestQuad.Points[0].x := IplImage.width * 0.05; // dst Top left
    DestQuad.Points[0].y := IplImage.height * 0.33;
    DestQuad.Points[1].x := IplImage.width * 0.9; // dst Top right
    DestQuad.Points[1].y := IplImage.height * 0.25;
    DestQuad.Points[2].x := IplImage.width * 0.2; // dst Bottom left
    DestQuad.Points[2].y := IplImage.height * 0.7;
    DestQuad.Points[3].x := IplImage.width * 0.8; // dst Bot right
    DestQuad.Points[3].y := IplImage.height * 0.9;
  end;
end;

end.
