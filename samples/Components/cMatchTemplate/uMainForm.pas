unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uOCVTypes,
  uOCVImageOperation, uOCVSource, uOCVView;

type
  TMainForm = class(TForm)
    ocvw1: TocvView;
    ocvw2: TocvView;
    ocvcmrsrc1: TocvCameraSource;
    ocvmgprtn1: TocvImageOperation;
    btn1: TButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure ocvw2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ocvw2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ocvw2AfterPaint(Sender: TObject; const IplImage: IocvImage);
    procedure ocvw2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btn2Click(Sender: TObject);
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

Uses
  core_c,
  Core.types_c, cvUtils;

procedure TMainForm.btn1Click(Sender: TObject);
begin
  SnapImage := ocvcmrsrc1.Image;
  mX := 0;
  mY := 0;
  mX1 := 0;
  mY1 := 0;
  ocvw2.DrawImage(SnapImage);
end;

procedure TMainForm.btn2Click(Sender: TObject);
begin
  (ocvmgprtn1.Operation as TocvMatchTemplate).IPLTemplate := CropIplImage(SnapImage.IpImage, CvRect(
    {} Trunc(ocvcmrsrc1.Width * mX / ocvw2.Width),
    {} Trunc(ocvcmrsrc1.Height * mY / ocvw2.Height),
    {} Trunc(ocvcmrsrc1.Width * (mX1 - mX) / ocvw2.Width),
    {} Trunc(ocvcmrsrc1.Height * (mY1 - mY) / ocvw2.Height)));
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
  mDown := true;
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
