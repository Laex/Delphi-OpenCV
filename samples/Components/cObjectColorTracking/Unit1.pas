unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ocv.comp.View, ocv.comp.Types,
  ocv.comp.Source, ocv.comp.ImageOperation, Vcl.ComCtrls, Unit2, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ocvcmrsrc1: TocvCameraSource;
    ocvw1: TocvView;
    ocvmgprtn1: TocvImageOperation;
    frm: TFrame2;
    frm21: TFrame2;
    frm2: TFrame2;
    frm3: TFrame2;
    frm4: TFrame2;
    frm5: TFrame2;
    ocvw2: TocvView;
    pnl1: TPanel;
    frm22: TFrame2;
    pnl2: TPanel;
    chk1: TCheckBox;
    procedure frm3trckbr1Change(Sender: TObject);
    procedure ocvw1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure frm22trckbr1Change(Sender: TObject);
    procedure ocvw1BeforePaint(Sender: TObject; var IplImage: IocvImage);
  private
    SelectedPixel: TocvPixel;
    procedure UpdateTrack;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Uses ocv.core.types_c,
  System.Math,
  ocv.core_c,
  ocv.utils, ocv.imgproc_c, ocv.imgproc.types_c;

procedure TForm1.frm22trckbr1Change(Sender: TObject);
begin
  frm22.trckbr1Change(Sender);
  UpdateTrack;
end;

procedure TForm1.frm3trckbr1Change(Sender: TObject);
Var
  R, G, B: Byte;
begin
  ((Sender as TTrackBar).Parent as TFrame2).trckbr1Change(Sender);
  ((ocvmgprtn1.Operations.Items[2] as TocvImageOperationCollectionItem).Operation as TocvInRangeSOperation)
    .Lower.CvScalar := CvScalar(frm3.trckbr1.Position, frm2.trckbr1.Position, frm21.trckbr1.Position);
  ((ocvmgprtn1.Operations.Items[2] as TocvImageOperationCollectionItem).Operation as TocvInRangeSOperation)
    .Upper.CvScalar := CvScalar(frm.trckbr1.Position, frm4.trckbr1.Position, frm5.trckbr1.Position);
  ocvHSVToRGB(frm3.trckbr1.Position + frm22.trckbr1.Position, frm2.trckbr1.Position + frm22.trckbr1.Position,
    frm21.trckbr1.Position + frm22.trckbr1.Position, R, G, B);
  pnl2.Color := RGB(Trunc(R), Trunc(G), Trunc(B));
end;

procedure TForm1.ocvw1BeforePaint(Sender: TObject; var IplImage: IocvImage);
Var
  p, p1: IocvImage;
begin
  if chk1.Checked then
  begin
    p := ocvw1.Image.Clone;
    p1 := ocvw1.Image.Clone;
    cvCvtColor(ocvw2.Image.IpImage, p.IpImage, CV_GRAY2BGR);
    cvAnd(IplImage.IpImage, p.IpImage, p1.IpImage);
    IplImage := p1;
  end;
end;

procedure TForm1.ocvw1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  O: TocvView;
  xx, yy: Integer;
begin
  O := (Sender as TocvView);
  xx := Trunc(O.Image.Width * X / O.Width);
  yy := Trunc(O.Image.Height * Y / O.Height);
  SelectedPixel := O.Image.Pixel[xx, yy];
  UpdateTrack;
  chk1.Enabled:=True;
end;

procedure TForm1.UpdateTrack;
Var
  H, S, V: Byte;
  R, G, B: Byte;
  H1, S1, V1: Integer;
begin
  pnl1.Caption := '(R:' + SelectedPixel.R.ToString() + ',G:' + SelectedPixel.G.ToString() + ',B:' +
    SelectedPixel.B.ToString() + ')';
  pnl1.Color := RGB(SelectedPixel.R, SelectedPixel.G, SelectedPixel.B);
  ocvRGBToHSV(SelectedPixel.R, SelectedPixel.G, SelectedPixel.B, H, S, V);
  H1 := Trunc(H);
  S1 := Trunc(S);
  V1 := Trunc(V);
  frm3.trckbr1.Position := Trunc(H1 - frm22.trckbr1.Position);
  frm.trckbr1.Position := Trunc(H1 + frm22.trckbr1.Position);
  frm2.trckbr1.Position := Trunc(S1 - frm22.trckbr1.Position);
  frm4.trckbr1.Position := Trunc(S1 + frm22.trckbr1.Position);
  frm21.trckbr1.Position := Trunc(V1 - frm22.trckbr1.Position);
  frm5.trckbr1.Position := Trunc(V1 + frm22.trckbr1.Position);
  ocvHSVToRGB(H, S, V, R, G, B);
  pnl2.Caption := '(H:' + H1.ToString + ',S:' + S1.ToString + ',V:' + V1.ToString + ')';
  pnl2.Color := RGB(Trunc(R), Trunc(G), Trunc(B));
end;

end.
