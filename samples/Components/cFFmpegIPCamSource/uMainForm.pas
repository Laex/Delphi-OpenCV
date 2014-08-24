unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ffm.libavcodec.avcodec,
  ocv.comp.Types, ocv.comp.Source, ocv.comp.View;

type
  TForm1 = class(TForm)
    ocvFFMpegIPCamSource1: TocvFFMpegIPCamSource;
    ocvView1: TocvView;
    procedure ocvFFMpegIPCamSource1LostConnection(Sender: TObject);
  private
    LCCount: Integer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ocvFFMpegIPCamSource1LostConnection(Sender: TObject);
Const
  LostConText = 'Lost connection ';
Var
  TW: Integer;
  LostConBmp: TBitmap;
begin
  LostConBmp := TBitmap.Create;
  LostConBmp.SetSize(ocvView1.Width, ocvView1.Height);
  LostConBmp.PixelFormat := pf24bit;
  TW := LostConBmp.Canvas.TextWidth(LostConText);
  LostConBmp.Canvas.TextOut((ocvView1.Width - TW) div 2, ocvView1.Height div 2, LostConText + ' ' + LCCount.ToString);
  inc(LCCount);
  ocvView1.DrawImage(TocvImage.Create(LostConBmp));
  LostConBmp.Free;
end;

end.
