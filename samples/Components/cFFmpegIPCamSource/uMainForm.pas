unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uOCVTypes, uOCVSource, uOCVView, ffm.libavcodec.avcodec;

type
  TForm1 = class(TForm)
    ocvFFMpegIPCamSource1: TocvFFMpegIPCamSource;
    ocvView1: TocvView;
  private
    {Private declarations}
  public
    {Public declarations}
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
