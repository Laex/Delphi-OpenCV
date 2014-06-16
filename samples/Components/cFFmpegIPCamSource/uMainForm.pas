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
