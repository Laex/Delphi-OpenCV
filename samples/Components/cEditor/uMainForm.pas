unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ocv.comp.View, ocv.comp.Types,
  ocv.comp.Source, Vcl.StdCtrls, ocv.comp.ImageOperation;

type
  TForm1 = class(TForm)
    chk1: TCheckBox;
    ocvcmrsrc1: TocvCameraSource;
    ocvw1: TocvView;
    ocvmgprtn1: TocvImageOperation;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
