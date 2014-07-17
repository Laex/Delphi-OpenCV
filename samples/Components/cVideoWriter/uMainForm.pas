unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ocv.comp.View,
  ocv.comp.Types, ocv.comp.VideoWriter, ocv.comp.Source;

type
  TForm2 = class(TForm)
    ocvcmrsrc1: TocvCameraSource;
    ocvdwrtr1: TocvVideoWriter;
    ocvw1: TocvView;
    lbl1: TLabel;
    edt1: TEdit;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    procedure btn1Click1(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btn1Click1(Sender: TObject);
begin
  btn1.Enabled := False;
  ocvdwrtr1.Enabled := False;
  btn1.OnClick := btn1Click;
  btn1.Enabled := True;
  btn1.Caption:='Save media';
end;

procedure TForm2.btn1Click(Sender: TObject);
begin
  btn1.Enabled := False;
  ocvdwrtr1.FileName := edt1.Text;
  ocvdwrtr1.Enabled := True;
  btn1.OnClick := btn1Click1;
  btn1.Caption:='Stop';
  btn1.Enabled := True;
end;

end.
