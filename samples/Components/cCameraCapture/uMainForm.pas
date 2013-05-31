unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uOCVTypes, uOCVImageOperation,
  uOCVCamera, uOCVView, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    lbl1: TLabel;
    cbb1: TComboBox;
    chk1: TCheckBox;
    ocvw1: TocvView;
    ocvcmr1: TocvCamera;
    ocvmgprtn1: TocvImageOperation;
    procedure FormCreate(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure chk1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.cbb1Change(Sender: TObject);
begin
  ocvmgprtn1.ImageOperation := TcvImageOperation(cbb1.ItemIndex);
end;

procedure TMainForm.chk1Click(Sender: TObject);
begin
  ocvcmr1.Enabled := chk1.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cbb1.ItemIndex := Integer(ocvmgprtn1.ImageOperation);
  chk1.Checked := ocvcmr1.Enabled;
end;

end.
