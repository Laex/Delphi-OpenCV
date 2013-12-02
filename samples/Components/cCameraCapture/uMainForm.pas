unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uOCVTypes, uOCVImageOperation,
  uOCVCamera, uOCVView, Vcl.StdCtrls, uOCVSplitter;

type
  TMainForm = class(TForm)
    lbl1: TLabel;
    cbb1: TComboBox;
    chk1: TCheckBox;
    ocvcmr1: TocvCamera;
    ocvw1: TocvView;
    ocvmgprtn1: TocvImageOperation;
    ocvw2: TocvView;
    ocvspltr1: TocvSplitter;
    procedure FormCreate(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure chk1Click(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.cbb1Change(Sender: TObject);
begin
  ocvmgprtn1.Operation := TcvImageOperations(cbb1.ItemIndex);
end;

procedure TMainForm.chk1Click(Sender: TObject);
begin
  ocvcmr1.Enabled := chk1.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ocvw1.VideoSource := ocvspltr1.Channels[0];

  ocvmgprtn1.VideoSource := ocvspltr1.Channels[1];
  ocvw2.VideoSource := ocvmgprtn1;

  cbb1.ItemIndex := Integer(ocvmgprtn1.Operation);
  chk1.Checked := ocvcmr1.Enabled;
end;

end.
