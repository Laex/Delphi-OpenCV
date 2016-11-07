unit uIPCameraLoginDlg;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TIPCameraLoginDlgDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    lbl1: TLabel;
    edt1: TEdit;
    lbl2: TLabel;
    edt2: TEdit;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function IPCameraLoginDlg(const Host: String; Var UserName, Password: String): Integer;

implementation

{$R *.dfm}

function IPCameraLoginDlg(const Host: String; Var UserName, Password: String): Integer;
begin
  with TIPCameraLoginDlgDlg.Create(nil) do
    try
      edt1.Text := UserName;
      edt2.Text := Password;
      Caption := 'Camera ' + Host;
      Result := ShowModal;
      if Result = mrOk then
      begin
        UserName := edt1.Text;
        Password := edt2.Text;
      end;
    finally
      Free;
    end;
end;

procedure TIPCameraLoginDlgDlg.FormActivate(Sender: TObject);
begin
  edt1.SetFocus;
end;

end.
