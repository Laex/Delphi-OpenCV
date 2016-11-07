program OnvifDemo;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uIPCameraLoginDlg in 'uIPCameraLoginDlg.pas' {IPCameraLoginDlgDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
