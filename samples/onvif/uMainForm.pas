unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ONVIF, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    onvfprb1: TONVIFProbe;
    btn1: TButton;
    tv1: TTreeView;
    procedure btn1Click(Sender: TObject);
    procedure onvfprb1ProbeMath(const ProbeMatch: TProbeMatch);
    procedure onvfprb1Completed(Sender: TObject);
    procedure tv1DblClick(Sender: TObject);
  private
    F: TProbeMatchArray;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uIPCameraLoginDlg;

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  tv1.Items.Clear;
  btn1.Enabled := False;
  onvfprb1.ExecuteAsync;
end;

procedure TForm1.onvfprb1Completed(Sender: TObject);
Var
  ProbeMatch: TProbeMatch;
  s: string;
  T, T1: TTreeNode;
begin
  F := UniqueProbeMatch(onvfprb1.ProbeMatchArray);
  tv1.Items.Clear;
  for ProbeMatch in F do
  begin
    T := tv1.Items.Add(nil, 'IP4: ' + ProbeMatch.XAddrs);
    if Length(ProbeMatch.XAddrsV6) > 0 then
      tv1.Items.AddChild(T, 'IP6: ' + ProbeMatch.XAddrsV6);
    T1 := tv1.Items.AddChild(T, 'Type:');
    if ptNetworkVideoTransmitter in ProbeMatch.Types then
      tv1.Items.AddChild(T1, 'NetworkVideoTransmitter');
    if ptDevice in ProbeMatch.Types then
      tv1.Items.AddChild(T1, 'Device');
    if ptNetworkVideoDisplay in ProbeMatch.Types then
      tv1.Items.AddChild(T1, 'NetworkVideoDisplay');
    T1 := tv1.Items.AddChild(T, 'Scopes:');
    for s in ProbeMatch.Scopes do
      tv1.Items.AddChild(T1, s);
    tv1.Items.AddChild(T, 'MetadataVersion: ' + ProbeMatch.MetadataVersion.ToString);
  end;
  btn1.Enabled := True;
end;

procedure TForm1.onvfprb1ProbeMath(const ProbeMatch: TProbeMatch);
begin
  tv1.Items.Add(nil, ProbeMatch.XAddrs);
end;

procedure TForm1.tv1DblClick(Sender: TObject);
Var
  T: TTreeNode;
  UserName, Password: string;
  XML: AnsiString;
  DeviceInformation: TDeviceInformation;
  Profiles: TProfiles;
begin
  if Assigned(tv1.Selected) then
  begin
    T := tv1.Selected;
    while Assigned(T.Parent) do
      T := T.Parent;
    if IPCameraLoginDlg(F[T.Index].XAddrs, UserName, Password) = mrOk then
    begin
      XML := ONVIFGetDeviceInformation(F[T.Index].XAddrs, UserName, Password);
      if XMLDeviceInformationToDeviceInformation(XML, DeviceInformation) then
      begin
        Application.MessageBox(PChar(F[T.Index].XAddrs + #13#10#13#10 + 'Manufacturer: ' + DeviceInformation.Manufacturer + #13#10 +
          'Model: ' + DeviceInformation.Model + #13#10 + 'FirmwareVersion: ' + DeviceInformation.FirmwareVersion + #13#10 + 'SerialNumber: '
          + DeviceInformation.SerialNumber + #13#10 + 'HardwareId: ' + DeviceInformation.HardwareId), 'IP camera device info', MB_OK);
      end;

      // get profiles
      XMLProfilesToProfiles(ONVIFGetProfiles(GetONVIFAddr(F[T.Index].XAddrs, atMedia), UserName, Password), Profiles);

    end;
  end;
end;

end.
