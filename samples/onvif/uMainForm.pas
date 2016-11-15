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
  T, T1, T2, T3, T4: TTreeNode;
  UserName, Password: string;
  XML: AnsiString;
  DeviceInformation: TDeviceInformation;
  Profiles: TProfiles;
  i: Integer;
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
        T1 := T.GetLastChild;
        if (Pos('DeviceInformation', T1.Text) = 0) and (Pos('Profiles', T1.Text) = 0) then
        begin
          T1 := tv1.Items.AddChild(T, 'DeviceInformation');
          tv1.Items.AddChild(T1, 'Manufacturer: ' + DeviceInformation.Manufacturer);
          tv1.Items.AddChild(T1, 'Model: ' + DeviceInformation.Model);
          tv1.Items.AddChild(T1, 'FirmwareVersion: ' + DeviceInformation.FirmwareVersion);
          tv1.Items.AddChild(T1, 'SerialNumber: ' + DeviceInformation.SerialNumber);
          tv1.Items.AddChild(T1, 'HardwareId: ' + DeviceInformation.HardwareId);
        end;
      end;
      T1 := T.GetLastChild;
      if Pos('Profiles', T1.Text) = 0 then
      begin
        T1 := tv1.Items.AddChild(T, 'Profiles');
        XMLProfilesToProfiles(ONVIFGetProfiles(GetONVIFAddr(F[T.Index].XAddrs, atMedia), UserName, Password), Profiles);
        for i := 0 to High(Profiles) do
        begin
          T2 := tv1.Items.AddChild(T1, 'Name: ' + Profiles[i].Name);
          tv1.Items.AddChild(T2, 'fixed: ' + Profiles[i].fixed.ToString(True));
          tv1.Items.AddChild(T2, 'token: ' + Profiles[i].token);
          T3 := tv1.Items.AddChild(T2, 'VideoSourceConfiguration');
          tv1.Items.AddChild(T3, 'Name: ' + Profiles[i].VideoSourceConfiguration.Name);
          tv1.Items.AddChild(T3, 'token: ' + Profiles[i].VideoSourceConfiguration.token);
          tv1.Items.AddChild(T3, 'UseCount: ' + Profiles[i].VideoSourceConfiguration.UseCount.ToString);
          tv1.Items.AddChild(T3, 'SourceToken: ' + Profiles[i].VideoSourceConfiguration.SourceToken);
          tv1.Items.AddChild(T3, 'Bounds: ' + Format('(x:%d, y:%d, width:%d, height:%d)', [Profiles[i].VideoSourceConfiguration.Bounds.x,
            Profiles[i].VideoSourceConfiguration.Bounds.y, Profiles[i].VideoSourceConfiguration.Bounds.width,
            Profiles[i].VideoSourceConfiguration.Bounds.Height]));
          T3 := tv1.Items.AddChild(T2, 'VideoEncoderConfiguration');
          tv1.Items.AddChild(T3, 'Name: ' + Profiles[i].VideoEncoderConfiguration.Name);
          tv1.Items.AddChild(T3, 'token: ' + Profiles[i].VideoEncoderConfiguration.token);
          tv1.Items.AddChild(T3, 'UseCount: ' + Profiles[i].VideoEncoderConfiguration.UseCount.ToString);
          tv1.Items.AddChild(T3, 'Encoding: ' + Profiles[i].VideoEncoderConfiguration.Encoding);
          tv1.Items.AddChild(T3, 'Resolution: ' + Format('(width:%d, height:%d)', [Profiles[i].VideoEncoderConfiguration.Resolution.width,
            Profiles[i].VideoEncoderConfiguration.Resolution.Height]));
          tv1.Items.AddChild(T3, 'Quality: ' + Profiles[i].VideoEncoderConfiguration.Quality.ToString);
          T4 := tv1.Items.AddChild(T3, 'RateControl');
          tv1.Items.AddChild(T4, 'FrameRateLimit: ' + Profiles[i].VideoEncoderConfiguration.RateControl.FrameRateLimit.ToString);
          tv1.Items.AddChild(T4, 'EncodingInterval: ' + Profiles[i].VideoEncoderConfiguration.RateControl.EncodingInterval.ToString);
          tv1.Items.AddChild(T4, 'BitrateLimit: ' + Profiles[i].VideoEncoderConfiguration.RateControl.BitrateLimit.ToString);
          T4 := tv1.Items.AddChild(T3, 'H264');
          tv1.Items.AddChild(T4, 'GovLength: ' + Profiles[i].VideoEncoderConfiguration.H264.GovLength.ToString);
          tv1.Items.AddChild(T4, 'GovLength: ' + Profiles[i].VideoEncoderConfiguration.H264.H264Profile);
          T4 := tv1.Items.AddChild(T3, 'Multicast');
          tv1.Items.AddChild(T4, 'Address type: ' + Profiles[i].VideoEncoderConfiguration.Multicast.Address.Type_);
          tv1.Items.AddChild(T4, 'Address IPv4Address: ' + Profiles[i].VideoEncoderConfiguration.Multicast.Address.IPv4Address);
          tv1.Items.AddChild(T4, 'Port: ' + Profiles[i].VideoEncoderConfiguration.Multicast.Port.ToString);
          tv1.Items.AddChild(T4, 'TTL: ' + Profiles[i].VideoEncoderConfiguration.Multicast.TTL.ToString);
          tv1.Items.AddChild(T4, 'AutoStart: ' + Profiles[i].VideoEncoderConfiguration.Multicast.AutoStart.ToString(True));

          tv1.Items.AddChild(T3, 'SessionTimeout: ' + Profiles[i].VideoEncoderConfiguration.SessionTimeout);
        end;
      end;

    end;
  end;
end;

end.
