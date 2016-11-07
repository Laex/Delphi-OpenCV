unit ONVIF;

interface

Uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  IdUDPServer,
  IdGlobal,
  IdSocketHandle;

Type
  TProbeType = (ptNetworkVideoTransmitter, ptDevice, ptNetworkVideoDisplay);
  TProbeTypeSet = set of TProbeType;

  TProbeMatchXMLArray = TArray<string>;

  TProbeMatch = record
    Types: TProbeTypeSet;
    Scopes: TArray<string>;
    XAddrs: String;
    XAddrsV6: string;
    MetadataVersion: Integer;
  end;

  TProbeMatchArray = TArray<TProbeMatch>;

  TProbeMathNotify = procedure(const ProbeMatch: TProbeMatch) of object;
  TProbeMathXMLNotify = procedure(const ProbeMatchXML: String) of object;

  TONVIFProbeThread = class;

  TONVIFProbe = class(TComponent)
  private
    FONVIFProbeThread: TONVIFProbeThread;
    FOnProbeMathXML: TProbeMathXMLNotify;
    FOnCompleted: TNotifyEvent;
    FOnProbeMath: TProbeMathNotify;
    FProbeType: TProbeTypeSet;
    FTimeout: Cardinal;
    function GetCount: Integer;
    function GetProbeMatch(const Index: Integer): TProbeMatch;
    function GetProbeMatchXML(const Index: Integer): String;
    function GetProbeMatchArray: TProbeMatchArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    function ExecuteAsync: Boolean;
    property Count: Integer read GetCount;
    property ProbeMatchXML[const Index: Integer]: String Read GetProbeMatchXML;
    property ProbeMatch[const Index: Integer]: TProbeMatch Read GetProbeMatch;
    property ProbeMatchArray: TProbeMatchArray read GetProbeMatchArray;
  published
    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;
    property OnProbeMath: TProbeMathNotify read FOnProbeMath write FOnProbeMath;
    property OnProbeMathXML: TProbeMathXMLNotify read FOnProbeMathXML write FOnProbeMathXML;
    property ProbeType: TProbeTypeSet read FProbeType write FProbeType default [ptNetworkVideoTransmitter, ptDevice, ptNetworkVideoDisplay];
    property Timeout: Cardinal read FTimeout write FTimeout default 1000;
  end;

  TONVIFProbeThread = class(TThread)
  private
    E: TEvent;
    FProbeMatchXML: TProbeMatchXMLArray;
    FProbeTypeSet: TProbeTypeSet;
    FTimeout: Cardinal;
    FProbeMatch: TProbeMatchArray;
    FProbeMathNotify: TProbeMathNotify;
    FProbeMathXMLNotify: TProbeMathXMLNotify;
    procedure UDPServerUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
  protected
    procedure Execute; override;
  public
    constructor Create(const ProbeMathNotify: TProbeMathNotify = nil; const ProbeMathXMLNotify: TProbeMathXMLNotify = nil;
      const ProbeTypeSet: TProbeTypeSet = [ptNetworkVideoTransmitter, ptDevice, ptNetworkVideoDisplay]; const Timeout: Cardinal = 1000);
    property ProbeMatchXML: TProbeMatchXMLArray read FProbeMatchXML;
    property ProbeMatch: TProbeMatchArray read FProbeMatch;
  end;

function ONVIFProbe: TProbeMatchArray;
function XMLToProbeMatch(const ProbeMatchXML: string; Var ProbeMatch: TProbeMatch): Boolean;
function UniqueProbeMatch(const ProbeMatch: TProbeMatchArray): TProbeMatchArray;

Type
  TDeviceInformation = record
    Manufacturer: string;
    Model: string;
    FirmwareVersion: String;
    SerialNumber: String;
    HardwareId: String;
  end;

  // Addr -> http://<host>/onvif/device_service
function ONVIFGetDeviceInformation(const Addr, UserName, Password: AnsiString): AnsiString;
function XMLDeviceInformationToDeviceInformation(const XMLDeviceInformation: AnsiString; Var DeviceInformation: TDeviceInformation)
  : Boolean;
function PrepareGetDeviceInformationRequest(const UserName, Password: AnsiString): AnsiString;

Type
  TProfile = record
    fixed: Boolean;
    token: string;
    Name: String;

    VideoSourceConfiguration: record
{$REGION 'VideoSourceConfiguration'}
      token: string;
      Name: String;
      UseCount: Integer;
      SourceToken: string;

      Bounds: record
        x: Integer;
        y: Integer;
        width: Integer;
        height: Integer;
      end;
{$ENDREGION}
    end;

    VideoEncoderConfiguration: record
{$REGION 'VideoEncoderConfiguration'}
      token: String;
      Name: String;
      UseCount: Integer;
      Encoding: string;

      Resolution: record
        width: Integer;
        height: Integer;
      end;

      Quality: Double;

      RateControl: record
        FrameRateLimit: Integer;
        EncodingInterval: Integer;
        BitrateLimit: Integer;
      end;

      H264: record
        GovLength: Integer;
        H264Profile: String;
      end;

      Multicast: record
        Address: record
          Type_: String;
          IPv4Address: String;
        end;

        Port: Word;
        TTL: Integer;
        AutoStart: Boolean;
      end;

      SessionTimeout: String;
{$ENDREGION}
    end;

    AudioEncoderConfiguration: record
{$REGION 'AudioEncoderConfiguration'}
      token: string;
      Name: string;
      UseCount: Integer;
      Encoding: string;
      Bitrate: Integer;
      SampleRate: Integer;

      Multicast: record
        Address: record
          Type_: string;
          IPv4Address: string;
        end;

        Port: Word;
        TTL: Integer;
        AutoStart: Boolean;
      end;

      SessionTimeout: String;
{$ENDREGION}
    end;

    (*
      <tt:VideoAnalyticsConfiguration token="000">
      <tt:Name>VideoAnalyticsConfig_000</tt:Name>
      <tt:UseCount>2</tt:UseCount>
      <tt:AnalyticsEngineConfiguration>
      <tt:AnalyticsModule Type="tt:CellMotionEngine" Name="tt:CellMotionEngine">
      <tt:Parameters>
      <tt:SimpleItem Value="48" Name="Sensitivity"/>
      <tt:ElementItem Name="Layout">
      <tt:CellLayout Columns="22" Rows="18">
      <tt:Transformation>
      <tt:Translate x="-1.0" y="-1.0"/>
      <tt:Scale x="0.00625" y="0.00834"/>
      </tt:Transformation>
      </tt:CellLayout>
      </tt:ElementItem>
      </tt:Parameters>
      </tt:AnalyticsModule>
      <tt:AnalyticsModule Type="tt:TamperEngine" Name="tt:TamperEngine">
      <tt:Parameters>
      <tt:SimpleItem Value="48" Name="Sensitivity"/>
      <tt:ElementItem Name="Field">
      <ns1:PolygonConfiguration>
      <ns1:Polygon>
      <Point x="0" y="0"/>
      <Point x="0" y="0"/>
      <Point x="0" y="0"/>
      <Point x="0" y="0"/>
      </ns1:Polygon>
      </ns1:PolygonConfiguration>
      </tt:ElementItem>
      <tt:ElementItem Name="Transform">
      <tt:Transformation>
      <tt:Translate x="-1.0" y="-1.0"/>
      <tt:Scale x="0.001250" y="0.001667"/>
      </tt:Transformation>
      </tt:ElementItem>
      </tt:Parameters>
      </tt:AnalyticsModule>
      </tt:AnalyticsEngineConfiguration>
      <tt:RuleEngineConfiguration>
      <tt:Rule Type="tt:CellMotionDetector" Name="tt:CellMotionDetector">
      <tt:Parameters>
      <tt:SimpleItem Value="zwA=" Name="ActiveCells"/>
      <tt:SimpleItem Value="1000" Name="AlarmOffDelay"/>
      <tt:SimpleItem Value="1000" Name="AlarmOnDelay"/>
      <tt:SimpleItem Value="4" Name="MinCount"/>
      </tt:Parameters>
      </tt:Rule>
      <tt:Rule Type="tt:TamperDetector" Name="tt:TamperDetector">
      <tt:Parameters>
      <tt:ElementItem Name="Field">
      <ns1:PolygonConfiguration>
      <ns1:Polygon>
      <Point x="0" y="0"/>
      <Point x="0" y="0"/>
      <Point x="0" y="0"/>
      <Point x="0" y="0"/>
      </ns1:Polygon>
      </ns1:PolygonConfiguration>
      </tt:ElementItem>
      </tt:Parameters>
      </tt:Rule>
      </tt:RuleEngineConfiguration>
      </tt:VideoAnalyticsConfiguration>
    *)

    PTZConfiguration: record
      token: String;
      Name: string;
      UseCount: Integer;
      NodeToken: String;
      DefaultContinuousPanTiltVelocitySpace: string;
      DefaultContinuousZoomVelocitySpace: string;
      DefaultPTZTimeout: String;
    end;

    Extension: record
      AudioOutputConfiguration: record
        token: String;
        Name: String;
        UseCount: Integer;
        OutputToken: String;
        SendPrimacy: string;
        OutputLevel: Integer;
      end;

      AudioDecoderConfiguration: record
        token: string;
        Name: String;
        UseCount: Integer;
      end;
    end;
  end;

  TProfiles = TArray<TProfile>;

  // Addr -> http://<host>/onvif/Media
function ONVIFGetProfiles(const Addr, UserName, Password: AnsiString): AnsiString;
function XMLProfilesToProfiles(const XMLProfiles: AnsiString; Var Profiles: TProfiles): Boolean;
function PrepareGetProfilesRequest(const UserName, Password: AnsiString): AnsiString;

type
  TStreamUri = record
    Uri: string;
    InvalidAfterConnect: Boolean;
    InvalidAfterReboot: Boolean;
    Timeout: String;
  end;

  // Protocol -> HTTP or RTSP
  // Addr -> http://<host>/onvif/Media
function ONVIFGetStreamUri(const Addr, UserName, Password, Stream, Protocol, ProfileToken: AnsiString): AnsiString;
function XMLStreamUriToStreamUri(const XMLStreamUri: AnsiString; Var StreamUri: TStreamUri): Boolean;
function PrepareGetStreamUriRequest(const UserName, Password, Stream, Protocol, ProfileToken: AnsiString): AnsiString;

procedure ONVIFRequest(const Addr: AnsiString; const InStream, OutStream: TStringStream); overload;
procedure ONVIFRequest(const Addr, Request: AnsiString; Var Answer: AnsiString); overload;

procedure GetONVIFPasswordDigest(const UserName, Password: AnsiString; Var PasswordDigest, Nonce, Created: AnsiString);
function GetONVIFDateTime(const DateTime: TDateTime): AnsiString;
function BytesToAnsiString(Data: TBytes): AnsiString; inline;
function SHA1(const Data: TBytes): TBytes;

Type
  TONVIFAddrType = (atDeviceService, atMedia);

function GetONVIFAddr(const XAddr: string; const ONVIFAddrType: TONVIFAddrType): string;

procedure Register;

implementation

Uses
  System.Generics.Defaults,
  System.Generics.Collections,
  System.NetEncoding,
  IdHashSHA,
  IdHTTP,
  IdURI,
  uNativeXML;

procedure Register;
begin
  RegisterComponents('ONVIF', [TONVIFProbe]);
end;

const
  onvifDeviceService = 'device_service';
  onvifMedia = 'Media';

function ONVIFProbe: TProbeMatchArray;
Var
  F: TONVIFProbeThread;
begin
  F := TONVIFProbeThread.Create;
  try
    F.WaitFor;
    Result := F.ProbeMatch;
  finally
    F.Free;
  end;
end;

function GetONVIFAddr(const XAddr: string; const ONVIFAddrType: TONVIFAddrType): string;
Var
  Uri: TIdURI;
begin
  Uri := TIdURI.Create(XAddr);
  try
    case ONVIFAddrType of
      atDeviceService:
        Uri.Document := onvifDeviceService;
      atMedia:
        Uri.Document := onvifMedia;
    end;
    Result := Uri.Uri;
  finally
    Uri.Free;
  end;
end;

function BytesToAnsiString(Data: TBytes): AnsiString; inline;
begin
  SetLength(Result, Length(Data));
  Move(Data[0], Result[1], Length(Data));
end;

procedure ONVIFRequest(const Addr, Request: AnsiString; Var Answer: AnsiString);
Var
  InStream, OutStream: TStringStream;
begin
  InStream := TStringStream.Create(Request);
  OutStream := TStringStream.Create;
  try
    ONVIFRequest(Addr, InStream, OutStream);
    Answer := OutStream.DataString;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

procedure ONVIFRequest(const Addr: AnsiString; const InStream, OutStream: TStringStream);
Var
  idhtp1: TIdHTTP;
  Uri: TIdURI;
begin
  idhtp1 := TIdHTTP.Create;
  Uri := TIdURI.Create(Addr);
  try
    With idhtp1 do
    begin
      AllowCookies := True;
      HandleRedirects := True;
      Request.Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
      Request.UserAgent := 'Mozilla/3.0 (compatible; Indy Library)';
      Request.Host := '';
      Request.Connection := '';
      Request.Accept := '';
      Request.UserAgent := '';

      Request.CustomHeaders.Clear;
      Request.ContentType := 'text/xml;charset=utf-8';
      Request.CustomHeaders.Add('Host: ' + Uri.Host);

      ProtocolVersion := pv1_1;
      HTTPOptions := [hoNoProtocolErrorException, hoWantProtocolErrorContent];
      Post(Addr, InStream, OutStream);
    end;
  finally
    idhtp1.Free;
  end;
end;

function ONVIFGetDeviceInformation(const Addr, UserName, Password: AnsiString): AnsiString;
begin
  // Addr -> http://<host>/onvif/device_service
  ONVIFRequest(Addr, PrepareGetDeviceInformationRequest(UserName, Password), Result);
end;

function XMLDeviceInformationToDeviceInformation(const XMLDeviceInformation: AnsiString; Var DeviceInformation: TDeviceInformation)
  : Boolean;
var
  SS: TStringStream;
  XmlNode, Node: TXmlNode;
  Xml: TNativeXML;
begin
  Xml := TNativeXML.Create;
  SS := TStringStream.Create(XMLDeviceInformation);
  DeviceInformation := default (TDeviceInformation);
  Result := False;
  try
    Xml.LoadFromStream(SS);
    XmlNode := Xml.Root.NodeByName('Body');
    if Assigned(XmlNode) then
    begin
      XmlNode := XmlNode.NodeByName('GetDeviceInformationResponse');
      if Assigned(XmlNode) then
      begin
        Node := XmlNode.NodeByName('Manufacturer');
        if Assigned(Node) then
        begin
          DeviceInformation.Manufacturer := Node.ValueUnicode;
          Result := True;
        end;

        Node := XmlNode.NodeByName('Model');
        if Assigned(Node) then
        begin
          DeviceInformation.Model := Node.ValueUnicode;
          Result := True;
        end;

        Node := XmlNode.NodeByName('FirmwareVersion');
        if Assigned(Node) then
        begin
          DeviceInformation.FirmwareVersion := Node.ValueUnicode;
          Result := True;
        end;

        Node := XmlNode.NodeByName('SerialNumber');
        if Assigned(Node) then
        begin
          DeviceInformation.SerialNumber := Node.ValueUnicode;
          Result := True;
        end;

        Node := XmlNode.NodeByName('HardwareId');
        if Assigned(Node) then
        begin
          DeviceInformation.HardwareId := Node.ValueUnicode;
          Result := True;
        end;

      end;
    end;
  finally
    Xml.Free;
    SS.Free;
  end;
end;

function PrepareGetDeviceInformationRequest(const UserName, Password: AnsiString): AnsiString;
const
  GetDeviceInformationFmt: AnsiString = // PasswordDigest,Nonce,Created // http://<host>/onvif/device_service
    '<?xml version="1.0"?> ' + //
    '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:wsdl="http://www.onvif.org/ver10/device/wsdl"> ' + //
    '<soap:Header>' + //
    '<Security xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" s:mustUnderstand="1"> ' + //
    '<UsernameToken> ' + //
    '<Username>admin</Username> ' + //
    '<Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">%s</Password> ' +
    '<Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">%s</Nonce> ' +
    '<Created xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">%s</Created> ' + //
    '</UsernameToken> ' + //
    '</Security> ' + //
    '</soap:Header>' + //
    '<soap:Body> ' + //
    '<wsdl:GetDeviceInformation/> ' + //
    '</soap:Body> ' + //
    '</soap:Envelope>';
Var
  PasswordDigest, Nonce, Created: AnsiString;
begin
  GetONVIFPasswordDigest(UserName, Password, PasswordDigest, Nonce, Created);
  Result := Format(GetDeviceInformationFmt, [PasswordDigest, Nonce, Created]);
end;

function ONVIFGetProfiles(const Addr, UserName, Password: AnsiString): AnsiString;
begin
  // Addr -> http://<host>/onvif/Media
  ONVIFRequest(Addr, PrepareGetProfilesRequest(UserName, Password), Result);
end;

function XMLProfilesToProfiles(const XMLProfiles: AnsiString; Var Profiles: TProfiles): Boolean;
var
  SS: TStringStream;
  XmlNode, Node, N, M, K: TXmlNode;
  Xml: TNativeXML;
  i: Integer;
  Profile: TProfile;
begin
  Xml := TNativeXML.Create;
  SS := TStringStream.Create(XMLProfiles);
  Result := False;
  try
    Xml.LoadFromStream(SS);
    XmlNode := Xml.Root.NodeByName('Body');
    if Assigned(XmlNode) then
    begin
      XmlNode := XmlNode.NodeByName('GetProfilesResponse');
      if Assigned(XmlNode) then
      begin
        Profile := default (TProfile);
        for i := 0 to XmlNode.ContainerCount - 1 do
        begin
          Node := XmlNode.Containers[i];
          Profile.fixed := string(Node.AttributeValueByName['fixed']).ToBoolean;
          Profile.token := string(Node.AttributeValueByName['token']);
          N := Node.NodeByName('Name');
          if Assigned(N) then
            Profile.Name := N.ValueUnicode;
          N := Node.NodeByName('VideoSourceConfiguration');
          if Assigned(N) then
          begin
            Profile.VideoSourceConfiguration.token := string(N.AttributeValueByName['token']);
            M := N.NodeByName('Name');
            if Assigned(M) then
              Profile.VideoSourceConfiguration.Name := M.ValueUnicode;
            M := N.NodeByName('UseCount');
            if Assigned(M) then
              Profile.VideoSourceConfiguration.UseCount := M.ValueUnicode.ToInteger;
            M := N.NodeByName('SourceToken');
            if Assigned(M) then
              Profile.VideoSourceConfiguration.SourceToken := M.ValueUnicode;
            M := N.NodeByName('Bounds');
            if Assigned(M) then
            begin
              Profile.VideoSourceConfiguration.Bounds.x := string(M.AttributeValueByName['x']).ToInteger;
              Profile.VideoSourceConfiguration.Bounds.y := string(M.AttributeValueByName['y']).ToInteger;
              Profile.VideoSourceConfiguration.Bounds.width := string(M.AttributeValueByName['width']).ToInteger;
              Profile.VideoSourceConfiguration.Bounds.height := string(M.AttributeValueByName['height']).ToInteger;
            end;
          end;
          N := Node.NodeByName('VideoEncoderConfiguration');
          if Assigned(N) then
          begin
            Profile.VideoEncoderConfiguration.token := string(N.AttributeValueByName['token']);
            M := N.NodeByName('Name');
            if Assigned(M) then
              Profile.VideoEncoderConfiguration.Name := M.ValueUnicode;
            M := N.NodeByName('UseCount');
            if Assigned(M) then
              Profile.VideoEncoderConfiguration.UseCount := M.ValueUnicode.ToInteger;
            M := N.NodeByName('Encoding');
            if Assigned(M) then
              Profile.VideoEncoderConfiguration.Encoding := M.ValueUnicode;
            M := N.NodeByName('Resolution');
            if Assigned(M) then
            begin
              K := M.NodeByName('Width');
              Profile.VideoEncoderConfiguration.Resolution.width := K.ValueUnicode.ToInteger;
              K := M.NodeByName('Height');
              Profile.VideoEncoderConfiguration.Resolution.height := K.ValueUnicode.ToInteger;
            end;
            M := N.NodeByName('Quality');
            if Assigned(M) then
              Profile.VideoEncoderConfiguration.Quality := M.ValueUnicode.ToDouble;
            M := N.NodeByName('RateControl');
            if Assigned(M) then
            begin
              K := M.NodeByName('FrameRateLimit');
              Profile.VideoEncoderConfiguration.RateControl.FrameRateLimit := K.ValueUnicode.ToInteger;
              K := M.NodeByName('EncodingInterval');
              Profile.VideoEncoderConfiguration.RateControl.EncodingInterval := K.ValueUnicode.ToInteger;
              K := M.NodeByName('BitrateLimit');
              Profile.VideoEncoderConfiguration.RateControl.BitrateLimit := K.ValueUnicode.ToInteger;
            end;
            M := N.NodeByName('H264');
            if Assigned(M) then
            begin
              K := M.NodeByName('GovLength');
              Profile.VideoEncoderConfiguration.H264.GovLength := K.ValueUnicode.ToInteger;
              K := M.NodeByName('H264Profile');
              Profile.VideoEncoderConfiguration.H264.H264Profile := K.ValueUnicode;
            end;
            M := N.NodeByName('Multicast');
            if Assigned(M) then
            begin
              K := M.NodeByName('Address');
              Profile.VideoEncoderConfiguration.Multicast.Address.Type_ := K.NodeByName('Type').ValueUnicode;
              Profile.VideoEncoderConfiguration.Multicast.Address.IPv4Address := K.NodeByName('IPv4Address').ValueUnicode;
              K := M.NodeByName('Port');
              Profile.VideoEncoderConfiguration.Multicast.Port := K.ValueUnicode.ToInteger;
              K := M.NodeByName('TTL');
              Profile.VideoEncoderConfiguration.Multicast.TTL := K.ValueUnicode.ToInteger;
              K := M.NodeByName('AutoStart');
              Profile.VideoEncoderConfiguration.Multicast.AutoStart := K.ValueUnicode.ToBoolean;
            end;
            M := N.NodeByName('SessionTimeout');
            if Assigned(M) then
              Profile.VideoEncoderConfiguration.SessionTimeout := M.ValueUnicode;
          end;
          N := Node.NodeByName('AudioEncoderConfiguration');
          if Assigned(N) then
          begin
            Profile.AudioEncoderConfiguration.token := string(N.AttributeValueByName['token']);
            M := N.NodeByName('Name');
            if Assigned(M) then
              Profile.AudioEncoderConfiguration.Name := M.ValueUnicode;
            M := N.NodeByName('UseCount');
            if Assigned(M) then
              Profile.AudioEncoderConfiguration.UseCount := M.ValueUnicode.ToInteger;

            M := N.NodeByName('Encoding');
            if Assigned(M) then
              Profile.AudioEncoderConfiguration.Encoding := M.ValueUnicode;
            M := N.NodeByName('Bitrate');
            if Assigned(M) then
              Profile.AudioEncoderConfiguration.Bitrate := M.ValueUnicode.ToInteger;
            M := N.NodeByName('SampleRate');
            if Assigned(M) then
              Profile.AudioEncoderConfiguration.SampleRate := M.ValueUnicode.ToInteger;
            M := N.NodeByName('Multicast');
            if Assigned(M) then
            begin
              K := M.NodeByName('Address');
              Profile.AudioEncoderConfiguration.Multicast.Address.Type_ := K.NodeByName('Type').ValueUnicode;
              Profile.AudioEncoderConfiguration.Multicast.Address.IPv4Address := K.NodeByName('IPv4Address').ValueUnicode;
              K := M.NodeByName('Port');
              if Assigned(K) then
                Profile.AudioEncoderConfiguration.Multicast.Port := K.ValueUnicode.ToInteger;
              K := M.NodeByName('TTL');
              if Assigned(K) then
                Profile.AudioEncoderConfiguration.Multicast.TTL := K.ValueUnicode.ToInteger;
              K := M.NodeByName('AutoStart');
              if Assigned(K) then
                Profile.AudioEncoderConfiguration.Multicast.AutoStart := K.ValueUnicode.ToBoolean;
            end;
            M := N.NodeByName('Multicast');
            if Assigned(M) then
              Profile.AudioEncoderConfiguration.SessionTimeout := M.ValueUnicode;
          end;

          N := Node.NodeByName('PTZConfiguration');
          if Assigned(N) then
          begin
            Profile.PTZConfiguration.token := string(N.AttributeValueByName['token']);
            M := N.NodeByName('Name');
            if Assigned(M) then
              Profile.PTZConfiguration.Name := M.ValueUnicode;
            M := N.NodeByName('UseCount');
            if Assigned(M) then
              Profile.PTZConfiguration.UseCount := M.ValueUnicode.ToInteger;
            M := N.NodeByName('NodeToken');
            if Assigned(M) then
              Profile.PTZConfiguration.NodeToken := M.ValueUnicode;
            M := N.NodeByName('DefaultContinuousPanTiltVelocitySpace');
            if Assigned(M) then
              Profile.PTZConfiguration.DefaultContinuousPanTiltVelocitySpace := M.ValueUnicode;
            M := N.NodeByName('DefaultContinuousZoomVelocitySpace');
            if Assigned(M) then
              Profile.PTZConfiguration.DefaultContinuousZoomVelocitySpace := M.ValueUnicode;
            M := N.NodeByName('DefaultPTZTimeout');
            if Assigned(M) then
              Profile.PTZConfiguration.DefaultPTZTimeout := M.ValueUnicode;
          end;

          N := Node.NodeByName('Extension');
          if Assigned(N) then
          begin
            K := N.NodeByName('AudioOutputConfiguration');
            if Assigned(K) then
            begin
              Profile.Extension.AudioOutputConfiguration.token := string(K.AttributeValueByName['token']);
              M := K.NodeByName('Name');
              if Assigned(M) then
                Profile.Extension.AudioOutputConfiguration.Name := M.ValueUnicode;
              M := K.NodeByName('UseCount');
              if Assigned(M) then
                Profile.Extension.AudioOutputConfiguration.UseCount := M.ValueUnicode.ToInteger;
              M := K.NodeByName('OutputToken');
              if Assigned(M) then
                Profile.Extension.AudioOutputConfiguration.OutputToken := M.ValueUnicode;
              M := K.NodeByName('SendPrimacy');
              if Assigned(M) then
                Profile.Extension.AudioOutputConfiguration.SendPrimacy := M.ValueUnicode;
              M := K.NodeByName('OutputLevel');
              if Assigned(M) then
                Profile.Extension.AudioOutputConfiguration.OutputLevel := M.ValueUnicode.ToInteger;
            end;
            K := N.NodeByName('AudioDecoderConfiguration');
            if Assigned(K) then
            begin
              Profile.Extension.AudioDecoderConfiguration.token := string(K.AttributeValueByName['token']);
              M := K.NodeByName('Name');
              if Assigned(M) then
                Profile.Extension.AudioDecoderConfiguration.Name := M.ValueUnicode;
              M := K.NodeByName('UseCount');
              if Assigned(M) then
                Profile.Extension.AudioDecoderConfiguration.UseCount := M.ValueUnicode.ToInteger;
            end;
          end;

          SetLength(Profiles, Length(Profiles) + 1);
          Profiles[High(Profiles)] := Profile;
          Result := True;
        end;
      end;
    end;
  finally
    Xml.Free;
    SS.Free;
  end;
end;

function PrepareGetProfilesRequest(const UserName, Password: AnsiString): AnsiString;
const
  GetProfilesFmt: AnsiString = // PasswordDigest,Nonce,Created // http://<host>/onvif/Media
    '<?xml version="1.0"?> ' + //
    '<soap:Envelope ' + //
    'xmlns:soap="http://www.w3.org/2003/05/soap-envelope" ' + //
    'xmlns:wsdl="http://www.onvif.org/ver10/media/wsdl">' + //
    '<soap:Header>' + //
    '<Security xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" s:mustUnderstand="1"> ' + //
    '<UsernameToken> ' + //
    '<Username>admin</Username> ' + //
    '<Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">%s</Password> ' +
    '<Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">%s</Nonce> ' +
    '<Created xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">%s</Created> ' + //
    '</UsernameToken> ' + //
    '</Security> ' + //
    '</soap:Header>' + //
    '<soap:Body xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> ' + //
    '<GetProfiles xmlns="http://www.onvif.org/ver10/media/wsdl" /> ' + //
    '</soap:Body> ' + //
    '</soap:Envelope>';

Var
  PasswordDigest, Nonce, Created: AnsiString;
begin
  GetONVIFPasswordDigest(UserName, Password, PasswordDigest, Nonce, Created);
  Result := Format(GetProfilesFmt, [PasswordDigest, Nonce, Created]);
end;

function XMLStreamUriToStreamUri(const XMLStreamUri: AnsiString; Var StreamUri: TStreamUri): Boolean;
var
  SS: TStringStream;
  XmlNode, Node: TXmlNode;
  Xml: TNativeXML;
begin
  Xml := TNativeXML.Create;
  SS := TStringStream.Create(XMLStreamUri);
  StreamUri := default (TStreamUri);
  Result := False;
  try
    Xml.LoadFromStream(SS);
    XmlNode := Xml.Root.NodeByName('Body');
    if Assigned(XmlNode) then
    begin
      XmlNode := XmlNode.NodeByName('GetStreamUriResponse');
      if Assigned(XmlNode) then
      begin
        XmlNode := XmlNode.NodeByName('MediaUri');
        if Assigned(XmlNode) then
        begin
          Node := XmlNode.NodeByName('Uri');
          if Assigned(Node) then
          begin
            StreamUri.Uri := String(Node.Value);
            Result := True;
          end;

          Node := XmlNode.NodeByName('InvalidAfterConnect');
          if Assigned(Node) then
          begin
            StreamUri.InvalidAfterConnect := string(Node.Value).ToBoolean;
            Result := True;
          end;

          Node := XmlNode.NodeByName('InvalidAfterReboot');
          if Assigned(Node) then
          begin
            StreamUri.InvalidAfterReboot := string(Node.Value).ToBoolean;
            Result := True;
          end;

          Node := XmlNode.NodeByName('Timeout');
          if Assigned(Node) then
          begin
            StreamUri.Timeout := Node.ValueUnicode;
            Result := True;
          end;

        end;
      end;
    end;
  finally
    Xml.Free;
    SS.Free;
  end;
end;

function ONVIFGetStreamUri(const Addr, UserName, Password, Stream, Protocol, ProfileToken: AnsiString): AnsiString;
begin
  // Addr -> http://<host>/onvif/Media
  ONVIFRequest(Addr, PrepareGetStreamUriRequest(UserName, Password, Stream, Protocol, ProfileToken), Result);
end;

function PrepareGetStreamUriRequest(const UserName, Password, Stream, Protocol, ProfileToken: AnsiString): AnsiString;
const
  GetStreamUriFmt: AnsiString = // PasswordDigest,Nonce,Created,Stream,Protocol,ProfileToken // http://<host>/onvif/Media
    '<?xml version="1.0"?> ' + //
    '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" ' + //
    'xmlns:wsdl="http://www.onvif.org/ver10/media/wsdl" xmlns:sch="http://www.onvif.org/ver10/schema"> ' + //
    '<soap:Header>' + //
    '<Security xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" s:mustUnderstand="1"> ' + //
    '<UsernameToken> ' + //
    '<Username>admin</Username> ' + //
    '<Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">%s</Password> ' +
    '<Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">%s</Nonce> ' +
    '<Created xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">%s</Created> ' + //
    '</UsernameToken> ' + //
    '</Security> ' + //
    '</soap:Header>' + //
    '<soap:Body> ' + //
    '<wsdl:GetStreamUri> ' + //
    '<wsdl:StreamSetup> ' + //
    '<sch:Stream>%s</sch:Stream> ' + //
    '<sch:Transport> ' + //
    '<sch:Protocol>%s</sch:Protocol> ' + //
    '<!--Optional:--> ' + //
    '<sch:Tunnel/> ' + //
    '</sch:Transport> ' + //
    '<!--You may enter ANY elements at this point--> ' + //
    '</wsdl:StreamSetup> ' + //
    '<wsdl:ProfileToken>%s</wsdl:ProfileToken> ' + //
    '</wsdl:GetStreamUri> ' + //
    '</soap:Body> ' + //
    '</soap:Envelope>';

Var
  PasswordDigest, Nonce, Created: AnsiString;
begin
  GetONVIFPasswordDigest(UserName, Password, PasswordDigest, Nonce, Created);
  Result := Format(GetStreamUriFmt, [PasswordDigest, Nonce, Created, Stream, Protocol, ProfileToken]);
end;

function SHA1(const Data: TBytes): TBytes;
Var
  IdHashSHA1: TIdHashSHA1;
  i, j: TIdBytes;
begin
  IdHashSHA1 := TIdHashSHA1.Create;
  try
    SetLength(i, Length(Data));
    Move(Data[0], i[0], Length(Data));
    j := IdHashSHA1.HashBytes(i);
    SetLength(Result, Length(j));
    Move(j[0], Result[0], Length(j));
  finally
    IdHashSHA1.Free;
  end;
end;

procedure GetONVIFPasswordDigest(const UserName, Password: AnsiString; Var PasswordDigest, Nonce, Created: AnsiString);
Var
  i: Integer;
  raw_nonce, bnonce, digest: TBytes;
  raw_digest: TBytes;
  CreatedByte, PasswordByte: TBytes;
begin
  SetLength(raw_nonce, 20);
  for i := 0 to High(raw_nonce) do
    raw_nonce[i] := Random(256);
  bnonce := TNetEncoding.Base64.Encode(raw_nonce);
  Nonce := BytesToAnsiString(bnonce);
  Created := GetONVIFDateTime(Now);
  SetLength(CreatedByte, Length(Created));
  Move(Created[1], CreatedByte[0], Length(Created));
  SetLength(PasswordByte, Length(Password));
  Move(Password[1], PasswordByte[0], Length(Password));
  raw_digest := SHA1(raw_nonce + CreatedByte + PasswordByte);
  digest := TNetEncoding.Base64.Encode(raw_digest);
  PasswordDigest := BytesToAnsiString(digest);
end;

function GetONVIFDateTime(const DateTime: TDateTime): AnsiString;
Var
  formattedDate, formattedTime: string;
begin
  DateTimeToString(formattedDate, 'yyyy-mm-dd', DateTime);
  DateTimeToString(formattedTime, 'hh:nn:ss.zzz', DateTime);
  Result := formattedDate + 'T' + formattedTime + 'Z';
end;

function UniqueProbeMatch(const ProbeMatch: TProbeMatchArray): TProbeMatchArray;
Var
  ProbeMatchDic: TDictionary<string, TProbeMatch>;
  PM: TProbeMatch;
  ñomparer: IComparer<TProbeMatch>;
begin
  ProbeMatchDic := TDictionary<string, TProbeMatch>.Create;
  try
    for PM in ProbeMatch do
      if not ProbeMatchDic.ContainsKey(PM.XAddrs) then
        ProbeMatchDic.Add(PM.XAddrs, PM);
    Result := ProbeMatchDic.Values.ToArray;
    ñomparer := TDelegatedComparer<TProbeMatch>.Create(
      function(const Left, Right: TProbeMatch): Integer
      begin
        Result := AnsiCompareText(Left.XAddrs, Right.XAddrs);
      end);
    TArray.Sort<TProbeMatch>(Result, ñomparer);
  finally
    ProbeMatchDic.Free;
  end;
end;

function XMLToProbeMatch(const ProbeMatchXML: string; Var ProbeMatch: TProbeMatch): Boolean;
var
  SS: TStringStream;
  XmlNode, Node: TXmlNode;
  S: string;
  Xml: TNativeXML;
  i: Integer;
begin
  Xml := TNativeXML.Create;
  SS := TStringStream.Create(ProbeMatchXML);
  ProbeMatch := default (TProbeMatch);
  Result := False;
  try
    Xml.LoadFromStream(SS);
    XmlNode := Xml.Root.NodeByName('Body');
    if Assigned(XmlNode) then
    begin
      XmlNode := XmlNode.NodeByName('ProbeMatches');
      if Assigned(XmlNode) then
      begin
        XmlNode := XmlNode.NodeByName('ProbeMatch');
        if Assigned(XmlNode) then
        begin
          Node := XmlNode.NodeByName('Types');
          if Assigned(Node) then
          begin
            S := String(Node.Value);
            if Pos('NetworkVideoTransmitter', S) > 0 then
              ProbeMatch.Types := ProbeMatch.Types + [ptNetworkVideoTransmitter];
            if Pos('Device', S) > 0 then
              ProbeMatch.Types := ProbeMatch.Types + [ptDevice];
            if Pos('NetworkVideoDisplay', S) > 0 then
              ProbeMatch.Types := ProbeMatch.Types + [ptNetworkVideoDisplay];
            Result := True;
          end;

          Node := XmlNode.NodeByName('Scopes');
          if Assigned(Node) then
          begin
            S := Trim(string(Node.Value));
            While Length(S) > 0 do
            begin
              SetLength(ProbeMatch.Scopes, Length(ProbeMatch.Scopes) + 1);
              i := Pos(' ', S);
              if i > 0 then
              begin
                ProbeMatch.Scopes[High(ProbeMatch.Scopes)] := Copy(S, 1, i - 1);
                Delete(S, 1, i);
              end
              else
              begin
                ProbeMatch.Scopes[High(ProbeMatch.Scopes)] := S;
                Break;
              end;
            end;

            Result := True;
          end;

          Node := XmlNode.NodeByName('XAddrs');
          if Assigned(Node) then
          begin
            S := string(Node.Value);
            if Pos(' ', S) <> 0 then
            begin
              ProbeMatch.XAddrs := Copy(S, 1, Pos(' ', S) - 1);
              ProbeMatch.XAddrsV6 := Copy(S, Pos(' ', S) + 1, Length(S));
            end
            else
              ProbeMatch.XAddrs := S;
            Result := True;
          end;

          Node := XmlNode.NodeByName('MetadataVersion');
          if Assigned(Node) then
          begin
            ProbeMatch.MetadataVersion := Node.ValueAsInteger;
            Result := True;
          end;

        end;
      end;
    end;
  finally
    Xml.Free;
    SS.Free;
  end;
end;

{ TONVIFProbeThread }

constructor TONVIFProbeThread.Create(const ProbeMathNotify: TProbeMathNotify; const ProbeMathXMLNotify: TProbeMathXMLNotify;
const ProbeTypeSet: TProbeTypeSet; const Timeout: Cardinal);
begin
  inherited Create(True);
  FProbeTypeSet := ProbeTypeSet;
  FTimeout := Timeout;
  FProbeMathNotify := ProbeMathNotify;
  FProbeMathXMLNotify := ProbeMathXMLNotify;
  Resume;
end;

procedure TONVIFProbeThread.Execute;

const
  NetworkVideoTransmitter: AnsiString = //
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://schemas.xmlsoap.org/ws/2004/08/addressing"><s:Header><a:Action s:mustUnderstand="1">http://schemas.xmlsoap.org/ws/2005/04/discovery/Probe</a:Action>'
    + '<a:MessageID>uuid:683b9488-db0b-44d6-9d40-a735d8483f8a</a:MessageID><a:ReplyTo><a:Address>http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous'
    + '</a:Address></a:ReplyTo><a:To s:mustUnderstand="1">urn:schemas-xmlsoap-org:ws:2005:04:discovery</a:To></s:Header><s:Body><Probe xmlns="http://schemas.xmlsoap.org/ws/2005/04/discovery">'
    + '<d:Types xmlns:d="http://schemas.xmlsoap.org/ws/2005/04/discovery" xmlns:dp0="http://www.onvif.org/ver10/network/wsdl">dp0:NetworkVideoTransmitter</d:Types></Probe></s:Body></s:Envelope>';

  Device: AnsiString = //
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://schemas.xmlsoap.org/ws/2004/08/addressing"><s:Header><a:Action s:mustUnderstand="1">http://schemas.xmlsoap.org/ws/2005/04/discovery/Probe</a:Action>'
    + '<a:MessageID>uuid:ad3ceb1c-17a4-424c-ab82-1e227f808cf8</a:MessageID><a:ReplyTo><a:Address>http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous'
    + '</a:Address></a:ReplyTo><a:To s:mustUnderstand="1">urn:schemas-xmlsoap-org:ws:2005:04:discovery</a:To></s:Header><s:Body><Probe xmlns="http://schemas.xmlsoap.org/ws/2005/04/discovery">'
    + '<d:Types xmlns:d="http://schemas.xmlsoap.org/ws/2005/04/discovery" xmlns:dp0="http://www.onvif.org/ver10/device/wsdl">dp0:Device</d:Types></Probe></s:Body></s:Envelope>';

  NetworkVideoDisplay: AnsiString = //
    '<s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://schemas.xmlsoap.org/ws/2004/08/addressing"><s:Header><a:Action s:mustUnderstand="1">http://schemas.xmlsoap.org/ws/2005/04/discovery/Probe</a:Action>'
    + '<a:MessageID>uuid:37c8b349-37d7-4d0e-be66-134af54b65cb</a:MessageID><a:ReplyTo><a:Address>http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous'
    + '</a:Address></a:ReplyTo><a:To s:mustUnderstand="1">urn:schemas-xmlsoap-org:ws:2005:04:discovery</a:To></s:Header><s:Body><Probe xmlns="http://schemas.xmlsoap.org/ws/2005/04/discovery">'
    + '<d:Types xmlns:d="http://schemas.xmlsoap.org/ws/2005/04/discovery" xmlns:dp0="http://www.onvif.org/ver10/network/wsdl">dp0:NetworkVideoDisplay</d:Types></Probe></s:Body></s:Envelope>';

Var
  UDPServer: TIdUDPServer;
  SndBytes: TIdBytes;
  S: AnsiString;
begin
  if FProbeTypeSet = [] then
    Exit;
  UDPServer := TIdUDPServer.Create(nil);
  E := TEvent.Create;
  try
    UDPServer.BroadcastEnabled := True;
    E.ResetEvent;
    UDPServer.OnUDPRead := UDPServerUDPRead;
    with UDPServer.Bindings.Add do
    begin
      IP := '0.0.0.0';
      Port := 0;
    end;
    UDPServer.Active := True;
    if UDPServer.Active then
    begin
      if ptNetworkVideoTransmitter in FProbeTypeSet then
      begin
        S := NetworkVideoTransmitter;
        SetLength(SndBytes, Length(S));
        Move(S[1], SndBytes[0], Length(S));
        UDPServer.SendBuffer('239.255.255.250', 3702, SndBytes);
      end;
      if ptDevice in FProbeTypeSet then
      begin
        S := Device;
        SetLength(SndBytes, Length(S));
        Move(S[1], SndBytes[0], Length(S));
        UDPServer.SendBuffer('239.255.255.250', 3702, SndBytes);
      end;
      if ptNetworkVideoDisplay in FProbeTypeSet then
      begin
        S := NetworkVideoDisplay;
        SetLength(SndBytes, Length(S));
        Move(S[1], SndBytes[0], Length(S));
        UDPServer.SendBuffer('239.255.255.250', 3702, SndBytes);
      end;

      While (E.WaitFor(FTimeout) = wrSignaled) and (not Terminated) do
        E.ResetEvent;
    end;
  finally
    UDPServer.Active := False;
    UDPServer.Free;
    E.Free;
  end;
end;

procedure TONVIFProbeThread.UDPServerUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
Var
  ProbeMatch: TProbeMatch;
  ProbeMatchStr: string;
begin
  E.SetEvent;

  ProbeMatchStr := BytesToString(AData);

  SetLength(FProbeMatchXML, Length(FProbeMatchXML) + 1);
  FProbeMatchXML[High(FProbeMatchXML)] := ProbeMatchStr;
  if Assigned(FProbeMathXMLNotify) then
    FProbeMathXMLNotify(FProbeMatchXML[High(FProbeMatchXML)]);

  if XMLToProbeMatch(ProbeMatchStr, ProbeMatch) then
  begin
    SetLength(FProbeMatch, Length(FProbeMatch) + 1);
    FProbeMatch[High(FProbeMatch)] := ProbeMatch;
    if Assigned(FProbeMathNotify) then
      FProbeMathNotify(FProbeMatch[High(FProbeMatch)]);
  end;
end;

{ TONVIFProbe }

constructor TONVIFProbe.Create(AOwner: TComponent);
begin
  inherited;
  FTimeout := 1000;
  FProbeType := [ptNetworkVideoTransmitter, ptDevice, ptNetworkVideoDisplay];
end;

destructor TONVIFProbe.Destroy;
begin
  if Assigned(FONVIFProbeThread) then
    FreeAndNil(FONVIFProbeThread);
  inherited;
end;

function TONVIFProbe.Execute: Boolean;
begin
  if Assigned(FONVIFProbeThread) then
    FreeAndNil(FONVIFProbeThread);
  FONVIFProbeThread := TONVIFProbeThread.Create(nil, nil, ProbeType, Timeout);
  FONVIFProbeThread.WaitFor;
  Result := Length(FONVIFProbeThread.ProbeMatchXML) > 0;
end;

function TONVIFProbe.ExecuteAsync: Boolean;
begin
  if Assigned(FONVIFProbeThread) then
    FreeAndNil(FONVIFProbeThread);
  FONVIFProbeThread := TONVIFProbeThread.Create(OnProbeMath, OnProbeMathXML, ProbeType, Timeout);
  FONVIFProbeThread.OnTerminate := OnCompleted;
  Result := True;
end;

function TONVIFProbe.GetCount: Integer;
begin
  if Assigned(FONVIFProbeThread) then
    Result := Length(FONVIFProbeThread.ProbeMatch)
  else
    Result := 0;
end;

function TONVIFProbe.GetProbeMatch(const Index: Integer): TProbeMatch;
begin
  if Assigned(FONVIFProbeThread) then
    Result := FONVIFProbeThread.ProbeMatch[Index]
  else
    Result := default (TProbeMatch)
end;

function TONVIFProbe.GetProbeMatchArray: TProbeMatchArray;
begin
  if Assigned(FONVIFProbeThread) then
    Result := FONVIFProbeThread.ProbeMatch
  else
    Result := default (TProbeMatchArray);
end;

function TONVIFProbe.GetProbeMatchXML(const Index: Integer): String;
begin
  if Assigned(FONVIFProbeThread) then
    Result := FONVIFProbeThread.ProbeMatchXML[Index]
  else
    Result := default (String);
end;

end.
