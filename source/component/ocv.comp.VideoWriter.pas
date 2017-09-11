{$IFNDEF CLR}
{$I Opencv.inc}
unit ocv.comp.VideoWriter;
{$ENDIF}

interface

Uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  System.Classes,
  System.AnsiStrings,
{$ELSE}
  SysUtils,
  Classes,
{$ENDIF}
  ocv.highgui_c,
  ocv.core_c,
  ocv.core.types_c,
  ocv.imgproc_c,
  ocv.comp.Types;

Type

  TocvFrameSize = class(TPersistent)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  private
    FFrameSize: TcvSize;
  public
    constructor Create;
    property cvFrameSize: TcvSize read FFrameSize write FFrameSize;
  published
    property Width: Integer read FFrameSize.Width write FFrameSize.Width;
    property Height: Integer read FFrameSize.Height write FFrameSize.Height;
  end;

  TocvOnGetVideoFileName = procedure(Sender: TObject; Var AFileName: string) of object;
  TocvOnGetVideoParams = procedure(Sender: TObject; Var FrameWidth, FrameHeight: Integer; Var VideoFPS: Double; Var CodecFourCC: AnsiString)
    of object;

  TocvVideoWriter = class(TocvDataReceiver)
  private
    FFps: Double;
    FWriter: pCvVideoWriter;
    FFourCC: AnsiString;
    FFileName: TFileName;
    FEnabled: Boolean;
    FVideoAsSource: Boolean;
    FFrameSize: TocvFrameSize;
    FOnGetVideoFileName: TocvOnGetVideoFileName;
    FOnGetVideoParams: TocvOnGetVideoParams;
    procedure SetFourCC(const Value: AnsiString);
    procedure SetFileName(const Value: TFileName);
    procedure SetEnabled(const Value: Boolean);
    procedure CloseWriter;
    procedure OpenWriter;
    procedure DoGetVideoFileName;
    procedure DoGetVideoParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TakeImage(const IplImage: IocvImage); override;
  published
    property Enabled: Boolean Read FEnabled write SetEnabled default false;
    property FourCC: AnsiString read FFourCC write SetFourCC;
    property FileName: TFileName read FFileName write SetFileName;
    property VideoAsSource: Boolean read FVideoAsSource write FVideoAsSource default True;
    property FrameSize: TocvFrameSize read FFrameSize write FFrameSize;
    property OnGetVideoFileName: TocvOnGetVideoFileName read FOnGetVideoFileName write FOnGetVideoFileName;
    property OnGetVideoParams: TocvOnGetVideoParams read FOnGetVideoParams write FOnGetVideoParams;
  end;

implementation

{ TocvVideoWriter }

procedure TocvVideoWriter.CloseWriter;
begin
  if Assigned(FWriter) then
  begin
    cvReleaseVideoWriter(FWriter);
    FWriter := nil;
  end;
end;

constructor TocvVideoWriter.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := false;
  FourCC := 'XVID';
  FVideoAsSource := True;
  FFrameSize := TocvFrameSize.Create;
end;

destructor TocvVideoWriter.Destroy;
begin
  CloseWriter;
  FFrameSize.Free;
  inherited;
end;

procedure TocvVideoWriter.DoGetVideoFileName;
Var
  VFileName: String;
begin
  if Assigned(OnGetVideoFileName) then
  begin
    VFileName := FileName;
    OnGetVideoFileName(Self, VFileName);
    FileName := VFileName;
  end;
end;

procedure TocvVideoWriter.DoGetVideoParams;
var
  W, H: Integer;
begin
  if Assigned(OnGetVideoParams) then
  begin
    W := FrameSize.Width;
    H := FrameSize.Height;
    OnGetVideoParams(Self, W, H, FFps, FFourCC);
    FrameSize.Width := W;
    FrameSize.Height := H;
  end;
end;

procedure TocvVideoWriter.OpenWriter;
begin
  if Assigned(VideoSource) and (Length(Trim(FFourCC)) > 3) then
  begin
    CloseWriter;

    if VideoAsSource then
    begin
      FrameSize.cvFrameSize := CvSize(VideoSource.Width, VideoSource.Height);
      FFps := VideoSource.FPS;
    end;
    if (FrameSize.cvFrameSize.Width = 0) or (FrameSize.cvFrameSize.Height = 0) then
      FrameSize.cvFrameSize := CvSize(640, 480);

    if FFps = 0 then
      FFps := 15;

    DoGetVideoParams;
    DoGetVideoFileName;

    try
      if (Length(Trim(FFourCC)) > 3) and (Length(Trim(FileName)) > 0) then
        FWriter := cvCreateVideoWriter(PAnsiChar(AnsiString(FileName)), CV_FOURCC(FFourCC[1], FFourCC[2], FFourCC[3], FFourCC[4]), FFps,
          FrameSize.cvFrameSize)
      else
        FWriter := nil;

    except
      FWriter := nil;
    end;
  end;
end;

procedure TocvVideoWriter.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    CloseWriter;
    FEnabled := Value;
  end;
end;

procedure TocvVideoWriter.SetFileName(const Value: TFileName);
begin
  if not SameText(FFileName, Value) then
  begin
    CloseWriter;
    FFileName := Value;
  end;
end;

procedure TocvVideoWriter.SetFourCC(const Value: AnsiString);
begin
  if not AnsiSameText(FFourCC, Value) then
  begin
    CloseWriter;
    FFourCC := Value;
  end;
end;

procedure TocvVideoWriter.TakeImage(const IplImage: IocvImage);
begin
  if Enabled then
  begin
    if not Assigned(FWriter) then
      OpenWriter;
    if Assigned(FWriter) then
      cvWriteFrame(FWriter, IplImage.IpImage);
  end;
end;

{ TocvFrameSize }

procedure TocvFrameSize.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TocvFrameSize then
    FFrameSize := (Dest as TocvFrameSize).FFrameSize;
end;

constructor TocvFrameSize.Create;
begin
  FFrameSize.Width := 640;
  FFrameSize.Height := 480;
end;

end.
