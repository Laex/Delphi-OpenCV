unit ocv.comp.Proc;

interface

Uses
  ocv.comp.Types,
  ocv.objdetect_c,
  ocv.core.types_c;

function ocvHaarCascadeTransform(
  { } const Source: IocvImage;
  { } const Cascade: pCvHaarClassifierCascade;
  { } var HaarRects: TocvRects;
  { } const MinSize, MaxSize: TcvSize;
  { } const Equalize: Boolean = True;
  { } const Scale: Double = 1.3;
  { } const MinNeighbors: Integer = 3;
  { } const Flag: TocvHaarCascadeFlagSet = []): Boolean;

function ocvLoadHaarCascade(const HaarCascadeType: TocvHaarCascadeType): pCvHaarClassifierCascade;

function IsRectEmpty(const Rect: TocvRect): Boolean;

implementation

Uses
{$IFDEF HAS_UNITSCOPE}
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  System.SysUtils,
  System.Classes,
  System.ZLib,
{$ELSE}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  SysUtils,
  Classes,
  ZLib,
{$ENDIF}
  ocv.core_c,
  ocv.imgproc_c,
  ocv.utils;

{$I Opencv.inc}

function ocvLoadHaarCascade(const HaarCascadeType: TocvHaarCascadeType): pCvHaarClassifierCascade;

  function TempPath: string;
  var
    BufSize: Cardinal;
  begin
    BufSize := GetTempPath(0, nil);
    SetLength(Result, BufSize);
    GetTempPath(BufSize, PChar(Result));
    Result := Trim(Result);
  end;

  function FileSize(const aFilename: String): Int64;
  var
    info: TWin32FileAttributeData;
  begin
    Result := -1;

    if NOT GetFileAttributesEx(PWideChar(aFilename), GetFileExInfoStandard, @info) then
      EXIT;

    Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
  end;

Var
  FullFileName: String;
  RS: TResourceStream;
  DC: TZDecompressionStream;
  FS: TFileStream;
begin
  Result := nil;
  FullFileName := TempPath + CascadeRecourse[HaarCascadeType].FileName;
  if (not FileExists(FullFileName)) or (FileSize(FullFileName) = 0) then
  begin
    RS := TResourceStream.Create(hInstance, CascadeRecourse[HaarCascadeType].Name, RT_RCDATA);
    DC := TZDecompressionStream.Create(RS);
    FS := TFileStream.Create(FullFileName, fmCreate);
    try
      FS.CopyFrom(DC, DC.Size);
    finally
      DC.Free;
      FS.Free;
      RS.Free;
    end;
  end;
  if FileExists(FullFileName) and (FileSize(FullFileName) > 0) then
    Result := cvLoad(c_str(FullFileName), nil, nil, nil);
end;

function ocvHaarCascadeTransform;
Var
  storage: pCvMemStorage;
  gray: IocvImage;
  detected_objects: pCvSeq;
  i: Integer;
  cvr: pCvRect;
  // r, g, b: byte;
begin
  SetLength(HaarRects, 0);
  Result := False;
  if Assigned(Cascade) then
  begin
    storage := cvCreateMemStorage(0);
    try
      gray := Source.GrayImage;
      if Equalize then
        cvEqualizeHist(gray.IpImage, gray.IpImage);
      detected_objects := cvHaarDetectObjects(gray.IpImage, Cascade, storage, Scale, MinNeighbors, HaarSetToFlag(Flag), MinSize, MaxSize);
      if Assigned(detected_objects) then
      begin
        SetLength(HaarRects, detected_objects^.total);
        i := 0;
        While i < detected_objects^.total do
        begin
          cvr := pCvRect(cvGetSeqElem(detected_objects, i));
          HaarRects[i] := ocvRect(cvr^.X, cvr^.Y, (cvr^.X) + (cvr^.Width), (cvr^.Y) + (cvr^.Height));
          Inc(i);
        end;
      end;
      Result := True;
    finally
      cvReleaseMemStorage(storage);
    end;
  end;
end;

function IsRectEmpty(const Rect: TocvRect): Boolean;
begin
  Result := (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top);
end;

end.
