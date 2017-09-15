unit ocv.fmxutils;

interface

Uses
  ocv.core.types_c
{$IFDEF DELPHIXE5_UP}
  , FMX.Graphics
{$IFEND}
  ;

{$IFDEF DELPHIXE5_UP}
procedure IPLImageToFMXBitmap(const IpImage: pIplImage; const FMXBitmap: TBitmap); inline;
{$IFEND}

implementation

Uses FMX.Types;

{$IFDEF DELPHIXE5_UP}
procedure IPLImageToFMXBitmap(const IpImage: pIplImage; const FMXBitmap: TBitmap); inline;
Var
  BitmapData: TBitmapData;
  i: Integer;
  SrcData, DestData: pByte;
  nC: Integer;
  pf: Integer;
begin
  SrcData := nil;
  Assert(Assigned(IpImage) and Assigned(FMXBitmap));
  if (IpImage^.Width > 0) and (IpImage^.Height > 0) and Assigned(IpImage^.imageData) then
    try
      nC := IpImage^.nChannels;
      With IpImage^ do
      begin
        SrcData := AllocMem(Width * Height * nC);
        Move(imageData^, SrcData^, Width * Height * nC);
      end;
      // FMXBitmap.Canvas.BeginScene;
      // try
      if (FMXBitmap.Width <> IpImage^.Width) or (FMXBitmap.Height <> IpImage^.Height) then
        FMXBitmap.SetSize(IpImage^.Width, IpImage^.Height);
      if FMXBitmap.Map(TMapAccess.Write, BitmapData) then
        try
          DestData := pByte(BitmapData.Data);
          pf := PixelFormatBytes[FMXBitmap.PixelFormat];
          for i := 0 to BitmapData.Width * BitmapData.Height - 1 do
          begin
            DestData[i * pf + 0] := SrcData[i * nC + 0];
            DestData[i * pf + 1] := SrcData[i * nC + 1];
            DestData[i * pf + 2] := SrcData[i * nC + 2];
            DestData[i * pf + 3] := $FF;
          end;
        finally
          FMXBitmap.Unmap(BitmapData);
        end;
      // finally
      // FMXBitmap.Canvas.EndScene;
      // end;
    finally
      if Assigned(SrcData) then
        FreeMem(SrcData);
    end;
end;
{$IFEND}

end.
