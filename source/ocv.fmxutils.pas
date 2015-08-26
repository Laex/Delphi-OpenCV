unit ocv.fmxutils;

interface

Uses
  ocv.core.types_c,
  FMX.Graphics;

procedure IPLImageToFMXBitmap(const IpImage: pIplImage; const FMXBitmap: TBitmap);

implementation

Uses FMX.Types;

procedure IPLImageToFMXBitmap(const IpImage: pIplImage; const FMXBitmap: TBitmap);
Var
  M: TBitmapData;
  i: Integer;
  SrcData, DestData: pByte;
  nC: Integer;
begin
  Assert(Assigned(IpImage) and Assigned(FMXBitmap));
  FMXBitmap.Canvas.BeginScene;
  if (FMXBitmap.Width <> IpImage^.Width) or (FMXBitmap.Height <> IpImage^.Height) then
    FMXBitmap.SetSize(IpImage^.Width, IpImage^.Height);
  if FMXBitmap.Map(TMapAccess.Write, M) then
    try
      SrcData := pByte(IpImage^.imageData);
      DestData := pByte(M.Data);
      nC := IpImage^.nChannels;
      for i := 0 to M.Width * M.Height - 1 do
      begin
        DestData[i * PixelFormatBytes[FMXBitmap.PixelFormat] + 0] := SrcData[i * nC + 0];
        DestData[i * PixelFormatBytes[FMXBitmap.PixelFormat] + 1] := SrcData[i * nC + 1];
        DestData[i * PixelFormatBytes[FMXBitmap.PixelFormat] + 2] := SrcData[i * nC + 2];
        DestData[i * PixelFormatBytes[FMXBitmap.PixelFormat] + 3] := $FF;
      end;
    finally
      FMXBitmap.Unmap(M);
    end;
  FMXBitmap.Canvas.EndScene;
end;

end.
