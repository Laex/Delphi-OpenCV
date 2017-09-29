(*
  ****************************************************************
  Delphi-OpenCV Demo
  Copyright (C) 2013 Project Delphi-OpenCV
  ****************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  ****************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  ****************************************************************
  The contents of this file are used with permission, subject to
  the Mozilla Public License Version 1.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1_1Final.html

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
  ****************************************************************
*)

unit ocv.fmxutils;

{$I OpenCV.inc}

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

{$IFDEF DELPHIXE5_UP}

Uses FMX.Types;

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
