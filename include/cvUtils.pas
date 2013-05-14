(* /  **************************************************************************************************
  //                                 Project Delphi-OpenCV
  //  **************************************************************************************************
  //  Contributor:
  //  laentir Valetov
  //  email:laex@bk.ru
  //  **************************************************************************************************
  //  You may retrieve the latest version of this file at the GitHub,
  //  located at git://github.com/Laex/Delphi-OpenCV.git
  //  **************************************************************************************************
  //  License:
  //  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  //  you may not use this file except in compliance with the License. You may obtain a copy of the
  //  License at http://www.mozilla.org/MPL/
  //
  //  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  //  ANY KIND, either express or implied. See the License for the specific language governing rights
  //  and limitations under the License.
  //
  //  Alternatively, the contents of this file may be used under the terms of the
  //  GNU Lesser General Public License (the  "LGPL License"), in which case the
  //  provisions of the LGPL License are applicable instead of those above.
  //  If you wish to allow use of your version of this file only under the terms
  //  of the LGPL License and not to allow others to use your version of this file
  //  under the MPL, indicate your decision by deleting  the provisions above and
  //  replace  them with the notice and other provisions required by the LGPL
  //  License.  If you do not delete the provisions above, a recipient may use
  //  your version of this file under either the MPL or the LGPL License.
  //
  //  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  //  **************************************************************************************************
  //  Warning: Using Delphi XE3 syntax!
  //  **************************************************************************************************
  //  The Initial Developer of the Original Code:
  //  OpenCV: open source computer vision library
  //  Homepage:    http://opencv.org
  //  Online docs: http://docs.opencv.org
  //  Q&A forum:   http://answers.opencv.org
  //  Dev zone:    http://code.opencv.org
  //  ************************************************************************************************** *)

unit cvUtils;

interface

Uses core.types_c, Vcl.Graphics;

Function hsv2rgb(hue: single): TCvScalar;
procedure IplImage2Bitmap(iplImg: PIplImage; var bitmap: Vcl.Graphics.TBitmap);
function cvImage2Bitmap(img: PIplImage): Vcl.Graphics.TBitmap;

Type
  TAnsiString = record helper for AnsiString
  public
    function AsPAnsiChar: pAnsiChar;
  end;

implementation

Uses WinApi.Windows, System.SysUtils;

Function hsv2rgb(hue: single): TCvScalar;
var
  rgb: array [0 .. 2] of integer;
  p, sector: integer;
const
  sector_data: array [0 .. 5, 0 .. 2] of integer = ((0, 2, 1), (1, 2, 0), (1, 0, 2), (2, 0, 1), (2, 1, 0), (0, 1, 2));
Begin
  hue := hue * 0.033333333333333333333333333333333;
  sector := cvFloor(hue);
  p := cvRound(255 * (hue - sector));
  if (sector and 1) = 1 then
    p := 255
  else
    p := 0;

  rgb[sector_data[sector][0]] := 255;
  rgb[sector_data[sector][1]] := 0;
  rgb[sector_data[sector][2]] := p;

  result := cvScalar(rgb[2], rgb[1], rgb[0], 0);
End;

{ -----------------------------------------------------------------------------
  Procedure:  IplImage2Bitmap
  Author:     De Sanctis
  Date:       23-set-2005
  Arguments:  iplImg: PIplImage; bitmap: TBitmap
  Description: convert a IplImage to a Windows bitmap
  ----------------------------------------------------------------------------- }
procedure IplImage2Bitmap(iplImg: PIplImage; var bitmap: Vcl.Graphics.TBitmap);
VAR
  i, j: integer;
  offset: longint;
  dataByte, RowIn: PByteArray;
  channelsCount: integer;
BEGIN
  TRY
    // assert((iplImg.Depth = 8) and (iplImg.NChannels = 3),
    // 'IplImage2Bitmap: Not a 24 bit color iplImage!');
    bitmap.Height := iplImg.Height;
    bitmap.Width := iplImg.Width;
    FOR j := 0 TO bitmap.Height - 1 DO
    BEGIN
      // origin BL = Bottom-Left
      if (iplImg.Origin = IPL_ORIGIN_BL) then
        RowIn := bitmap.Scanline[bitmap.Height - 1 - j]
      else
        RowIn := bitmap.Scanline[j];

      offset := longint(iplImg.ImageData) + iplImg.WidthStep * j;
      dataByte := PByteArray(offset);

      if (iplImg.ChannelSeq = 'BGR') then
      begin
        { direct copy of the iplImage row bytes to bitmap row }
        CopyMemory(RowIn, dataByte, iplImg.WidthStep);
      End
      else if (iplImg.ChannelSeq = 'GRAY') then
        FOR i := 0 TO bitmap.Width - 1 DO
        begin
          RowIn[3 * i] := dataByte[i];
          RowIn[3 * i + 1] := dataByte[i];
          RowIn[3 * i + 2] := dataByte[i];
        End
      else
        FOR i := 0 TO 3 * bitmap.Width - 1 DO
        begin
          RowIn[i] := dataByte[i + 2];
          RowIn[i + 1] := dataByte[i + 1];
          RowIn[i + 2] := dataByte[i];
        End;
    End;
  Except
  End
END; { IplImage2Bitmap }

function cvImage2Bitmap(img: PIplImage): Vcl.Graphics.TBitmap;
var
  info: string;
  bmp: Vcl.Graphics.TBitmap;
  deep: integer;
  i, j, K, wStep, Channels: integer;
  data: PByteArray;
  pb: PByteArray;
begin
  result := NIL;
  if (img <> NIL) then
  begin
    bmp := Vcl.Graphics.TBitmap.Create;
    bmp.Width := img^.Width;
    bmp.Height := img^.Height;
    deep := img^.nChannels * img^.depth;
    case deep of
      8:
        bmp.PixelFormat := pf8bit;
      16:
        bmp.PixelFormat := pf16bit;
      24:
        bmp.PixelFormat := pf24bit;
      32:
        bmp.PixelFormat := pf32bit;
    End;
    wStep := img^.WidthStep;
    Channels := img^.nChannels;
    data := Pointer(img^.ImageData);
    for i := 0 to img^.Height - 1 do
    begin
      pb := bmp.Scanline[i];
      for j := 0 to img^.Width - 1 do
      begin
        for K := 0 to Channels - 1 do
          pb[3 * j + K] := data[i * wStep + j * Channels + K]
      End;
    End;
    result := bmp;
    // bmp.Free;
  End;
end;

{ TAnsiString }

function TAnsiString.AsPAnsiChar: pAnsiChar;
begin
  result := pAnsiChar(@Self[1]);
end;

end.
