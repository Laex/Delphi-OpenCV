(* /  **************************************************************************************************
  //                                 Project Delphi-OpenCV
  //  **************************************************************************************************
  //  Contributor:
  //  Laentir Valetov
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
  //  Warning: Using Delphi XE2 syntax!
  //  **************************************************************************************************
  //  The Initial Developer of the Original Code:
  //  OpenCV: open source computer vision library
  //  Homepage:    http://opencv.org
  //  Online docs: http://docs.opencv.org
  //  Q&A forum:   http://answers.opencv.org
  //  Dev zone:    http://code.opencv.org
  //  ***************************************************************************************************)

{$POINTERMATH ON}
unit cvUtils;

interface

Uses
  WinApi.Windows,
  core.types_c,
  Vcl.Graphics;

Function hsv2rgb(hue: single): TCvScalar;
procedure IplImage2Bitmap(iplImg: PIplImage; var bitmap: Vcl.Graphics.TBitmap);

function cvImage2Bitmap(img: PIplImage): Vcl.Graphics.TBitmap;

function ipDraw(dc: HDC; img: PIplImage; const rect: TRect; const Stretch: Boolean = true): Boolean; overload;
procedure ipDraw(const x, y: Integer; const _Grab: PIplImage; const Wnd: THandle); overload;
function CreateRGBBitmap(_Grab: PIplImage): HBITMAP;

function c_str(const Text: String): pCVChar;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: pCvArr): pCvArr; overload;
function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: string): string; overload;
function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: TCvScalar): TCvScalar; overload;

function BitmapToIplImage(const bitmap: Vcl.Graphics.TBitmap): PIplImage;
function CropIplImage(const src: PIplImage; const roi: TCvRect): PIplImage;

implementation

Uses
  System.SysUtils,
  core_c;

function BitmapToIplImage(const bitmap: Vcl.Graphics.TBitmap): PIplImage;
Var
  bitmapData: PByte;
begin
  Assert(bitmap.PixelFormat = pf24bit); // Пока только такой формат

  bitmapData := bitmap.Scanline[0];
  Result := cvCreateImage(cvSize(bitmap.Width, bitmap.Height), IPL_DEPTH_8U, 3);
  CopyMemory(Result^.imageData, bitmapData, Result^.imageSize);
  Result^.imageDataOrigin := nil;
  Result^.imageId := nil;
  Result^.maskROI := nil;
  Result^.roi := nil;
end;

function CropIplImage(const src: PIplImage; const roi: TCvRect): PIplImage;
begin
  // Must have dimensions of output image
  Result := cvCreateImage(cvSize(roi.Width, roi.Height), src^.depth, src^.nChannels);
  // Say what the source region is
  cvSetImageROI(src, roi);
  // Do the copy
  cvCopyImage(src, Result);
  cvResetImageROI(src);
  Result := Result;
end;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: pCvArr): pCvArr; overload;
begin
  if Cond then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: string): string; overload;
begin
  if Cond then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: TCvScalar): TCvScalar; overload;
begin
  if Cond then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

// ---------------------------------------------------------------------------
// Создание API шного битмапа из интеловского RGB изображения
// ---------------------------------------------------------------------------
function CreateRGBBitmap(_Grab: PIplImage): HBITMAP;

  function WIDTHBYTES(bits: DWORD): DWORD; inline;
  begin
    Result := ((((bits) + 31) div 32) * 4);
  end;

Var
  lpbi: TBitmapInfo;
  App: PByte;
  pBits: Pointer;
  i, j: Integer;
begin
  lpbi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  lpbi.bmiHeader.biWidth := _Grab^.Width;
  lpbi.bmiHeader.biHeight := _Grab^.Height;
  lpbi.bmiHeader.biPlanes := 1;
  lpbi.bmiHeader.biBitCount := 24;
  lpbi.bmiHeader.biCompression := BI_RGB;
  lpbi.bmiHeader.biSizeImage := WIDTHBYTES(_Grab^.Width * 8) * _Grab^.Height;
  lpbi.bmiHeader.biXPelsPerMeter := 0;
  lpbi.bmiHeader.biYPelsPerMeter := 0;
  lpbi.bmiHeader.biClrUsed := 0;
  lpbi.bmiHeader.biClrImportant := 0;
  Result := CreateDIBSection(0, lpbi, DIB_RGB_COLORS, pBits, 0, 0);
  if Result <> 0 then
  begin
    App := pBits;

    if (_Grab^.nChannels = 1) then // Серое или бинарное
    begin

      for i := 0 to _Grab^.Height - 1 do
      begin
        for j := 0 to _Grab^.Width - 1 do
        begin
          App[_Grab^.Width * 3 * (_Grab^.Height - i - 1) + j * 3] := _Grab^.imageData[_Grab^.Width * (i) + j];
          App[_Grab^.Width * 3 * (_Grab^.Height - i - 1) + j * 3 + 1] := _Grab^.imageData[_Grab^.Width * (i) + j];
          App[_Grab^.Width * 3 * (_Grab^.Height - i - 1) + j * 3 + 2] := _Grab^.imageData[_Grab^.Width * (i) + j];
        end;
      end;

    end;

    if (_Grab^.nChannels = 3) then // Цветное
    begin
      for i := 0 to _Grab^.Height - 1 do
      begin
        CopyMemory(App + _Grab^.Width * 3 * (_Grab^.Height - i - 1), _Grab^.imageData + _Grab^.Width * 3 * i, _Grab^.Width * 3);
        // Копируем память
      end;

    end;
  end;
end;

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// Функция вывода изображения на HANDLE оконного компонента
// ---------------------------------------------------------------------------
procedure ipDraw(const x, y: Integer; const _Grab: PIplImage; const Wnd: THandle); overload;
Var
  hMemDC, dc: HDC;
  bitmap: HBITMAP;
begin
  dc := GetDC(Wnd);
  hMemDC := CreateCompatibleDC(dc);
  bitmap := CreateRGBBitmap(_Grab);
  SelectObject(hMemDC, bitmap);
  BitBlt(dc, x, y, _Grab^.Width, _Grab^.Height, hMemDC, 0, 0, SRCCOPY);
  DeleteObject(bitmap);
  DeleteDC(hMemDC);
  DeleteDC(dc);
end;

function c_str(const Text: String): pCVChar;
begin
  Result := pCVChar(@(AnsiString(Text)[1]));
end;

Function hsv2rgb(hue: single): TCvScalar;
var
  rgb: array [0 .. 2] of Integer;
  p, sector: Integer;
const
  sector_data: array [0 .. 5, 0 .. 2] of Integer = ((0, 2, 1), (1, 2, 0), (1, 0, 2), (2, 0, 1), (2, 1, 0), (0, 1, 2));
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

  Result := cvScalar(rgb[2], rgb[1], rgb[0], 0);
End;

{-----------------------------------------------------------------------------
  Procedure:  IplImage2Bitmap
  Author:     De Sanctis
  Date:       23-set-2005
  Arguments:  iplImg: PIplImage; bitmap: TBitmap
  Description: convert a IplImage to a Windows bitmap
  -----------------------------------------------------------------------------}
procedure IplImage2Bitmap(iplImg: PIplImage; var bitmap: Vcl.Graphics.TBitmap);
VAR
  i, j: Integer;
  offset: longint;
  dataByte, RowIn: PByteArray;
  // channelsCount: Integer;
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

      offset := longint(iplImg.imageData) + iplImg.WidthStep * j;
      dataByte := PByteArray(offset);

      if (iplImg.ChannelSeq = 'BGR') then
      begin
        {direct copy of the iplImage row bytes to bitmap row}
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
END; {IplImage2Bitmap}

function cvImage2Bitmap(img: PIplImage): Vcl.Graphics.TBitmap;
var
  // info: string;
  bmp: Vcl.Graphics.TBitmap;
  deep: Integer;
  i, j, K, wStep, Channels: Integer;
  data: PByteArray;
  pb: PByteArray;
begin
  Result := NIL;
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
    data := Pointer(img^.imageData);
    for i := 0 to img^.Height - 1 do
    begin
      pb := bmp.Scanline[i];
      for j := 0 to img^.Width - 1 do
      begin
        for K := 0 to Channels - 1 do
          pb[3 * j + K] := data[i * wStep + j * Channels + K]
      End;
    End;
    Result := bmp;
    // bmp.Free;
  End;
end;

function ipDraw(dc: HDC; img: PIplImage; const rect: TRect; const Stretch: Boolean = true): Boolean;

Type
  pCOLORREF = ^COLORREF;
  pBITMAPINFOHEADER = ^BITMAPINFOHEADER;

Var
  isrgb: Boolean;
  isgray: Boolean;
  buf: array [1 .. SizeOf(BITMAPINFOHEADER) + SizeOf(RGBQUAD) * 256] of byte;
  dibhdr: pBITMAPINFOHEADER;
  _dibhdr: TBitmapInfo ABSOLUTE buf;
  _rgb: pCOLORREF;
  i: Integer;
begin
  if (not Assigned(img)) or (not Assigned(img^.imageData)) then
    Exit(false);
  isrgb := ('R' = upcase(img^.colorModel[0])) and ('G' = upcase(img^.colorModel[1])) and ('B' = upcase(img^.colorModel[2]));
  isgray := 'G' = upcase(img^.colorModel[0]);
  if (not isgray) and (not isrgb) then
    Exit(false);
  if (1 = img^.nChannels) and (not isgray) then
    Exit(false);

  dibhdr := @buf;
  _rgb := pCOLORREF(Integer(dibhdr) + SizeOf(BITMAPINFOHEADER));

  if (isgray) then
    for i := 0 to 255 do
      _rgb[i] := rgb(i, i, i);
  dibhdr^.biSize := SizeOf(BITMAPINFOHEADER);
  dibhdr^.biWidth := img^.Width;
  // Check origin for display
  if img^.Origin = 0 then
    dibhdr^.biHeight := -img^.Height
  else
    dibhdr^.biHeight := img^.Height;

  dibhdr^.biPlanes := 1;
  dibhdr^.biBitCount := 8 * img^.nChannels;
  dibhdr^.biCompression := BI_RGB;
  dibhdr^.biSizeImage := 0; // img^.imageSize;
  dibhdr^.biXPelsPerMeter := 0;
  dibhdr^.biYPelsPerMeter := 0;
  dibhdr^.biClrUsed := 0;
  dibhdr^.biClrImportant := 0;

  if Stretch then
  begin
    SetStretchBltMode(dc, COLORONCOLOR);
    // Stretch the image to fit the rectangle
    Result := StretchDIBits(dc, rect.left, rect.top, rect.Width, rect.Height, 0, 0, img^.Width, img^.Height, img^.imageData,
      _dibhdr, DIB_RGB_COLORS, SRCCOPY) > 0;
  end
  else
    // Draw without scaling
    Result := SetDIBitsToDevice(dc, rect.left, rect.top, img^.Width, img^.Height, 0, 0, 0, img^.Height, img^.imageData, _dibhdr,
      DIB_RGB_COLORS) > 0;
end;

end.
